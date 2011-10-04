#!/usr/bin/env python
#
# python-bufr-transform , BUFR variable translation 
# 
# Copyright (C) 2010  Kristian Rune Larsen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
# Author Kristian Rune Larsen <krl@dmi.dk>

""" converts any BUFR file to NetCDF, variable name mapping based on database
"""

__revision__ = 0.1

import os
import sys
import logging

import numpy as np
from netCDF4 import Dataset

import bufr
import bufr.metadb as bufrmetadb 
from bufr.transform import BUFR2NetCDFError, netcdf_datatype

# Log everything, and send it to stderr.
logging.basicConfig(level=logging.DEBUG)


def _create_global_attributes(rootgrp, instr):
    """ Creates global netcdf attributes and assigns values
    
        Params : 
            ncf : filehandle
            instr : SQLalchemy BufrDesc object

    """
    
    rootgrp.Conventions = "CF-1.0"
    rootgrp.title = instr.title.encode('latin-1')
    rootgrp.institution = instr.institution.encode('latin-1')
    rootgrp.source = instr.source.encode('latin-1')
    rootgrp.history = instr.history.encode('latin-1')
    rootgrp.references = instr.references.encode('latin-1')
    rootgrp.comments = instr.comments.encode('latin-1')


def _create_dimensions(rootgrp, vname_map):
    """ Create dimensions 

        Param : 
            rootgrp : netcdf filehandle
            vname_map : list of BUFR info objects

    """ 

    # Create basic dimensions in the NetCDF file

    rootgrp.createDimension('record', None)
    rootgrp.createDimension('scalar', 1)

    # Create dimensions from database
    dims = {}
    for key in vname_map.keys():
        var_info_dict = vname_map[key]
        if 'netcdf_name' in var_info_dict:
            dims[ var_info_dict['netcdf_dimension_name'] ] = \
                    int( var_info_dict['netcdf_dimension_length'])
    for dim_key, dim_size in dims.iteritems():
        rootgrp.createDimension(dim_key, dim_size)

def _create_variables(rootgrp, vname_map):
    """ Create all variables

        Params : 
            rootgrp : netcdf filehandle
            vname_map : list of BUFR info objects

        Output : 
            nc_vars : dict
                netcdf variable name and variable object

    """
    nc_vars = {}
    for key, ncvar_params in vname_map.iteritems():
        
        # only try to convert variables that define a netcdf_name parameter
        try:
            # don't try to write to netcdf file if name is not defined
            ncvar_name = ncvar_params['netcdf_name']
        except KeyError:
            continue

        # Guard ... dimension names cannot also be used at variable names
        if ncvar_name in rootgrp.dimensions:
            raise ValueError("Variable name %s also used as dimension name" %\
                    ncvar_name)

        try:
            var_type = ncvar_params['var_type']
            if var_type not in ['int', 'float', 'str', 
                    'double', 'long']:
                raise BUFR2NetCDFError("Not a valid type %s" % var_type )
        except KeyError:
            raise BUFR2NetCDFError("Not a valid type %s" % var_type )
        #FIXME VARIABLE DIM CONFLICT
        try:
            fillvalue = ncvar_params['netcdf__FillValue']
        except KeyError:
            fillvalue = None

        try:
            # variable can be packed into a scalar
            if ncvar_params['packable_1dim'] and ncvar_params['packable_2dim']:
                nc_vars[key] = rootgrp.createVariable( ncvar_name, 
                        netcdf_datatype(ncvar_params['var_type']) , 
                        ('scalar',), fill_value=eval(var_type)(fillvalue), 
                        zlib=True,least_significant_digit=3 )
            # variable can be packed into a scanline vector 
            elif ncvar_params['packable_1dim']:
                nc_vars[key] = rootgrp.createVariable( ncvar_name, 
                        netcdf_datatype(ncvar_params['var_type']) , 
                        (ncvar_params['netcdf_dimension_name'] ,), 
                        fill_value=eval(var_type)(fillvalue), 
                        zlib=True,least_significant_digit=3 )
            # variable can be packed into a per scanline vector
            elif ncvar_params['packable_2dim']:
                nc_vars[key] = rootgrp.createVariable( ncvar_name, 
                        netcdf_datatype(ncvar_params['var_type']) , 
                        ('record',), fill_value=eval(var_type)(fillvalue), 
                        zlib=True,least_significant_digit=3 )
            # variable can't be packed
            else:
                nc_vars[key] = rootgrp.createVariable( ncvar_name, 
                        netcdf_datatype(ncvar_params['var_type']) , 
                        ('record', ncvar_params['netcdf_dimension_name'] ), 
                        fill_value=eval(var_type)(fillvalue), 
                        zlib=True,least_significant_digit=3 )

            setattr(nc_vars[key], 'unit', ncvar_params['netcdf_unit'] )

            if 'netcdf_long_name' in ncvar_params:
                setattr(nc_vars[key], 'long_name', 
                        ncvar_params['netcdf_long_name'])

        except KeyError, key_exception:
            logging.exception("Unable to find netcdf conversion, parameters")


    return nc_vars


def _insert_record(vname_map, nc_var, record, scalars_handled, count, linked_index):
    """ Inserts data into a netcdf variable

        Parameters:
            vname_map : dict
                A dict extracted from the database containing metadata on this
                particular variable.

            nc_var : NetCDFVariable
                The NetCDF variable object.
                
            record : BUFRRecordEntry
                The BUFR data from the bufr reader.

            scalars_handled : bool
                Defines whether variables that should be considdered as
                constants / scalars are handled

            count : int
                Defines in which entry in the NetCDF running variable this
                data should be inserted. This should be a continously
                increasing variable.
                
    """

    try:
        
        # pack data  if possible
        packable_1dim = int(vname_map[linked_index]['packable_1dim'])
        packable_2dim = int(vname_map[linked_index]['packable_2dim'])

        var_type = vname_map[linked_index]['var_type']
        if var_type not in ['int', 'float', 'str', 
                'double', 'long']:
            logging.error("No valid type defined")
            return
       
        # Handle 32/64 numpy conversion
        if 'int' in var_type and not packable_2dim:
            var_type = 'int32'

        try:
            #
            # Handle cases where the data can be packed into a single value or
            # a single row or single column

            # Data packs to a single value
            if packable_2dim and packable_1dim:
                if not scalars_handled:
                    data = bufr.pack_record(record)
                    try:
                        nc_var[ 0 ] = eval(var_type)(data)
                    except OverflowError, overflow_error:
                        logging.exception("Unable to convert "+\
                                "value for %s in %s" %\
                                ( data, vname_map[linked_index]['netcdf_name']))
                        nc_var[ 0 ] = vname_map[linked_index]\
                                ['netcdf__FillValue']
                        
                return
            # Data packs to a single row
            elif packable_1dim:
                if not scalars_handled:
                    size = vname_map[ linked_index ]\
                            [ 'netcdf_dimension_length' ]
                    data = record.data
                    
                    if data.shape[ 0 ] < size:
                        fillvalue = vname_map[ linked_index ]\
                                [ 'netcdf__FillValue' ]
                        data = bufr.pad_record(data, size, fillvalue)
                    elif data.shape[0] > size:
                        raise BUFR2NetCDFError("Size mismatch netcdf "+\
                                "variable expected size is"+\
                                " %d, data size is %d" % (size, 
                                    data.shape[0]) )
                    nc_var[:] = data
                return
            # Data packes to a single column
            elif packable_2dim:
                data = bufr.pack_record(record)
                try:
                    nc_var[ count ] = eval(var_type)(data)
                except OverflowError, overflow_error:
                    logging.exception("Unable to convert value for %s in %s" %\
                            ( data, vname_map[linked_index]['netcdf_name']))
                    nc_var[ count ] = vname_map[linked_index]\
                            ['netcdf__FillValue']
                return

        except bufr.RecordPackError, pack_error:
            logging.exception("Unable to pack data for %s" %\
                    ( vname_map[linked_index]['netcdf_name'], ))

        
        
        # Handle data with varying lengths, padd data to fit size
        size = vname_map[ linked_index ][ 'netcdf_dimension_length' ]
        data = record.data
        
        if data.shape[ 0 ] < size:
            fillvalue = vname_map[ linked_index ][ 'netcdf__FillValue' ]
            data = bufr.pad_record(data, size, fillvalue)
        elif data.shape[0] > size:
            raise BUFR2NetCDFError("Size mismatch netcdf "+\
                    "variable expected size is %d, data size is %d" %\
                    (size, data.shape[0]) )
        
        nc_var[ count, : ] = data.astype(var_type)

    except ValueError, val_err:
        logging.exception("Unable to insert records %s" % (val_err, ))

def bufr2netcdf(instr_name, bufr_fn, nc_fn, dburl=None):
    """ Does the actual work in transforming the file """
    
    # Create file object and connect to database
    bfr = bufr.BUFRFile(bufr_fn)
    try:
        os.remove(nc_fn)
    except OSError:
        pass
    
    rootgrp = Dataset(nc_fn, 'w', format='NETCDF4')

    conn = None
    if dburl is not None:
        conn = bufrmetadb.BUFRDescDBConn(dburl)
    else:
        conn = bufrmetadb.BUFRDescDBConn()

    instr = conn.get_instrument(instr_name)

    #Get bufr record sections from database
    bstart = instr.bufr_record_start
    bend = instr.bufr_record_end

    # Read BUFR file keys and get corresponding NetCDF names from 
    # from the database. Fast forward to record start. 
    for i in range(bstart+1):
        records = bfr.read()

    # Set up accounting for each variable , to be used when wriing variables to
    # netcdf.
    bfr_count = []
    for r in records:
        bfr_count.append(0)

    vname_map = conn.get_netcdf_parameters_dict(instr_name)
    
    # get replication indicies, the indicies handle multiple records of the
    # same variable within a bufr subsection.
    replication_indicies = conn.get_replication_indicies(instr_name)
    
    # Create attributes 
    _create_global_attributes(rootgrp, instr)

    # Create dimensions
    _create_dimensions(rootgrp, vname_map)

    #
    # Get list of variables which should be treated as constants and
    # global attributes, Notice these values should be constant for
    # every entry in the scanline so below we just insert the value
    # from a random scanline 
    #
    global_var_attrs = conn.get_netcdf_global_attrs(instr_name)
    for record in records:
        if record.name in global_var_attrs:
            setattr(rootgrp, global_var_attrs[record.name], "%s" % \
                    record.data)

    # create variables
    _create_variables(rootgrp, vname_map)


    #
    # Insert data into variables
    #

    rootgrp.close()
    rootgrp = Dataset(nc_fn, 'a', format='NETCDF4')
    
    bfr.reset()
    scalars_handled = False

   
    # Loop though all sections and dump to netcdf
    #
    for count, section in enumerate(bfr):

        # manage record boundaries. In some cases BUFR sections differ within a
        # file . This enables the user to only convert similar sections
        if count < bstart: 
            continue
        if bend is not -1 and count > bend-1:
            break

        for record in section:
           
            # linked index handles BUFR replication factors with multiple data
            # entries in one subsection.
            linked_index = replication_indicies[record.index] 

            # only try to convert variables that define the netcdf_name
            # parameter
            try:
                nc_var = rootgrp.variables[vname_map[linked_index]\
                        ['netcdf_name']]
            except KeyError:
                continue
            
            _insert_record(vname_map, nc_var, record, scalars_handled, bfr_count[linked_index], linked_index)
           
            # This variable determines which record number in the netcdf variables the
            # data should be stored 
            bfr_count[linked_index] += 1
        print ",".join([str(i) for i in bfr_count[0:40]])


        # we have inserted the first bufr section and hence all variables that
        # can be packed into scalars or per scan vectors should be accounted for
        scalars_handled = True 

    rootgrp.close()
    #bfr.close()
