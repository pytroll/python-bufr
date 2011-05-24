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
import traceback

import numpy as np
from Scientific.IO import NetCDF

import bufr.metadb as bufrmetadb 
import bufr

class BUFR2NetCDFError(Exception):
    """ Simple exception for handeling errors """
    pass

def _netcdf_datatype(type_name):
    """ converts numpy datatype to NetCDF datatype 
        
        all floats are converted to doubles
        all integers are converted to 4 byte int
        all chars are converted to NetCDF byte-type
    
    """
    if 'float' in type_name:
        return 'd'
    if 'int' in type_name:
        return 'i'
    if 'long' in type_name:
        return 'i'
    if 'string' in type_name:
        return 'b'
    
    raise BUFR2NetCDFError("Cannot convert %s to NetCDF compatible type" %\
            type_name)

def _create_global_attributes(ncf, instr):
    """ Creates global netcdf attributes and assigns values
    
        Params : 
            ncf : filehandle
            instr : SQLalchemy BufrDesc object

    """
    
    setattr(ncf, 'Conventions', "CF-1.0")
    setattr(ncf, 'title', instr.title.encode('latin-1'))
    setattr(ncf, 'institution', instr.institution.encode('latin-1'))
    setattr(ncf, 'source', instr.source.encode('latin-1'))
    setattr(ncf, 'history', instr.history.encode('latin-1'))
    setattr(ncf, 'references', instr.references.encode('latin-1'))
    setattr(ncf, 'comments', instr.comments.encode('latin-1'))


def _create_dimensions(ncf, vname_map):
    """ Create dimensions 

        Param : 
            ncf : netcdf filehandle
            vname_map : list of BUFR info objects

    """ 

    # Create basic dimensions in the NetCDF file

    ncf.createDimension('record', None)
    ncf.createDimension('scalar', 1)

    # Create dimensions from database
    dims = {}
    for key in vname_map.keys():
        var_info_dict = vname_map[key]
        if 'netcdf_name' in var_info_dict:
            dims[ var_info_dict['netcdf_dimension_name'] ] = \
                    int( var_info_dict['netcdf_dimension_length'])
    for dim_key, dim_size in dims.iteritems():
        ncf.createDimension(dim_key, dim_size)

def _create_variables(ncf, vname_map):
    """ Create all variables

        Params : 
            ncf : netcdf filehandle
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
        
        try:
            # variable can be packed into a scalar
            if ncvar_params['packable_1dim'] and ncvar_params['packable_2dim']:
                nc_vars[key] = ncf.createVariable( ncvar_name, 
                        _netcdf_datatype(ncvar_params['var_type']) , 
                        ('scalar',))
            # variable can be packed into a scanline vector 
            elif ncvar_params['packable_1dim']:
                nc_vars[key] = ncf.createVariable( ncvar_name, 
                        _netcdf_datatype(ncvar_params['var_type']) , 
                        (ncvar_params['netcdf_dimension_name'] ,))
            # variable can be packed into a per scanline vector
            elif ncvar_params['packable_2dim']:
                nc_vars[key] = ncf.createVariable( ncvar_name, 
                        _netcdf_datatype(ncvar_params['var_type']) , 
                        ('record',))
            # variable can't be packed
            else:
                nc_vars[key] = ncf.createVariable( ncvar_name, 
                        _netcdf_datatype(ncvar_params['var_type']) , 
                        ('record', ncvar_params['netcdf_dimension_name'] ))

            setattr(nc_vars[key], 'unit', ncvar_params['netcdf_unit'] )

            var_type = ncvar_params['var_type']
            if var_type not in ['int', 'float', 'str', 
                    'double', 'long']:
                print "no valid type defined"
                return

            fillvalue = ncvar_params['netcdf__FillValue']
            setattr(nc_vars[key], '_FillValue', eval(var_type)(fillvalue))

            if 'netcdf_long_name' in ncvar_params:
                setattr(nc_vars[key], 'long_name', 
                        ncvar_params['netcdf_long_name'])

        except KeyError, key_exception:
            traceback.print_exc(file=sys.stdout)
            print key_exception

    return nc_vars


def _insert_record(vname_map, nc_var, record, scalars_handled, count):
    """ Insert record in netcdf variables
    """

    try:
        
        # pack data  if possible
        packable_1dim = int(vname_map[record.index]['packable_1dim'])
        packable_2dim = int(vname_map[record.index]['packable_2dim'])

        var_type = vname_map[record.index]['var_type']
        if var_type not in ['int', 'float', 'str', 
                'double', 'long']:
            print "no valid type defined"
            return
       
        # Handle 32/64 numpy conversion
        if 'int' in var_type and not packable_2dim:
            var_type = 'int32'

        try:
            if packable_2dim and packable_1dim:
                if not scalars_handled:
                    data = bufr.pack_record(record)
                    try:
                        nc_var[ 0 ] = eval(var_type)(data)
                    except OverflowError, overflow_error:
                        traceback.print_exc(file=sys.stdout)
                        nc_var[ 0 ] = vname_map[record.index]\
                                ['netcdf__FillValue']
                return

            elif packable_1dim:
                if not scalars_handled:
                    ##data = np.array(record.data.tolist(), var_type) 
                    nc_var[:] = data.astype(var_type)
                return

            elif packable_2dim:
                data = bufr.pack_record(record)
                try:
                    nc_var[ count ] = eval(var_type)(data)
                except OverflowError, overflow_error:
                    traceback.print_exc(file=sys.stdout)
                    nc_var[ count ] = vname_map[record.index]\
                            ['netcdf__FillValue']
                return

        except bufr.RecordPackError, pack_error:
            traceback.print_exc(file=sys.stdout)
            print pack_error
        
        
        # Handle data with varying lengths, padd data to fit size
        size = vname_map[ record.index ][ 'netcdf_dimension_length' ]
        data = record.data
        
        if data.shape[ 0 ] < size:
            fillvalue = vname_map[ record.index ][ 'netcdf__FillValue' ]
            data = bufr.pad_record(data, size, fillvalue)
        elif data.shape[0] > size:
            raise BUFR2NetCDFError("Size mismatch netcdf "+\
                    "variable expected size is %d, data size is %d" %\
                    (size, data.shape[0]) )
        
        #convert to netcdf variable type
        ##data = np.array(data.tolist(), dtype=np.dtype(var_type))

        nc_var[ count, : ] = data.astype(var_type)

    except ValueError, val_err:
        traceback.print_exc(file=sys.stdout)
        print val_err



def bufr2netcdf(instr_name, bufr_fn, nc_fn, dburl=None):
    """ Does the actual work in transforming the file """
    
    # Create file object and connect to database
    bfr = bufr.BUFRFile(bufr_fn)
    try:
        os.remove(nc_fn)
    except OSError:
        traceback.print_exc(file=sys.stdout)
        pass
    
    ncf = NetCDF.NetCDFFile(nc_fn,'w')

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
    bfr_keys = [r.index for r in records]
    vname_map = {}
    for k in bfr_keys:
        vname_map[k] = conn.get_netcdf_parameters(instr_name, k)
    
    # Create attributes 
    _create_global_attributes(ncf, instr)

    # Create dimensions
    _create_dimensions(ncf, vname_map)

    #
    # Get list of variables which should be treated as constants and
    # global attributes, Notice these values should be constant for
    # every entry in the scanline so below we just insert the value
    # from a random scanline 
    #
    global_var_attrs = conn.get_netcdf_global_attrs(instr_name)
    for record in records:
        if record.name in global_var_attrs:
            setattr(ncf, global_var_attrs[record.name], "%s" % \
                    record.data)

    # create variables
    _create_variables(ncf, vname_map)

    # close file and reopen in append mode
    ncf.close()
    ncf = NetCDF.NetCDFFile(nc_fn,'a')

    #
    # Insert data into variables
    #
    
    bfr.reset()
    count = -1
    scalars_handled = False
    for section in bfr:
        count = count + 1 
        ncf.sync()
        # manage record boundaries... 
        if count < bstart: 
            continue
        if bend is not -1 and count > bend-1:
            break
        for record in section:
            # only try to convert variables that define the netcdf_name
            # parameter
            try:
                nc_var = ncf.variables[vname_map[record.index]['netcdf_name']]
            except KeyError:
                continue
            
            _insert_record(vname_map, nc_var, record, scalars_handled, count)

        # we have inserted the first bufr section and hence all variables that
        # can be packed into scalars or per scan vectors should be accounted for
        scalars_handled = True 

    ncf.close()
