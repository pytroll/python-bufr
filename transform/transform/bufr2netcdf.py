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

""" converts any BUFR file to NetCDF, variable name mapping based on database """

__revision__ = 0.1


import bufr.metadb as bufrmetadb 
from Scientific.IO import NetCDF
import bufr
import numpy as np

class BUFR2NetCDFError(Exception):
    pass

def BUFR2NetCDF(instr_name, bufr_fn, nc_fn, dburl=None):
    """ Does the actual work in transforming the file """
    
    def netcdf_datatype(type_name):
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
        
        raise BUFR2NetCDFError("Cannot convert %s to NetCDF compatible type" % type_name)


    # Create file object and connect to database
    bfr = pybufr.BUFRFile(bufr_fn)
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
    #
    # Create basic dimensions in the NetCDF file
    # 
    ncf.createDimension('record', None)
    ncf.createDimension('scalar', 1)
    
    #
    # Write global attributes from database 
    #
    
    setattr(ncf, 'Conventions', "CF-1.0")
    setattr(ncf, 'title', instr.title.encode('latin-1'))
    setattr(ncf, 'institution', instr.institution.encode('latin-1'))
    setattr(ncf, 'source', instr.source.encode('latin-1'))
    setattr(ncf, 'history', instr.history.encode('latin-1'))
    setattr(ncf, 'references', instr.references.encode('latin-1'))
    setattr(ncf, 'comments', instr.comments.encode('latin-1'))

    #
    # Get list of variables which should be treated as constants and
    # global attributes, Notice these values should be constant for
    # every entry in the scanline so below we just insert the value
    # from a random scanline 
    #
    global_var_attrs = conn.get_netcdf_global_attrs(instr_name)
    for r in records:
        if r.name in global_var_attrs:
            setattr(ncf, global_var_attrs[r.name], "%s" % r.data)
   
    #
    # Create dimensions 
    # 

    dims = {}
    for key in vname_map.keys():
        var_info_dict = vname_map[key]
        dims[ var_info_dict['netcdf_dimension_name'] ] = int( var_info_dict['netcdf_dimension_length'])
    for dim_key, dim_size in dims.iteritems():
        ncf.createDimension(dim_key, dim_size)
   
    #
    # Create all variables
    #
    vars = {}
    for key, ncvar_params in vname_map.iteritems():
        
        # only try to convert variables that define a netcdf_name parameter
        try:
            ncvar_name = ncvar_params['netcdf_name']
        except KeyError, e:
            continue
        
        try:
            # variable can be packed into a scalar
            if ncvar_params['packable_1dim'] and ncvar_params['packable_2dim']:
                vars[key] = ncf.createVariable( ncvar_name, netcdf_datatype(ncvar_params['var_type']) , 
                            ('scalar',))
            # variable can be packed into a scanline vector 
            elif ncvar_params['packable_1dim']:
                vars[key] = ncf.createVariable( ncvar_name, netcdf_datatype(ncvar_params['var_type']) , 
                            (ncvar_params['netcdf_dimension_name'] ,))
            # variable can be packed into a per scanline vector
            elif ncvar_params['packable_2dim']:
                vars[key] = ncf.createVariable( ncvar_name, netcdf_datatype(ncvar_params['var_type']) , 
                            ('record',))
            # varable can't be packed
            else:
                vars[key] = ncf.createVariable( ncvar_name, netcdf_datatype(ncvar_params['var_type']) , 
                            ('record', ncvar_params['netcdf_dimension_name'] ))

            setattr(vars[key], 'unit', ncvar_params['netcdf_unit'] )

            # Ensure that the fillvalue datatype is correct
            fillvalue =  ncvar_params['netcdf__FillValue']
            ##setattr(vars[key], '_FillValue', fillvalue )

            if 'netcdf_long_name' in ncvar_params:
                setattr(vars[key], 'long_name', ncvar_params['netcdf_long_name'] )

        except KeyError, e:
            print e
            pass

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

            # only try to convert variables that define the netcdf_name parameter
            try:
                nc_name = vname_map[record.index]['netcdf_name']
            except KeyError, e:
                print e
                continue
            
            try:
                nc_var = vars[record.index]
                
                # pack data  if possible
                packable_1dim = int(vname_map[record.index]['packable_1dim'])
                packable_2dim = int(vname_map[record.index]['packable_2dim'])
                try:

                    var_type = vname_map[record.index]['var_type']
                    if var_type not in ['int', 'float', 'str', 'double', 'long']:
                        print "no valid type defined"
                        continue

                    if packable_2dim and packable_1dim:
                        if not scalars_handled:
                            data = pybufr.pack_record(record)
                            try:
                                nc_var[ 0 ] = eval(var_type)(data)
                            except OverflowError, e:
                                nc_var[ 0 ] = vname_map[record.index]['netcdf__FillValue']
                        # entry already accounted for, continue
                        continue

                    elif packable_1dim:
                        if not scalars_handled:
                            data = np.array(record.data, var_type) 
                            nc_var[:] = data
                        # entry already accounted for, continue
                        continue

                    elif packable_2dim:
                        data = pybufr.pack_record(record)
                        try:
                            nc_var[ count ] = eval(var_type)(data)
                        except OverflowError, e:
                            nc_var[ count ] = vname_map[record.index]['netcdf__FillValue']
                        continue

                except pybufr.RecordPackError, e:
                    print e
                    pass
                
                # Handle data with varying lengths, padd data to fit size
                size = vname_map[ record.index ][ 'netcdf_dimension_length' ]
                data = record.data
                if data.shape[ 0 ] != size:
                    fillvalue = vname_map[ record.index ][ 'netcdf__FillValue' ]
                    data = pybufr.pad_record(data, size, fillvalue)
                
                data = np.array(data, vname_map[ record.index ][ 'var_type' ]) 
                nc_var[ count, : ] = data

            except ValueError, e:
                print e
                pass

        # we have inserted the first bufr section and hence all variables that
        # can be packed into scalars or per scan vectors should be accounted for
        scalars_handled = True 

    ncf.close()

