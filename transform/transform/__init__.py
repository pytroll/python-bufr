#!/usr/bin/env python

'''
File: __init__.py
Author: Kristian Rune Larsen 
Description: Facade for transforming BUFR files 
'''

class BUFR2NetCDFError(Exception):
    """ Simple exception for handeling errors """
    pass

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
    
    raise BUFR2NetCDFError("Cannot convert %s to NetCDF compatible type" %\
            type_name)

def bufr2netcdf(input_bufr_file, output_nc_file, netcdf='netcdf4', 
        database_url='sqlite:///bufr.db'):
    """Facade method for extracting instrument type from filename and decoding
    BUFR all at once"""
    from bufr import metadb

    # get instruments from database 
    conn = metadb.BUFRDescDBConn(database_url) 
    instr = conn.get_instrument_from_filename(input_bufr_file)

    # Transform BUFR file
    if netcdf == 'netcdf4':
        from bufr.transform import bufr2netcdf4
        bufr2netcdf4.bufr2netcdf(instr, input_bufr_file, output_nc_file, database_url) 
    elif netcdf == 'netcdf3':
        from bufr.transform import bufr2netcdf3
        bufr2netcdf3.bufr2netcdf(instr, input_bufr_file, output_nc_file, database_url) 
    else:
        raise BUFR2NetCDFError("Invalid netcdf argument \
valid arguments are 'netcdf4' and 'netcdf3'")
        

    
