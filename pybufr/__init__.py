#!/usr/bin/env python
#
#
# Author Kristian Rune Larsen <krl@dmi.dk>

""" Generic Python BUFR file reader. 

Reads all BUFR files supported by the ECMWF library 

"""
from _BUFRFile import *


def netcdf_compliant_names( section ):
    """ return variables as netcdf compliant names 
        
        input: BUFR section 
        output: List of unique names 
    
    """
    names = []
    for index, record in enumerate(section):
        name = record.name.strip().lower()
        for i in ('(', ')', ' ', '-'):
            name = name.replace(i, '_')
        names.append("%s_%d" % (name, index))
    return names

def max_record_length( bfr ):
    """ 
    
    Searches through file a finds maximum length of each record. 
    The filehandle is reset afterwards

    input : bufrfile object
    output : list of lengths
    
    """
    lengths = {}
    for section in bfr:
        for index,record in enumerate(section):
            if index in lengths:
                if record.data.__class__.__name__ != 'ndarray':
                    continue
                elif len(record.data) > lengths[index]:
                    lengths[index] = len(record.data)
            else:
                if record.data.__class__.__name__ != 'ndarray':
                    lengths[index] = 1
                else:
                    lengths[index] = len(record.data)
    bfr.reset()
    slengths = zip(lengths.keys(), lengths.values())
    slengths.sort()
    # return list of sorted max values
    return [ j for i,j in slengths]

def pad_record( record , length , fill_value):
    """
    pads a record with fill values to 

    input , record, max size, fill value

    output is a new record. 
    """
    pass




