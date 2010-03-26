#!/usr/bin/env python
#
# python-bufr , wrapper for ECMWF BUFR library
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

""" Generic Python BUFR file reader. 

Reads all BUFR files supported by the ECMWF library 

"""

__revision__ = 0.1

from _BUFRFile import *
import numpy as np

class RecordPackError(Exception):
    """ Raised when packing non homogenous data """
    pass

class BUFRRecordInfo:
    """ Small metadata container 
        
        holds info of record max size , name , if the record is packable,
        etc... 

        This class is sort of the python equivalent of the BUFRFileEntry class,
        maybe in the future these two classes will be fused.

        This also corresponds to a database record class . 
    
    """
    max_length = 1
    packable = True
    name = None
    index = None
    unit = None
    dimension_name = None
    dimension_lenght = None
    fillvalue_double = 1.7e38 # standard BUFR fill value
    fillvalue_int = 2147483647 # standard BUFR fill value
    type = np.int

    def __str__(self):
        """ Print string representation """
        ostr = ""
        for k,v in self.__dict__.iteritems():
            ostr = ostr + "%s : %s " % (k,v)
        return ostr
 

def netcdf_compliant_names( section ):
    """ return variables as netcdf compliant names 
        
        input: BUFR section 
        output: List of unique names 
    
    """
    names = {} 
    for record in section:
        name = record.name.strip().lower()
        for i in ('(', ')', ' ', '-'):
            name = name.replace(i, '_')
        names[record.index] = "%s_%d" % (name, record.index)
    return names


def get_type( record ): 
    """ 
        Tries to figure out if this is a integer or a float

        input : bufr record
        output : np type object
    
    """
    eps = np.finfo(np.float).eps
    try:
        if True in (np.floor(record.data) - record.data > 10*eps):
            return np.float
        return np.int
    except TypeError:
        return record.data.__class__.__name__


def pack_record( record ): 
    """ 
        packes a single measured record if all elements are alike 

        input: numpy array
        output: numpy array or scalar

    """
    try:
        N = len(record.data) # throws exception if not supported

        first_elem = record.data[0]
        eps = 0
        try:
            eps = np.finfo(first_elem.__class__).eps # find machine epsilon for this type
        except ValueError:
            # Handle integers, eps=0
            pass
        
        # compare all elements to the first element, maybe a bit slow since
        # it's a while array operation
        if True in ( record.data - first_elem > 10*eps ):
            # One of the numbers differ 
            raise RecordPackError("Heteogenous data")

        # If we reach this point we were able to pack the array into a single
        # scalar and we only need to return one variable

        # return int , if possible 
        if np.floor(first_elem) - first_elem < 10*np.finfo(np.float).eps:
            return int(first_elem)

        # return standard floating point
        return first_elem

    except TypeError, e:
        # if we didn't get an object supporting len we just return the object
        # because it must be a scalar
        raise RecordPackError("Not an array")


def pack_section( section ):
    """ 
        facilicates packing an entire bufr section by packing and iterating through the entire section.

        input: bufr section

        output: packed records

        implements iterator pattern through the yield command

        Usage: 

        bfr = BUFRFile('<bufr filename>')
        my_data = []
        for i in pack_section(bfr.next()):
            my_data.append(i)

        # the my_data array now contains the packed data for this section 
    """

    for record in section:
        yield pack_record(record)

def get_bfr_info(bfr):
    """ 
    
    Get prilimenery file info. This includes record lengts, if records are
    packable... etc. 
    
    
    """
    info_list = [] # this lists the BUFRRecordInfo objects
    section = bfr.next()
    
    # initialise the info objects
    for record in section:
        info_obj = BUFRRecordInfo()
        info_obj.name = record.name 
        info_obj.unit = record.unit
        info_obj.index = record.index
        try:
            pdata = pack_record(record) 
            info_obj.type = pdata.__class__.__name__
            info_obj.packable = True
        except RecordPackError, e:
            info_obj.packable = False
            info_obj.type = get_type(record)
        try:
            info_obj.max_length = len(record.data)
        except TypeError, e:
            pass
        info_list.append(info_obj)
    
    # Search through the rest of the file

    for section in bfr:
        for record in section:
            info_obj = info_list[record.index]
            try:
                N = len(record.data) # throws TypeError if not defined for record.data
                if N > info_obj.max_length: 
                    info_obj.max_length = N

                if info_obj.packable:
                    pdata = pack_record(record)
                    if info_obj.type is 'int':
                        info_obj.type = pdata.__class__.__name__

            except TypeError, e:
                info_obj.packable = False
                if info_obj.type is 'int':
                    info_obj.type = record.data.__class__.__name__
            except RecordPackError, e:
                info_obj.packable = False
                if info_obj.type is 'int':
                    info_obj.type = get_type(record)

    # Resets the file handle
    bfr.reset()

    # Initialise the dimension lenghts
    for info_obj in info_list:
        info_obj.dimension_length = info_obj.max_length

    return info_list


def pad_record( record , length , fill_value):
    """
    pads a record with fill values to 

    input , record, max size, fill value

    outputs a new padded array
    """
    padded_array = np.ones(length)*fill_value
    try:
        padded_array[0:len(record)] = record
    except TypeError:
        # handle scalar values
        return np.ones(length)*record
    return padded_array


def pad_section( section, info_list ):
    """
        Generator function used for padding data in an entire section 
        
        input : section , list of info objects.

        yields a padded array

    """
    for record in section:
        info_object = info_list[record.index]
        yield padded_array(record, info_object.dimension_length, info_object.fillvalue)


