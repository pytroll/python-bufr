#!/usr/bin/env python
import os
import sys
from distutils.core import setup, Extension 

try:
    BUFR_LIBRARY_PATH=os.environ['BUFR_LIBRARY_PATH']
    BUFR_TABLES=os.environ['BUFR_TABLES']
    NUMPY_INCLUDE_PATH=os.environ['NUMPY_INCLUDE_PATH']
except KeyError, e:
    print ("""Please define system variables 
            
            BUFR_LIBRARY_PATH, directory containing libbufr.a
            NUMPY_INCLUDE_PATH, directory containing numpy related include files
                                like numpy/arrayobject.h, numpy/arrayscalars.h, etc.

            BUFR_TABLES, path to your BUFR tables, this can be changed
            runtime by changing the environment variable
            
            """)
    sys.exit(1)

BUFRFile = Extension('bufr/_BUFRFile',
                     define_macros = [('DTABLE_PATH', BUFR_TABLES),],
                     sources = ['pybufr/_BUFRFile.c',], 
                     extra_compile_args = ['-O3', ], 
                     extra_link_args = [], 
                     libraries = ['bufr','gfortran',],
                     library_dirs = ['/usr/lib','/usr/local/lib',
                                     '/usr/lib64','/usr/local/lib64',
                                     BUFR_LIBRARY_PATH, ],
                     include_dirs = ['/usr/include',
                                     '/usr/local/include',
                                     NUMPY_INCLUDE_PATH])

setup(name='python-bufr',
      version='0.3',
      description='Generic Python BUFR file reader based on the ECMWF BUFR library',
      author='Kristian Rune Larsen',
      author_email='krl@dmi.dk',
      packages = ['bufr'],
      ext_modules = [ BUFRFile, ]
     )
