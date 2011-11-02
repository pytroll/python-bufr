#!/usr/bin/env python
import os
import sys
from distutils.core import setup, Extension 

#default_np_path = '/usr/lib/python2.5/site-packages/numpy/core/include/'
default_np_path = '/usr/share/pyshared/numpy/core/include/'
try:
    NUMPY_INCLUDE_PATH=os.environ['NUMPY_INCLUDE_PATH']
except:
    if os.path.exists(default_np_path):
        NUMPY_INCLUDE_PATH=default_np_path
    else:
        print """Please define system variable 
            
            NUMPY_INCLUDE_PATH, directory containing numpy related include files
                                like numpy/arrayobject.h, numpy/arrayscalars.h, etc.
            """
        sys.exit(1)

##,'-DNPY_SIZE_OF_DOUBLE=8'
BUFRFile = Extension('bufr/_BUFRFile',
                     sources = ['bufr/_BUFRFile.c',], 
                     extra_compile_args = ['-O3','-g','-fstack-protector-all',
                         '-D_FORTIFY_SOURCE=2'], 
                     extra_link_args = [], 
                     libraries = ['emos','gfortran',],
                     library_dirs = ['/opt/lib/emos',
                                     '/usr/lib64','/usr/local/lib64',],
                     include_dirs = ['/usr/include',
                                     '/usr/local/include',
                                     NUMPY_INCLUDE_PATH])

setup(name='python-bufr',
      version='0.2-2',
      description='Generic Python BUFR file reader based on the ECMWF BUFR library',
      author='Kristian Rune Larsen',
      author_email='krl@dmi.dk',
      packages = ['bufr'],
      ext_modules = [ BUFRFile, ]
     )
