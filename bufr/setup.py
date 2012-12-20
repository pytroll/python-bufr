#!/usr/bin/env python
#
# python-bufr , wrapper for ECMWF BUFR library
# 
# Copyright (C) 2012  Kristian Rune Larsen
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

""" install using LDFLAGS=-L/<path to emos library> CFLAGS=-I/<emos headers> python setup.py install  """

import os
import sys
from distutils.core import setup, Extension 

BUFRFile = Extension('bufr/_BUFRFile',
                     sources = ['bufr/_BUFRFile.c',], 
                     extra_compile_args = ['-O3','-g','-fstack-protector-all',
                         '-D_FORTIFY_SOURCE=2'], 
                     extra_link_args = [], 
                     libraries = ['emos','gfortran',])

setup(name='python-bufr',
      version='0.2-3',
      description='Generic Python BUFR file reader based on the ECMWF BUFR library',
      author='Kristian Rune Larsen',
      author_email='krl@dmi.dk',
      url="http://python-bufr.googlecode.com/",
      install_requires=['',],
      classifiers=[
      'Development Status :: 5 - Production/Stable',
      'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
      'Programming Language :: Python',
      'Operating System :: OS Independent',
      'Intended Audience :: Science/Research',
      'Topic :: Scientific/Engineering'
      ],
      packages = ['bufr'],
      ext_modules = [ BUFRFile, ]
     )
