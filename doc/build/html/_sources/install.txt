Install 
-------

Short description on how to install the python-bufr module from source

Dependencies
~~~~~~~~~~~~

The following dependencies are needed for the python-bufr package to work

*  ECMWF BUFR decoding library http://www.ecmwf.int/products/data/software/download/bufr.html
*  Python Numpy http://numpy.scipy.org/

If you are using easy_install or pip you only need to manually install the ECMWF bufr library. 

.. warning::
    
    Note that the ECMWF bufr library needs to be compiled with the -fPIC option
    both for the C and Fortran code please see instructions below. 

    You can use both the bufrdc library (default) and the emos library from
    ECMWF. In case you want to use the emos library you will have to change the
    library_dirs variable i the setup.py file.


ECMWF BUFR Library
++++++++++++++++++

Follow the install instructions in the package README file and use the
./build_library and install scripts. Hovever before you run the script you will
need to change the configuration file for you platform. The configuration files
can be found in config directory. 


The config files are sorted by OS, architecture and compiler 

The OS can be found using the following command
{{{
 uname -s | tr '[A-Z]' '[a-z]'
}}}
E.g the output of this command could be linux.

The platform can be found using the following command
{{{
uname -m
}}}
E.g the ouput of this command could be x84_64

I normally choose to use the gfortran compiler, and if I avoid the 64 bit reals
that the build_library script questions me for the resulting configuration file
will be:  config/config.linux_gfortranA64 following the pattern
config.<OS><_compiler><architecture>. Notice that for the default compiler the
_compiler will be an empty string.

You will need to edit the file and add the option '-fPIC' to the CFLAGS and
FFLAGS variable.

If you do not know which file to edit the build_library script prints it out,
so you can run the script first, find the config file, edit it and then rerun
the build_library script. 

.. warning::
    
    Note that by default the build_library script will create a libbufr.a that
    is read-only. This results in that that 'make clean' will not work
    proporly. You will have to manually remove the libbufr.a library from the
    root source directory. This also applies for the directories and libraries
    installed using the install script included in the source. 


Install Python-Bufr
~~~~~~~~~~~~~~~~~~~~

There are multiple ways to install the python-bufr code. At DMI we use debian
packages , you can install from source or you can use the pypi package. 

In the two later cases I recommend that you install using a virtual
environment. See http://pypi.python.org/pypi/virtualenv

Install Using the Pypi Package
++++++++++++++++++++++++++++++

To install using pypi use the following commands

{{{
LDFLAGS=-L/<path to libbufr directory> pip install python-bufr
}}}

The LDFLAGS variable tells the installation script in what directory to find
the libbufr.a library. 


Install Using Source
++++++++++++++++++++

Download the source package from the google-code download page. Untar the package and run the setup.py script
{{{
LDFLAGS=-L/<path to libbufr directory> python setup.py install 
}}}

The LDFLAGS variable tells the installation script in what directory to find
the libbufr.a library. 

