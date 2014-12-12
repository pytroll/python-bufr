Install 
=======

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

Some notes for old users. The dependencies of the packages have moved back and
forth between the ECMWF Bufrdc package and the ECMWF Emos package ... it is not
very nice I know. I thought that the Emos library was more widely used and the
Ubuntu 10.04 distribution included an Emos Debian package so I switched to the
Emos package. Now I have moved back to the bufrdc package since the Emos
package in the Ubuntu distribution will not work anyway because you need to
adjust the compiler flags. The Bufrdc package us preferred right now because it
is smaller. Please do not hesitate to let me know if you think otherwise. 

ECMWF BUFR Library
++++++++++++++++++

Follow the install instructions in the package README file and use the
**build_library** and **install scripts**. However before you run the script you will
need to change the configuration file for you platform. The configuration files
can be found in config directory. 


The config files are sorted by OS, architecture and compiler 

The OS can be found using the following command::

    uname -s | tr '[A-Z]' '[a-z]'

E.g the output of this command could be *Linux*.

The platform can be found using the following command::

    uname -m

E.g the output of this command could be *x84_64*

I normally choose to use the *gfortran* compiler, and if I avoid the 64 bit reals
that the **build_library** script questions me for the resulting configuration file
will be::  

 config/config.linux_gfortranA64 
 
following the pattern:: 

  config.<OS><_compiler><architecture>
  
Notice that for the default compiler the '_compiler' will be an empty string.

You will need to edit the file and add the option *-fPIC* to the *CFLAGS* and
*FFLAGS* variables.

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

To install using pypi use the following commands::
  
  LDFLAGS=-L<path to libbufr directory> pip install python-bufr

The *LDFLAGS* variable tells the installation script in what directory to find
the *libbufr.a* library. 

Install Using Source
++++++++++++++++++++

Download the source package from the google-code download page. Untar the
package and run the setup.py script::

  LDFLAGS=-L<path to libbufr directory> python setup.py install 

The *LDFLAGS* variable tells the installation script in what directory to find
the *libbufr.a* library. 

Install Using Debian Packages
+++++++++++++++++++++++++++++

The debian packages are build for an Ubuntu 10.04 lucid 64 bit. Only install
the packages if you have this exact OS and architecture.

First add the python-bufr repository your apt sources list::

    sudo -s # be root otherwise cat will not work
    cat << EOF > /etc/apt/sources.list.d/python-bufr.list
    #
    # python bufr repository
    deb http://python-bufr.googlecode.com/svn/apt lucid main
    EOF
    
    apt-get update
    apt-get install python-bufr


This will install python-bufr and a new emos library in into the */opt* directory. 

