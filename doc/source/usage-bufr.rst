
Usage
=====

The debian packages install the python packages and scripts in `/opt` hence you will need to set the following variables (in bash)::

  export PATH=$PATH:/opt/bin
  export PYTHONPATH=/opt/lib/python2.5/site-packages/

If you have an alternative path to your bufr tables you will need to set::

  export BUFR_TABLES=/path/bufrtables/

*Remember* the trailing `/`

By default the ECMWF BUFR library writes out some status information every time a new BUFR file is opened. To supress the status info you will need to set the following variable::

  export PRINT_TABLE_NAMES = false
 
Reading and Inspecting BUFR data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This assumes the above variables are correctly set.::

  Python 2.5.2 (r252:60911, Jul 22 2009, 15:35:03) 
  [GCC 4.2.4 (Ubuntu 4.2.4-1ubuntu3)] on linux2
  Type "help", "copyright", "credits" or "license" for more information.
  >>> import bufr
  >>> bfr = bufr.BUFRFile('NPR_TDIB.SA_D10108_S1823_E2008_B3353637_NS')

This will open the BUFR file.::

  >>> n = bfr.next()
                     ECMWF 
   
        BUFR DECODING SOFTWARE VERSION -  7.2 
              1 APRIL  2007. 
   
   
   
   Your path for bufr tables is :
   /home/krl/bufrtest/bufrtables/       
  BUFR TABLES TO BE LOADED  B0000000300007004001.TXT,D0000000300007004001.TXT
  
The BUFRFile object is a iterator and supports the 'next()' method. This means that you can use the BUFRFile object in a for-loop::

    for n in bfr: print n[0]



