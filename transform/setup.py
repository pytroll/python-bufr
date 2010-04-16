from distutils.core import setup, Extension

setup(name='bufr-transform',
      version='0.1',
      description='Translates BUFR variable names and units',
      author='Kristian Rune Larsen',
      author_email='krl@dmi.dk',
      maintainer='Kristian Rune Larsen',
      maintainer_email='krl@dmi.dk',
      packages = ['bufr/transform',],
      package_dir = {'bufr/transform': './transform',},
      scripts = ['transform/scripts/bufr2netcdf',],
     )

