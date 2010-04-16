from distutils.core import setup, Extension

setup(name='bufr-metadb',
      version='0.1',
      description='Translates BUFR variable names and units',
      author='Kristian Rune Larsen',
      author_email='krl@dmi.dk',
      maintainer='Kristian Rune Larsen',
      maintainer_email='krl@dmi.dk',
      package_dir = {'bufr/metadb': './metadb',},
      packages = ['bufr/metadb',],
      scripts = ['metadb/scripts/import_bufr','metadb/scripts/export_bufr',],
     )

