from distutils.core import setup, Extension

setup(name='bufr-metadb',
      version='0.2-2',
      description='Translates BUFR variable names and units',
      author='Kristian Rune Larsen',
      author_email='krl@dmi.dk',
      maintainer='Kristian Rune Larsen',
      maintainer_email='krl@dmi.dk',
      package_dir = {'bufr/metadb': './metadb',},
      packages = ['bufr/metadb',],
      scripts = ['metadb/scripts/import_bfr','metadb/scripts/export_bfr','metadb/scripts/assign_variables'],
     )

