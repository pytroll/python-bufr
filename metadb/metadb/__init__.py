#!/usr/bin/env python
#
# python-bufr-metadb , BUFR variable translation database
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

""" 
Variable descriptions of bufr file entries 

"""

__revision__ = 0.1

import bufr

import sqlalchemy
from sqlalchemy import *
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relation, backref, sessionmaker
from sqlalchemy.orm.exc import *

from sqlalchemy_marshall import *


Base = declarative_base()

#
# Top level entry ... instrument based 
#

class BUFRDesc(Base):
    """ Interface to the bufr_desc table which is a instrument 
    based entry to retriving variable mappings """
    
    __tablename__ = 'bufr_desc'
    #mapping
    id = Column(Integer, primary_key=True)
    name = Column(String, unique=True)
    description = Column(String)
    fn_regex = Column(String)
    title = Column(String)
    institution = Column(String)
    source = Column(String)
    history = Column(String)
    references = Column(String)
    comments = Column(String)
    bufr_record_start = Column(Integer)
    bufr_record_end = Column(Integer)

    _xml_vars =  ('id', 'name', 'description', 'fn_regex', 'title', 
            'institution', 'source', 'history','references', 'comments', 
            'bufr_var', 'bufr_record_start', 'bufr_record_end')
    
    _alt_key = 'name'
    
    def __init__(self, name=None, fn_regex=None):
        self.name = name
        self.description = ""
        self.fn_regex = fn_regex
        self.title = ""
        self.institution = ""
        self.source = ""
        self.history = "Converted from BUFR"
        self.references = ""
        self.comments = ""
        self.bufr_record_start = 0
        self.bufr_record_end = -1
    
    def __repr__(self):
        return "<BUFRDesc('%s', '%s')>" % (self.name, self.fn_regex, )

class BUFRVar(Base ):
    """ Interface to the bufr_var table which holds 
    the variable names for each instrument"""

    __tablename__ = 'bufr_var'
    #mapping
    id = Column(Integer, primary_key=True)
    name = Column(String)
    bdesc_id  = Column(Integer, ForeignKey('bufr_desc.id'),)
    bdesc = relation(BUFRDesc, backref=backref('bufr_var', order_by=id) ) 
    seq = Column(Integer) # BUFR File sequence number
    __table_args__ = ( UniqueConstraint('bdesc_id', 'name', 'seq', 
        name='instr_var'), {})

    ##_xml_vars =  ('id', 'name', 'bdesc', 'seq', 'bufr_param')
    _xml_vars =  ('id', 'name', 'bdesc_id', 'seq', 'bufr_param')
    _alt_key = 'seq'

    def __init__(self, desc=None, name=None, seq=None):  
        self.bdesc = desc
        self.name = name
        self.seq = seq

    def __repr__(self):
        return "<BUFRVar('%s', '%s', '%s')>" % (self.name, self.bdesc_id, 
                self.seq)

class BUFRDataType(Base):
    """ Defines basic python datatypes , e.g. int, string , ..."""
    __tablename__ = 'bufr_data_type'
    id = Column(Integer, primary_key=True)
    ptype = Column(String, unique=True)

    _xml_vars = ('id', 'ptype')
    _alt_key = 'ptype'

    def __init__(self, ptype=None):
        self.ptype = ptype

    def __repr__(self):
        return "<BUFRDataType('%s','%s')>" % (self.id, self.ptype,)

    def get_dtype(self):
        """ returns python type """
        if self.ptype == 'float': 
            return type(0.0)
        if self.ptype == 'int' : 
            return type(0)
        if self.ptype == 'str' : 
            return type('')
        if self.ptype == 'bool': 
            return type(True)
        return None
	
class BUFRParamType(Base):
    """ Defines parameter types, table: 'bufr_param_type'. These entries
    defines the mappings between BUFR variable names and for instance NetCDF
    variable names """

    __tablename__ = 'bufr_param_type'
    id = Column(Integer, primary_key=True)
    bufr_data_type_id = Column(Integer, ForeignKey('bufr_data_type.id'), )
    name = Column(String, unique=True)
    description = Column(String)
    bufr_data_type = relation( BUFRDataType, backref=backref('bufr_param_type', 
        order_by=id))

    _xml_vars = ('id', 'bufr_data_type_id', 'name', 'bufr_data_type')
    _alt_key = 'name'

    def __init__(self, bufr_data_type=None, name=None, description=None):
        self.bufr_data_type = bufr_data_type
        self.name = name
        self.description = description

    def __repr__(self):
        return "<BUFRParamType('%s','%s','%s')>" % (self.id, 
                self.bufr_data_type_id, self.name)

class BUFRParam(Base):
    """ Holds the data for the corrosponding BUFRParamType , 
    table: 'bufr_param' """

    __tablename__ = 'bufr_param'
    #mapping
    id = Column(Integer, primary_key=True)
    bparam_data = Column(String )
    
    bufr_var_id = Column(Integer, ForeignKey('bufr_var.id'), )
    bufr_param_type_id = Column(Integer, ForeignKey('bufr_param_type.id'), )
    
    bufr_var = relation( BUFRVar , backref=backref('bufr_param', order_by=id))
    bufr_param_type = relation( BUFRParamType , backref=backref('bufr_param', 
        order_by=id))
    
    __table_args__ = ( UniqueConstraint('bufr_var_id', 'bufr_param_type_id', 
        name='distinct_bparam'), {})

    _xml_vars =  ('id', 'bparam_data', 'bufr_var_id', 'bufr_param_type_id',
            'bufr_param_type') 
    _alt_key = 'bparam_data'

    def __init__(self, param_type=None, param_data=None, bufr_var=None):
        self.bufr_var = bufr_var
        self.bufr_param_type = param_type
        self.bparam_data = param_data
    
    def __repr__(self):
        return "<BUFRParam('%s', '%s', '%s', '%s')>" % (self.id, 
                self.bufr_var_id, 
                self.bufr_param_type_id, 
                self.bparam_data )
    
    def get_data(self):
        """ returns the parameter data with the correct python type """
        dtype = self.bufr_param_type.bufr_data_type.get_dtype()
        data = self.bparam_data
        if 'bool' in self.bufr_param_type.bufr_data_type.ptype:
            data = int(data)
        return dtype(data)



#
# Namespace factory 
#

class NmFactory:
    """ Namespace factory, passes namespace to generating xml import """
    def eval(self, string):
        return eval(string)


#
# Main interface to the variable name database
#

class BUFRDescDBConn(SQLXMLMarshall):
    """ Main entry to the variable-name database """

    def __init__(self, dburl='sqlite:///bufr_var_1.db'):
        """Initiates singleton connection class. Creates database if not
        existent"""
        super(BUFRDescDBConn, self).__init__(dburl)

        global Base
        Base.metadata.create_all(self._engine)
        
        # 
        # Create basic data types and parameters 
        #

        try:
            self._session.add( BUFRDataType( 'int' ))
            self._session.add( BUFRDataType( 'float'))
            self._session.add( BUFRDataType( 'str'))
            self._session.add( BUFRDataType( 'bool'))
            self._session.commit()
            
            # Get the new data types 
            str_type = self._session.query( BUFRDataType ).\
                    filter( BUFRDataType.ptype == 'str' ).one()
            float_type = self._session.query( BUFRDataType ).\
                    filter( BUFRDataType.ptype == 'float' ).one()
            bool_type = self._session.query( BUFRDataType ).\
                    filter( BUFRDataType.ptype == 'bool' ).one()
            int_type = self._session.query( BUFRDataType ).\
                    filter( BUFRDataType.ptype == 'int' ).one()
            
            # Add parameter types 
            self._session.add( BUFRParamType(str_type, "bufr_name") )
            self._session.add( BUFRParamType(str_type, "bufr_unit") )
            self._session.add( BUFRParamType(bool_type, "packable_1dim") )
            self._session.add( BUFRParamType(bool_type, "packable_2dim") )
            self._session.add( BUFRParamType(int_type, "max_length") )
            self._session.add( BUFRParamType(str_type, "var_type") )
            self._session.add( BUFRParamType(float_type, "bufr_fill_float") )
            self._session.add( BUFRParamType(int_type, "bufr_fill_int") )
            
            # Default netcdf parameters  
            self._session.add( BUFRParamType(str_type, "netcdf_name") )
            self._session.add( BUFRParamType(str_type, "netcdf_unit") )
            self._session.add( BUFRParamType(str_type, 
                "netcdf_global_attribute") )
            self._session.add( BUFRParamType(str_type, 
                "netcdf_missing_value") )
            self._session.add( BUFRParamType(str_type, 
                "netcdf_add_offset") )
            self._session.add( BUFRParamType(float_type, 
                "netcdf__FillValue") )
            self._session.add( BUFRParamType(str_type, 
                "netcdf_long_name") )
            self._session.add( BUFRParamType(float_type, 
                "netcdf_scale_factor") )
            self._session.add( BUFRParamType(str_type, 
                "netcdf_dimension_name") )
            self._session.add( BUFRParamType(int_type, 
                "netcdf_dimension_length") )

            # Misc parameters
            self._session.add( BUFRParamType(str_type, "bin_file_extension") )
            self._session.add( BUFRParamType(int_type, "bin_file_column") )
            self._session.add( BUFRParamType(int_type, "nasa_team_low_V") )
            self._session.add( BUFRParamType(int_type, "nasa_team_low_H") )
            self._session.add( BUFRParamType(int_type, "nasa_team_high_V") )

            # commit new parameters 
            self._session.commit()

        except IntegrityError:
    	## except Exception:
            self._session.rollback()

    def save(self):
        try:
            self._session.commit()
        except:
            self._session.rollback()
    #
    # General get methods
    #

    def get_parameter_type_names(self):
        """ Returns a list of all available parameter types """
        res = []
        for i in self._session.query(BUFRParamType):
            res.append(i.name)
        return res

    def get_instrument(self, name):
        """ Get instrument entry """
        instr = self._session.query(BUFRDesc).filter(BUFRDesc.name ==
                name).one() 
        return instr
    
    def get_instruments(self):
        """Returns all instruements """
        return  self._session.query(BUFRDesc).all()

    def get_instrument_names(self):
        """ Returns a list of all available parameter types """
        res = []
        for i in self._session.query(BUFRDesc):
            res.append(i.name)
        return res

    def get_instrument_descriptions(self):
        """ Returns a list of all instument descriptions"""
        res = []
        for i in self._session.query(BUFRDesc):
            res.append(i.description)
        return res
   
    def get_variable(self, instr_name=None, var_name=None, 
            var_seq=None, var_id=None):
        """ name is not secure !!! 
        """
        var = None
        if var_id is not None:
            var = self._session.query(BUFRVar).\
                    filter(BUFRVar.id == var_id).one()
        elif var_name is not None:
            var = self._session.query(BUFRVar).join(BUFRDesc).\
                    filter(BUFRDesc.name == instr_name).\
                    filter(BUFRVar.name == var_name).all()[0]
        elif var_seq is not None:
            var = self._session.query(BUFRVar).join(BUFRDesc).\
                    filter(BUFRDesc.name == instr_name).\
                    filter(BUFRVar.seq == var_seq).one()
        return var


    def get_variable_datatypes(self, instr , bvar):
        """ Returns a list of datatype corresponding to 
        instrument, variable combination
        """
        res = []
        var = self._session.query(BUFRVar).join(BUFRDesc).\
                filter( BUFRDesc.id == instr.id and \
                BUFRVar.id == bvar.id ).one()
        for tdata in var.bufr_param:
            res.append(tdata.bufr_param_type.name)
        return res
    
    def get_variable_param(self, instr_name, var_id, ptype_name):
        """ Returns the parameter data for a defined parameter type and
        variable
        """
        param = self._session.query(BUFRParam).join(BUFRVar).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRVar.id == var_id).\
                filter(BUFRParamType.name == ptype_name).one()
        return param

    def get_param_type(self, ptype_name):
        """ Returns the parameter type for a defined parameter type and
        variable"""
        print ptype_name
        param_type = self._session.query(BUFRParamType).\
                filter(BUFRParamType.name == ptype_name).one()
        print "success"
        return param_type

    def get_variable_param_data(self, instr_name, var_name, ptype_name):
        """ Returns the parameter data for a defined parameter type and
        variable
        """
        param = self._session.query(BUFRParam).join(BUFRVar).join(BUFRDesc).\
                join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRVar.name == var_name).\
                filter(BUFRParamType.name == ptype_name).one()
        return param.get_data() 

    def get_instrument(self, instr_name):
        """ Returns a list of variable names corresponding to instrument name
        """
        instr = self._session.query(BUFRDesc).\
                filter(BUFRDesc.name == instr_name).one()
        return instr

    def get_instrument_variables(self, instr_name):
        """ Returns a list of variable names corresponding to instrument name
        """
        res = []
        variables = self._session.query(BUFRVar).join(BUFRDesc).\
                filter(BUFRDesc.name == instr_name)
        for bufr_var in variables:
            res.append(bufr_var.name)
        return res
    
    #
    # More specific get methods for easy access
    #

    def get_netcdf_parameters(self, instr_name, bufr_var_seq ):
        """ Returns a list of netcdf values corresponding to a bufr variable
        name 
        """
        bufr_params = self._session.query(BUFRParam).\
                join(BUFRVar).join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRVar.seq == bufr_var_seq)

        nc_att = {}
        for bufr_param in bufr_params:
            
            if bufr_param.bufr_param_type.name == 'packable_1dim':
                nc_att[p.bufr_param_type.name] = bufr_param.get_data()
                continue
            if bufr_param.bufr_param_type.name == 'packable_2dim':
                nc_att[p.bufr_param_type.name] = bufr_param.get_data()
                continue
            if bufr_param.bufr_param_type.name == 'var_type':
                nc_att[p.bufr_param_type.name] = bufr_param.get_data()
                continue
            
            # default guard
            if not bufr_param.bufr_param_type.name.startswith('netcdf_'): 
                continue

            nc_att[bufr_param.bufr_param_type.name] = bufr_param.get_data()
        return nc_att

    def get_netcdf_global_attrs(self, instr_name):
        """ Returns a dict of bufr-variables/attr-names that should be
        treated like global attributes.
        
        """
        bufr_params = self._session.query(BUFRParam).join(BUFRVar).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRParamType.name == 'netcdf_global_attribute')
        #
        # Search through valid params and return bufr variable names
        # and param values
        #
        res = {} 
        for bufr_param in bufr_params:
            res[bufr_param.bufr_var.name] = bufr_param.get_data()
        return res

    def get_bufr_name(self, instr_name, netcdf_name):
        """ Returns the name of the bufr variable corresponding to netcdf name 
        
        """
        bvar = self._session.query(BUFRVar).join(BUFRParam).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRParamType.name == 'netcdf_name').\
                filter(BUFRParam.bparam_data == netcdf_name).one()
        return bvar.name

    def get_bufr_seq(self, instr_name, netcdf_name):
        """ Returns the name of the bufr variable corresponding to netcdf name 
        
        """
        bvar = self._session.query(BUFRVar).join(BUFRParam).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRParamType.name == 'netcdf_name').\
                filter(BUFRParam.bparam_data == netcdf_name).one()
        return bvar.seq 


    def get_binary_dump_variables(self, instr_name):
        """ Easy access to varables used for binary dumps 
        
        """
        bufr_params = self._session.query(BUFRParam).join(BUFRVar).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRParamType.name == 'bin_file')
        
        file_columns = {} 
        for p in bufr_params:
            bv = p.bufr_var
            bin_column = None
            for bp in bv.bufr_param:
                # we nee to find column number
                if bp.bufr_param_type.name == 'bin_file_column':
                    bin_column = bp.get_data()
            if not bin_column: 
                continue
            if p.get_data() not in file_columns:
                file_columns[p.get_data()] = []
            file_columns[ p.get_data() ].append((bin_column,bv.seq))
        #
        # sort columns before returning
        for key in file_columns.keys():
            file_columns[key].sort()

        return file_columns

    def get_nasa_team_columns(self, instr_name):
        """ returns columns used in nasa team algorithm """
        nasa_low_v = self._session.query(BUFRVar).join(BUFRParam).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRParamType.name == 'nasa_team_low_V').one()

        nasa_low_h = self._session.query(BUFRVar).join(BUFRParam).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRParamType.name == 'nasa_team_low_H').one()

        nasa_high_v = self._session.query(BUFRVar).join(BUFRParam).\
                join(BUFRDesc).join(BUFRParamType).\
                filter(BUFRDesc.name == instr_name).\
                filter(BUFRParamType.name == 'nasa_team_high_V').one()

        return (nasa_low_v.bvar.seq, nasa_low_h.bvar.seq, nasa_high_v.bvar.seq)

    def get_factory(self):
        return NmFactory()

    #
    # insert methods
    #


    def insert_bufr_keys(self, name, fglob, entries, index ):
        """ Inserts and entire list of BUFR file info entries into the database
        """
        str_type = self._session.query( BUFRDataType ).\
                filter( BUFRDataType.ptype == 'str' ).one()
        bufr_unit = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'bufr_unit' ).one()
        netcdf_unit = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'netcdf_unit' ).one()
        netcdf_name = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'netcdf_name' ).one()
        netcdf_dimension_name = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'netcdf_dimension_name' ).one()
        netcdf_dimension_length = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'netcdf_dimension_length' ).one()
        var_type = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'var_type' ).one()
        netcdf__FillValue = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'netcdf__FillValue' ).one()
        packable_1dim = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'packable_1dim' ).one()
        packable_2dim = self._session.query( BUFRParamType ).\
                filter( BUFRParamType.name == 'packable_2dim' ).one()

        # create top level BUFR file object 
        bufr_desc = BUFRDesc(name, fglob)
        bufr_desc.bufr_record_start = index 
        self._session.add(bufr_desc)
        for entry in entries:

            #  
            name = entry.name
            unit = entry.unit
            nc_unit = unit.lower().strip()
            nc_name = bufr.netcdf_compliant_name(name, entry.index)

            bvar = BUFRVar(bufr_desc, name, entry.index)	
            self._session.add(bvar)
            self._session.commit()

            #
            # Add parameters to variable 
            #

            # Add BUFR unit
            param1 = BUFRParam(bufr_unit, unit, bvar) 
            self._session.add(param1)

            # Add netcdf unit
            param2 = BUFRParam( netcdf_unit, nc_unit, bvar) 
            self._session.add(param2)

            # Add netcdf name, guess
            param3 = BUFRParam(netcdf_name, nc_name, bvar) 
            self._session.add(param3)

            # Add dimension name 
            param4 = BUFRParam(netcdf_dimension_name, 
                    entry.dimension_name, bvar) 
            self._session.add(param4)

            # Add dimension length
            param5 = BUFRParam(netcdf_dimension_length, 
                    entry.dimension_length, bvar) 
            self._session.add(param5)

            # Add entry type, long , int, float, ...    
            param6 = BUFRParam(var_type, str(entry.type), bvar) 
            self._session.add(param6)

            # Add Fill value
            if 'int' in str(entry.type) or 'long' in str(entry.type) :
                param7 = BUFRParam(netcdf__FillValue, 
                        entry.fillvalue_int, bvar) 
                self._session.add(param7)
            elif 'float' in str(entry.type):
                param8 = BUFRParam(netcdf__FillValue, 
                        entry.fillvalue_float, bvar) 
                self._session.add(param8)

            # Add packable flag
            param8 = BUFRParam(packable_1dim, entry.packable_1d, bvar) 
            param8 = BUFRParam(packable_2dim, entry.packable_2d, bvar) 
            self._session.add(param8)


            self._session.commit()


