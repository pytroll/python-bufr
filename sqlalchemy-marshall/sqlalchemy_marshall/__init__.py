#!/usr/bin/env python
#
# sqlalchemy-marshall
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

#
# Author Kristian Rune Larsen <krl@dmi.dk>

""" Provides base module for an XML serializable SQLAlchemy database """

from sqlalchemy import *
from sqlalchemy.orm import relation, backref, sessionmaker
from sqlalchemy.orm.collections import InstrumentedList
from sqlalchemy.orm.exc import *
from sqlalchemy.exceptions import *
import types
import sys
import os

#
# Database handler is wrapped as a Singleton instance 
#

class Query:
    """ wrapper class for query """
    def __init__(self,):
        self.elements = []
    def __iter__(self,):
        return iter(self.elements)
    def append(self, element):
        self.elements.append(element)

class Singleton(type):
    def __init__(cls, name, bases, dict):
        super(Singleton, cls).__init__(name, bases, dict)
        cls.instance = None

    def __call__(cls,*args,**kw):
        if cls.instance is None:
            cls.instance = super(Singleton, cls).__call__(*args, **kw)
        return cls.instance

class SQLXMLMarshall:
    __metaclass__ = Singleton
    """ Singleton for connecting to BUFR variable database """
    def __init__(self, dburl='sqlite:///bufr_var_1.db' ):
        """ Initiates connection to database """
        self._dburl = dburl
        self._engine = create_engine(self._dburl, echo=False)
        self._Session=sessionmaker(bind=self._engine)
        self._session = self._Session()

    def get_factory():
        return None

    def update_object(self, obj):
        """ inserts or updates an object in the database """

        def assign_attrs(xobj, dobj, update=False ):
            """ transfer simple attribute values from xml object (xobj) to
            database object (dobj) 
            
            """
            try:
                for t in xobj._xml_vars:
                    
                    # if we're updating the object we don't need to
                    # worry about the id 
                    if update:
                        if t == 'id': continue

                    at_obj = getattr(xobj,t)
                    try:
                        at_obj.__tablename__
                        continue
                    except AttributeError:
                        pass
                    
                    # Do not update empty values 
                    if at_obj is None or at_obj == "None": 
                        continue

                    if type(at_obj) == types.StringType or type(at_obj) == unicode:
                        print "setting string type %s %s %s  " % (dobj,t,at_obj)    
                        
                        setattr( dobj, t, at_obj )
                        continue
                    try:
                        iter(at_obj)
                        continue
                    except TypeError:
                        pass
                    print "Setting %s %s %s" % (dobj, t, at_obj)
                    setattr( dobj, t, at_obj )
            except AttributeError, e:
                print "Attibute Error %s " % e
                pass
            return dobj 
        
        try:
            #
            # the object has an assigned id, we need to update it
            #
            if type(obj.id) == int:
                try:
                    dobj = self._session.query(obj.__class__).filter(obj.__class__.id == obj.id).one()
                    dobj = assign_attrs(obj, dobj)
                    self._session.update(dobj, update=True )
                    self._session.commit()
                    return dobj.id
                except Exception, e:
                    self._session.rollback()
        except AttributeError:
            pass
        
        #
        # Do database insertion 
        #

        # create a new object using a shallow copy constructor
        # Only looks at the variables defined in _xml_vars list
        dobj = obj.__class__()
        dobj = assign_attrs(obj, dobj)
        
        try:
            dobj.id = None 
            self._session.add(dobj)
            self._session.commit()
        except IntegrityError, e:
            """ Try to find matching object by name """
            self._session.rollback()
            dobj = self._session.query(obj.__class__).filter( getattr(obj.__class__,obj._alt_key ) == getattr(obj,obj._alt_key)).one()
            dobj = assign_attrs(obj, dobj, True)
            try:
                self._session.update(dobj)
                self._session.commit()
                return dobj.id
            except IntegrityError, e: 
                self._session.rollback()
                return None
        except Exception, e:
            self._session.rollback()
            return None

        return dobj.id

    def _update_from_xml(self,obj, calling_obj=None):
        """ generate database entries from xml file """
        # update or insert current object dependencies, looks for 'tablename' attribute
        #
        # Only try to insert database objects
        #
        try:
            obj.__tablename__
        except AttributeError:
            return None

        #
        # Test for upwards dependencies 
        #
        try:
            for var in obj.__dict__:
                try:
                    # test for attribute 'tablename'
                    xobj = getattr(obj,var)
                    table = xobj.__tablename__
                    table_id = "%s_id" % var
                    print "Examening table %s id: %s  obj %s " % (table, table_id, obj)
                    if table_id in obj._xml_vars:
                        nobj = self._update_from_xml( xobj, obj)
                        setattr(obj,"%s_id" % var,nobj)
                except AttributeError:
                    pass
        except AttributeError:
            return None

        #
        # update or insert current object into database, if possible
        #
       
        nobj_id = self.update_object(obj)
        
        #
        # If we are sorting out upwards dependencies we don't look down 
        # ... that would course infinite recursion
        #

        if calling_obj is not None: 
            return nobj_id

        #
        # update or insert dependent objects, these can have changed
        #
        try:
            for var in obj._xml_vars:
                cobj = getattr(obj,var)
                if type(cobj) == types.StringType or type(cobj) == unicode:
                    continue
                try:
                    it = iter(cobj)
                    for elem in it:
                        try:
                            self._update_from_xml(elem,None)
                        except Exception, e:
                            pass
                except TypeError:
                    pass
        except AttributeError:
            pass
       
        return nobj_id
        
    def import_xml(self, xml_file):
        """ Update database from an xml file """
        import xml.etree.ElementTree as ET
        from xml_marshall import loads
        etree = ET.parse(xml_file)
        factory = self.get_factory()
        root_query = loads(etree.getroot(), factory)
        try:
            for root_object in root_query:
                self._update_from_xml(root_object)
        except TypeError, e:
            # handle dumps not enclosed in Query tags
            self._update_from_xml(root_query)
 
    def dump_xml(self, root, name=None, filename=sys.stdout):
        """ Prints a specific entry to a XML string """
        import xml.etree.ElementTree as ET
        from xml_marshall import dumps
        if name is not None:
            obj = self._session.query(root).filter(getattr(root,'name') == name).one()
        else: 
            obj = self._session.query(root)
        root_node = dumps(obj)
        tree = ET.ElementTree(root_node)
        tree.write(filename)


