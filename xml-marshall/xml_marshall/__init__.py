#!/usr/bin/env python
#
#
# xml-marshall
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
# attempt to write a small generic XML-serialisation/deserialisation module in python
# 
# This is not ment to preform well
#
# Author Kristian Rune Larsen <krl@dmi.dk>


""" XML serialisation / deserialisation """

__revision__ = 0.1

import xml.etree.ElementTree as ET
import types
import sys

def dumps( obj, name=None ):
    """ serialize elementtree by recursivly scanning for known types """
    
    if name:
        root_node = ET.Element(obj.__class__.__name__ , name=name)
    else:
        root_node = ET.Element(obj.__class__.__name__ )
    
    try:
        # complex types, _xml_vars interface
        attrs = obj._xml_vars
        for att in attrs:
            att_obj = getattr(obj,att)
            root_node.append(dumps(att_obj,name=att))
        return root_node 
    except AttributeError:
        pass
    
    #
    # Dict instance
    #
    
    if type(obj) == types.DictType:
        for key,value in obj.iteritems():
            item_node = ET.SubElement(root_node, 'item') 
            item_node.append(dumps(key))
            item_node.append(dumps(value))
        return root_node
    #   
    # Iterator instance
    #  
    if type(obj) != types.StringType and type(obj) != unicode:
        try:
            it = iter(obj)
            for elm in obj:
                root_node.append(dumps(elm))
            return root_node 
        except TypeError:
            pass
        #
        # class instance 
        #   
        
        try:
            # complex types
            attrs = [att for att in obj.__dict__ ]
            if '_xml_vars' in attrs:
                attrs = obj._xml_vars
            for att in attrs:
                att_obj = getattr(obj,att)
                root_node.append(dumps(att_obj,name=att))
            return root_node 
        except AttributeError:
            pass

   
    # simple types
    root_node.text = str(obj)
    return root_node






def loads( root_node, factory):
    """ deserialize elementtree by recursivly scanning for known types """
    if root_node.tag == 'NoneType':
        return None
    root_obj = factory.eval(root_node.tag)()
    #
    # Instances which support _xml_vars interface
    #
    try:
        for att in root_obj._xml_vars:
            for obj_elem in root_node.getchildren():
                if 'name' in obj_elem.attrib and obj_elem.attrib['name'] == att:
                    value = loads(obj_elem, factory)
                    setattr(root_obj, att, value)
        return root_obj
    except AttributeError:
        pass
   
    #
    # Dict
    #

    if type(root_obj) == types.DictType:
        for dict_elem in root_node.getchildren():
            key_elem = dict_elem.getchildren()[0]
            value_elem = dict_elem.getchildren()[1]
            key_obj = factory.eval(key_elem.tag)(key_elem.text)
            value_obj = loads(value_elem, factory)
            root_obj[key_obj]=value_obj
        return root_obj
    #
    # Iterable containers, but not strings or unicode
    #

    if type(root_obj) != types.StringType and type(root_obj) != unicode:
        try:
            it = iter(root_obj)
            for elm in root_node.getchildren():
                value = loads(elm, factory)
                root_obj.append(value)
            return root_obj 
        except TypeError:
            pass
    #
    # Instances , user defined classes
    #
    
    try:
        attrs = [att for att in root_obj.__dict__ ]
        for att in attrs:
             for obj_elem in root_node.getchildren():
                 if 'name' in obj_elem.attrib and obj_elem.attrib['name'] == att:
                     setattr(root_obj, att, loads(obj_elemi,factory))
        return root_obj
    except AttributeError:
        pass

    #
    # Standard python types
    #
    root_obj = type(root_obj)(root_node.text)
    return root_obj

if __name__ == '__main__':
    class A:
        def __init__(self, a=None):
            self.a = a
            self.b = [range(2), range(3), range(5)]
            self.c = {'a':1, 'b':2, 'c': 3 }
            self.d = "this is a test"
            self.e = """ this is a
            long text """
        def get_a(self):
            return self.a
    class B:
        def __init__(self, a=None):
            self.a = a

    class C(object):
        def __init__(self, c=None):
            self.c = c

    class NmFactory():
        """ factory for parsing objects to different namespaces"""
        def eval(self, string):
            return eval(string)
    
    a = A(5)
    b = B(a)
    rn = dumps(b)
    tree = ET.ElementTree(rn)
    tree.write('/tmp/test.xml')
    tree = ET.parse('/tmp/test.xml')
    
    factory = NmFactory()

    c = loads(tree.getroot(), factory)
    rn1 = dumps(c)
    tree1 = ET.ElementTree(rn1)
    tree.write('/tmp/test1.xml')
    print c.a.b
    print b.a.b
    print c.a.c
    print b.a.c
    print c.a.a
    print b.a.a
    print c.a.e
    
    new_c = C(8)
    rn = dumps(new_c)
    tree = ET.ElementTree(rn)
    tree.write('/tmp/test.xml')
    tree = ET.parse('/tmp/test.xml')
    rc = loads(tree.getroot())
    print rc.c

    # new style classes

    
    from varmaps import *
    from sqlalchemy.orm.collections import InstrumentedList
    conn = BUFRDescDBConn()
    r = conn._session.query(BUFRDesc).first()
    print r
    rn = dumps(r)
    tree = ET.ElementTree(rn)
    tree.write('/tmp/dbdump.xml')
    tree = ET.parse('/tmp/dbdump.xml')
    rm = loads(tree.getroot())

