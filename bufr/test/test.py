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

import os
import sys
import unittest

import bufr

test_dir = os.path.dirname(os.path.abspath(__file__))

class TestBUFRFile(unittest.TestCase):

    def setUp(self):
        os.environ['BUFR_TABLES'] =  test_dir + "/bufrtables/"
        os.environ['PRINT_TABLE_NAMES'] = "false"

    def test_bufr_read(self):
        """Test reading data and data quality on Metop-A MHS BUFR file"""

        test_file = os.path.join(test_dir, "metop_mhs.bufr")
        bfr = bufr.BUFRFile(test_file)
        data = bfr.next()
        self.assertEqual(data[0].name.strip(' '), "TOVS/ATOVS PRODUCT QUALIFIER")
        self.assertEqual(data[0].data[0], 3) 
        self.assertEqual(data[0].unit.strip(' '), "CODE TABLE 8070")
        self.assertEqual(data[0].index, 0)

    def test_bufr_interate(self):
        """Test reading all data, last entry"""

        test_file = os.path.join(test_dir, "metop_mhs.bufr")
        bfr = bufr.BUFRFile(test_file)
        for i,data in enumerate(bfr):
            pass
        self.assertEqual(i, 3)
        self.assertEqual(data[0].name.strip(' '), "TOVS/ATOVS PRODUCT QUALIFIER")
        self.assertEqual(data[0].data[0], 3) 
        self.assertEqual(data[0].unit.strip(' '), "CODE TABLE 8070")
        self.assertEqual(data[0].index, 0)
            
if __name__ == '__main__':
    unittest.main()

