#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to build block grids"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "06/21"

# Build-in modules

# Third-party modules

# Local modules
from pre.checkgrd import checkgrd

grid_filename = "duct1A.grd"

xc, yc, zc = checkgrd(grid_filename)

# Convert the tuples to arrays
# from within np, find array()
# np.array(xc)
# np.array(yc)
# np.array(zc)

print(xc, yc, zc)
