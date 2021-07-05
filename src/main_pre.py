#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to build block grids"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "06/21"

# Build-in modules

# Third-party modules
from mayavi import mlab

# Local modules
from post.plotgrid import *
from pre.checkgrd import readgrd

# Define the filenames containing the grid block information
grid_filename = ["duct1A.grd", "duct1B.grd", "duct1C.grd"]

# Initialize the figure
# plt, ax, c = initplot()

# Define rgb colors
c = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

# Loop through all grid blocks
for block in range(len(grid_filename)):
    # Obtain the cell centers of this block from the .grd file
    xc, yc, zc = readgrd(grid_filename[block])
    # Plot this block
    plotgrid(grid_filename[block], xc, yc, zc, c[block])

# Set aspect ratio to equal
# set_axes_equal(ax)

mlab.show()
