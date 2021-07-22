#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module with functions related to 3d plotting"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Development"
__date__ = "07/21"

# Built-in modules

# Third-party modules
from mayavi.mlab import mesh, show
import numpy as np


def plotgrid(grid_filename, x, y, z, c):
    """Function to plot all grid blocks in a 3D scene using mayavi library"""

    # Loop through the three dimensions.
    for perm in range(3):

        # Permute the coordinates (each axes moves to the next position)
        x = np.moveaxis(x, 0, -1)
        y = np.moveaxis(y, 0, -1)
        z = np.moveaxis(z, 0, -1)

        # Plot x, y, z of the first and last element of the third coord (Top-Bottom, South-North, West-East for each loop)
        mesh(x[:, :, 0], y[:, :, 0], z[:, :, 0],
             representation='wireframe', color=c)
        mesh(x[:, :, -1], y[:, :, -1], z[:, :, -1],
             representation='wireframe', color=c)

        # Avoid blocking the execution
        # plt.pause(0.001)
        # input("Prese [enter] to close the figure.")


def plotshow():
    """Function to display the mayavi scene."""
    show()
