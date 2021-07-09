#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module with functions to plot grids"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "07/21"

# Third-party modules
from mayavi import mlab
import numpy as np


def initplot():
    """Function to initialize a figure. Currently deprecated, using mayavi library."""

    # Create a figure
    fig = plt.figure()

    # Display figure
    plt.ion()
    plt.show()

    # Create an axes in 3d
    ax = plt.axes(projection="3d")

    # Colors available
    c = 'rbgcm'

    return plt, ax, c


# Not in use currently as I am using mayavi and scales the coordinates by default.
# I am not the author of this function.
def set_axes_equal(ax):
    '''Make axes of 3D plot have equal scale so that spheres appear as spheres,
    cubes as cubes, etc..  This is one possible solution to Matplotlib's
    ax.set_aspect('equal') and ax.axis('equal') not working for 3D.

    Input
      ax: a matplotlib axis, e.g., as output from plt.gca().
    '''

    x_limits = ax.get_xlim3d()
    y_limits = ax.get_ylim3d()
    z_limits = ax.get_zlim3d()

    x_range = abs(x_limits[1] - x_limits[0])
    x_middle = np.mean(x_limits)
    y_range = abs(y_limits[1] - y_limits[0])
    y_middle = np.mean(y_limits)
    z_range = abs(z_limits[1] - z_limits[0])
    z_middle = np.mean(z_limits)

    # The plot bounding box is a sphere in the sense of the infinity
    # norm, hence I call half the max range the plot radius.
    plot_radius = 0.5*max([x_range, y_range, z_range])

    ax.set_xlim3d([x_middle - plot_radius, x_middle + plot_radius])
    ax.set_ylim3d([y_middle - plot_radius, y_middle + plot_radius])
    ax.set_zlim3d([0, z_middle + plot_radius])


def plotgrid(grid_filename, x, y, z, c):
    """Function to plot all grid blocks in a 3D scene using mayavi library"""

    # Loop through the three dimensions.
    for perm in range(3):

        # Permute the coordinates (each axes moves to the next position)
        x = np.moveaxis(x, 0, -1)
        y = np.moveaxis(y, 0, -1)
        z = np.moveaxis(z, 0, -1)

        # Plot x, y, z of the first and last element of the third coord (Top-Bottom, South-North, West-East for each loop)
        mlab.mesh(x[:, :, 0], y[:, :, 0], z[:, :, 0],
                  representation='wireframe', color=c)
        mlab.mesh(x[:, :, -1], y[:, :, -1], z[:, :, -1],
                  representation='wireframe', color=c)

        # Avoid blocking the execution
        # plt.pause(0.001)
        # input("Prese [enter] to close the figure.")


def plotshow():
    """Function to display the mayavi scene."""
    mlab.show()
