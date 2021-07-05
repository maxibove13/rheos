#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module with functions to plot grids"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "07/21"

# Built-in modules

# Third-party modules
# import matplotlib.pyplot as plt
# from mpl_toolkits import mplot3d
from mayavi import mlab
import numpy as np

# Local modules


def initplot():

    # Create a figure
    fig = plt.figure()

    # Display figure
    # plt.ion()
    # plt.show()

    # Create an axes in 3d
    ax = plt.axes(projection="3d")

    # Colors available
    c = 'rbgcm'

    return plt, ax, c


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

    # Loop through the three dimensions.
    for perm in range(3):

        # Permute the coordinates (each axes moves to the next position)
        x = np.moveaxis(x, 0, -1)
        y = np.moveaxis(y, 0, -1)
        z = np.moveaxis(z, 0, -1)

        # Plot x, y, z of the first and last element of the third coord (T-B, S-N, W-E for each loop)
        # ax.plot_surface(x[:, :, 1], y[:, :, 1],
        #                 z[:, :, 1], color=c, shade=True)
        # ax.plot_surface(x[:, :, -1], y[:, :, -1],
        #                 z[:, :, -1], color=c, shade=True)
        mlab.mesh(x[:, :, 0], y[:, :, 0], z[:, :, 0],
                  representation='wireframe', color=c)
        mlab.mesh(x[:, :, -1], y[:, :, -1], z[:, :, -1],
                  representation='wireframe', color=c)

        # Avoid blocking the execution
        # plt.pause(0.001)
        # input("Prese [enter] to close the figure.")
