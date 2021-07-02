#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to read binary files containing grid block information"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "06/21"


def checkgrd(grid_filename):

    # Built-in modules

    # Third-party modules
    import matlab.engine
    import numpy as np

    # Local modules

    # start matlab
    eng = matlab.engine.start_matlab()

    # send matlab work folder to pre directory
    eng.cd("src/pre")

    # Call matlab function that reads the .grd binary files and returns the coordinates of the cell centers
    xc, yc, zc = eng.checkGridMBo8o3008(grid_filename, nargout=3)

    # Convert coordinates returned by matlab as tuples to np.arrays
    xc = np.asarray(xc)
    yc = np.asarray(yc)
    zc = np.asarray(zc)

    # return cell centers coordinates
    return xc, yc, zc
