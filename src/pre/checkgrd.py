#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to read binary files containing grid block information"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "06/21"


def checkgrd(grid_filename):
    # import matlab engine
    import matlab.engine

    # start matlab
    eng = matlab.engine.start_matlab()

    # send matlab work folder to pre directory
    eng.cd("pre")

    # Call matlab function that reads the .grd binary files and returns the coordinates of the cell centers
    xc, yc, zc = eng.checkGridMBo8o3008(grid_filename, nargout=3)

    # return cell centers coordinates
    return xc, yc, zc
