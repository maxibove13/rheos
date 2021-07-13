#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module with functions to make structured block grids for later use in chaman flow solver"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "07/21"

# Built-in modules
import os
import glob

# Third party modules
from matplotlib import pyplot as plt


def makegrd(casename):
    """Function to execute grid3d.lnx fortran written caffa3d grid generator with a given *.gin file passed as an argument"""

    # Go to the case dir
    os.chdir("./samples/" + casename)

    # Look for all gin files inside the case folder
    gin_names = glob.glob('*.gin')

    # Loop through all gin files
    for i in range(len(gin_names)):

        # Extract the block grid name from the full filename (remove the .gin extension)
        gin_names[i] = gin_names[i][:-4]

        # Full name of grid3d program to execute
        grid3d_filename = '../../src/pre/grid3d.lnx'

        # Pass the name of the gin file to the program and execute it
        os.system("echo " + gin_names[i] + " | " +
                  grid3d_filename + " /dev/stdin" + ">" "grid3d.out")

    # go back to root dir
    os.chdir("../../")


def rmfiles(casename):
    """Function to remove useless files at the end of the execution"""

    # Go to samples/case folder
    os.chdir("./samples/" + casename)

    # Remove *.grd and *.out files
    os.system("rm *.grd")
    os.system("rm *.out")


def seg_cells(nlines, idir, nicv, njcv, edge):
    """Return the number of cells of this segment"""

    # If only one segment exists in this edge, the number of cells of this segment is equal to NICV or NJCV
    if nlines == 1:
        if idir == 1:
            if edge == "South" or edge == "North":
                seg_cells = nicv
            else:
                seg_cells = njcv
        # If straight lines goes West-East, seg_cells equals to njcv
        else:
            if edge == "South" or edge == "North":
                seg_cells = njcv
            else:
                seg_cells = nicv
    else:
        print("Number of cells of this segment?")
        while True:
            try:
                seg_cells = int(input("seg_cells: "))
                break
            except:
                print("Only positive integers are accepted. Try again.")

    return seg_cells


def input_point():
    """Ask user for (x,y) coordinates of a point and plot it."""

    # Ask user for (x,y) coordinates of the point.
    while True:
        try:
            x = float(input("x: "))
            y = float(input("y: "))
            break
        except:
            print("x and y must be numeric. Try again.")

    # Plot the point
    fig = plt.plot(x, y, 'ro')

    return x, y


def cells_distro():
    """Ask user to input the cell distribution"""

    # Distribution of cells along this segment (DX1 or EXP):
    # DX1 (if non-zero) is the size of the first cell in this segment.
    # The rest of the cells will be expanded or contracted with a factor k,
    # computed so that the seg_cells cells fill the value.
    while True:
        print(
            "Size of first cell in this segment. (0 for uniform cells distribution)")
        try:
            dx1 = float(input("DX1: "))
            if dx1 > 0:
                break
            else:
                print("Only positive float values are valid. Try again.")
        except:
            print("Only float values are valid. Try again.")

    # EXP stands for the expansion (or contraction) factor.
    if dx1 == 0:
        while True:
            print(
                "Expansion (or contraction) factor. (0 for uniform cells distribution")
            try:
                exp = float(input("EXP: "))
                break
            except:
                print("Only positive float values are valid. Try again.")
    # If dx1 is non-zero exp must be 0.
    else:
        exp = 0

    return dx1, exp
