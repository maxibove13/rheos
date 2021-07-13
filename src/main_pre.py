#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to build block grids"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "06/21"

# Built-in modules
import glob
import os

# Local modules
from post.plotgrid import plotgrid, plotshow, mayavi_flower
from pre.checkgrd import readgrd, makegrd, rmfiles


def main_pre():
    # List all cases present in samples
    cases = next(os.walk("./samples/"))[1]

    # Define the casename (the simulation casename)
    while True:
        casename = input("Enter your case name (test case is: duct11): \n")
        # Break loop only if casename is in cases
        if casename in cases:
            break
        # Easter!
        elif casename == "flower":
            mayavi_flower()
            exit(1)
        else:
            print(
                "This case does not exist in /samples folder, please specify a different casename")

    # Define the case dir
    casedir = "./samples/" + casename

    # Generate all the *.grd file using Grid3d.MB (from all the *gin present in case folder)
    makegrd(casename)

    # Look for all grd files inside the case folder
    grd_names = glob.glob(casedir + '/*.grd')

    # Define rgb colors
    rgb = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

    # Extend c length with rgb colors
    c = rgb
    while len(c) < len(grd_names):
        c += rgb

    # Loop through all grid blocks
    for block in range(len(grd_names)):

        # Obtain the cell centers of this block from the .grd file
        xc, yc, zc = readgrd(grd_names[block])

        # Plot this block in 3d with mayavi
        print("Ploting block: ", block+1, "| File: ", grd_names[block])
        plotgrid(grd_names[block], xc, yc, zc, c[block])

    # Display all grid blocks
    plotshow()

    # Clean useless files
    rmfiles(casename)


if __name__ == "__main__":
    main_pre()
