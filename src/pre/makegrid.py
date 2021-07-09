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
