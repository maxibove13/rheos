#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module containing functions and classes to manage files"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Development"
__date__ = "07/21"

# Built-in modules
import os


def rmfiles(casename):
    """Function to remove useless files at the end of the execution"""

    # Go to samples/case folder
    os.chdir("./samples/" + casename)

    # Remove *.grd and *.out files
    os.system("rm *.grd")
    os.system("rm *.out")