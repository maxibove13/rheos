#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to manage fields"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Development"
__date__ = "07/21"

# Third-party modules
import matlab.engine
import numpy as np


def get_fields(casename, regions, timestep, grid_flag=True, Lreadstat=False):
    """
    Function to import fields from caffa3dMBRi_gz_15_0001 script that reads fields in binary form from *rgc* files
    Currently only importing cell centers Xc from GR field.
    """

    # Define the path to look for rgc files
    path = f"./samples/{casename}/rgc"

    # start matlab
    eng = matlab.engine.start_matlab()

    eng.addpath("/home/max/Documents/FING/rheos/src/post")

    gr = []
    # Loop through all regions
    for r in regions:
        # Call matlab function that gets fields from rgc binary files (Get fields per region)
        GR = eng.caffa3dMBRi_gz_15_0002(
            str(path + "/"),
            str(casename),
            r,
            matlab.int8([]),
            int(timestep),
            bool(grid_flag),
            list([]),
            Lreadstat,
            nargout=1,
        )
        # GR matlab struct is returned as a dict, the value of the key 'Xc' are the cell centers in matlab.double() class form.
        # Convert that object to np.array and append it to the list of regions cell centers.
        gr.append(np.asarray(GR["Xc"]))

    return gr
