#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to process caffa3d simulation results"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "07/21"

# Built-in modules
import glob
import os

# Local modules
from post.fields import get_fields
from post.plot3d import plotgrid, plotshow

# Define the postprocessing settings variables and constants.
# We should move this to its own file and use some library like yaml.
server = "melete"
sample_path = "/home/mbove/chaman/samples"
regions = [1, 2]
timestep = 1


def main_post():

    # Define the casename (the simulation casename)
    casename = input("Enter your case name: \n")

    # List all cases present in ./samples
    present_cases = next(os.walk("./samples/"))[1]

    # If casename folder does not existe, create it.
    if casename not in present_cases:
        os.system(f"mkdir ./samples/{casename}")
    # List all folders present in samples/casename
    case_subfolders = next(os.walk(f"./samples/{casename}"))[1]
    # If "rgc" doesn't exist create it
    if "rgc" not in case_subfolders:
        os.system(f"mkdir ./samples/{casename}/rgc")

    # Download the *rgc* simulation outputs files from the selected dir
    os.system(
        f"rsync -zv {server}:{sample_path}/{casename}/*rgc* ./samples/{casename}/rgc"
    )

    # Get fields (currently only GR.Xc info)
    gr = get_fields(casename, regions, timestep)

    # Define rgb colors
    rgb = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

    for r in regions:
        # This region grid
        grd_xc = gr[r - 1]

        plotgrid(
            grd_xc[:, :, :, 0],
            grd_xc[:, :, :, 1],
            grd_xc[:, :, :, 2],
            rgb[r - 1],
            op=0.6,
        )

    # Show scene (figure)
    plotshow()


if __name__ == "__main__":
    main_post()
