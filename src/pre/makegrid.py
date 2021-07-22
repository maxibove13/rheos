#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module with functions to make structured block grids for later use in chaman flow solver"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "07/21"

# Built-in modules
from dataclasses import dataclass
from typing import List
import os
import glob

# Third party modules
from matplotlib import pyplot as plt
import numpy as np

# Local modules
from post.plot2d import ifig, setplot_props


# Define Point, Segment & Edge as dataclasses

# A point is a set of 3 floats
@dataclass
class Point:
    x: float
    y: float
    z: float = 0.0

# A segment consists of a start and an end Point


@dataclass
class Segment:
    start: Point
    end: Point
    ncell: int
    nodes: List[Point]

# An Edge is a list of segments


@dataclass
class Edge:
    name: str
    nlines: int
    segments: List[Segment]

    def __repr__(self):
        return f"{self.name}"


@dataclass
class Block:
    name: str
    edges: List[Edge]


def makesurf(edges: tuple, NICV: int, NJCV: int, block_name: str, uniform_cells: bool = True, IDIR: int = 1, fig_size: tuple = (14, 10)):
    """
    Generate the surface of a grid block interactively

    Parameters
    ----------
    edges : tuple of str
        All the edges of the surface as a tuple of string containing its names. 

    NICV : int
        Number of cells in I direction (horizontal by default, IDIR=1)

    NJCV : int
        Number of cells in J direction (vertical by default, IDIR=1)

    block_name: str
        This block name.

    uniform_cells : bool
        Whether the cells are uniform along all segments. True by default.

    IDIR : int
        Controls the direction of initial 'straight' lines.
        If lines goes South-North: IDIR=0
        If lines goes West-East: IDIR=0

    fig_size : tuple of int
        Figure size as a tuple of ints (represents pixels)

    Returns
    -------
    block: Block
``

    """

    # Initialize figure and axes
    fig, ax = plt.subplots(figsize=fig_size)

    # Loop through edges
    for edge in edges:
        # Number of segments needed to describe this edge.
        print(f"Number of {edge} edge segments:")
        while True:
            try:
                nlines = int(input("NLINES: "))
                if nlines > 0:
                    break
                else:
                    print("Only positive integers values are valid. Try again")
            except:
                print("Only postive integers values are valid. Try again")

        # Initialize this edge segments list
        segments = []

        # Loop through all segments of this edge
        for seg in range(0, nlines):

            # Coordinates (x,y) of the first point in this segment of this edge. (South-West point if first (or only) segment)
            print(
                f"Coordinates (x,y) of first point in segment {seg + 1} of {edge} edge")
            # Ask user for point and plot it.
            if nlines == 1:
                if edge == "West":
                    x0, y0 = south.segments[0].start.x, south.segments[0].start.y
                elif edge == "East":
                    x0, y0 = south.segments[0].end.x, south.segments[0].end.y
                    print(x0, y0)
                else:
                    # Ask user for coordinates of segment start point only if it is the first one
                    if seg == 0:
                        x0, y0 = input_point(ax)
                    # Otherwise, the endpoint of the previous segment must be the start of this one
                    else:
                        x0, y0 = x1, y1
            else:
                # Ask user for coordinates of segment start point only if it is the first one
                if seg == 0:
                    x0, y0 = input_point(ax)
                # Otherwise, the endpoint of the previous segment must be the start of this one
                else:
                    x0, y0 = x1, y1

            # Label the point if known
            if nlines == 1:
                if edge == "South":
                    ax.text(x0, y0, "SW", horizontalalignment="right")
                elif edge == "North":
                    ax.text(x0, y0, "NW", horizontalalignment="right")

            # Call function to set some plot properties.
            # Set xlabel, ylabel, title.
            setplot_props(
                ax, "x", "y", f"Bottom 2D section of block {block_name}")
            # Display figure
            ifig(fig)

            # Coordinates (x,y) of this segment endpoint. (South-East point if first (or only) segment)
            print(
                f"Coordinates (x,y) of endpoint in segment {seg + 1} of {edge} edge")
            # Ask user for point and plot it.
            if nlines == 1:
                if edge == "West":
                    x1, y1 = north.segments[0].start.x, north.segments[0].start.y
                elif edge == "East":
                    x1, y1 = north.segments[0].end.x, north.segments[0].end.y
                    print(x1, y1)
                else:
                    x1, y1 = input_point(ax)
            else:
                x1, y1 = input_point(ax)

            # Label this point if known
            if nlines == 1:
                if edge == "South":
                    ax.text(x1, y1, "SE", horizontalalignment="left")
                elif edge == "North":
                    ax.text(x1, y1, "NE", horizontalalignment="left")

            # Plot the segment as a line
            ax.plot([x0, x1], [y0, y1], 'k')

            # Distribution of cells along this segment (DX1, EXP):
            if not uniform_cells:
                # specify first cell size (dx1) or expansion (contraction) factor
                dx1, exp = cells_distro()
            else:
                # uniform cells
                dx1, exp = 0, 0

            # Number of cells of this segment
            # If this edge has only one segment, nseg is equal to NICV or NJCV
            ncell = seg_cells(nlines, IDIR, NICV, NJCV, edge)

            # Get coordinates of this segment nodes
            x_cells = np.linspace(x0, x1, ncell)
            y_cells = np.linspace(y0, y1, ncell)

            # Create a nodes list
            nodes = []
            for node in range(0, ncell):
                nodes.append(Point(x_cells[node], y_cells[node]))

            # Create a segment object
            seg = Segment(Point(x0, y0), Point(x1, y1), ncell, nodes)

            # Append it to this edge segments list
            segments.append(seg)

            # If the segment is <45° in a cartesian grid, represent the nodes as a vertical line, otherwise as a horizontal line.
            if (seg.end.x - seg.start.x) >= (seg.end.y - seg.start.y):
                marker = '|'
            else:
                marker = '_'
            # Plot the nodes
            ax.plot(x_cells, y_cells, 'k' + marker)

            # Update figure
            ifig(fig)

        # Instantiate this edge
        if edge == "South":
            south = Edge(edge, nlines, segments)

        elif edge == "North":
            north = Edge(edge, nlines, segments)

            # Link the nodes
            for node in range(north.segments[0].ncell):
                a = south.segments[0].nodes[node]
                b = north.segments[0].nodes[node]
                ax.plot([a.x, b.x], [a.y, b.y], 'k-', linewidth=0.3)

        elif edge == "West":
            west = Edge(edge, nlines, segments)

        elif edge == "East":
            east = Edge(edge, nlines, segments)

            # Link the nodes
            for node in range(east.segments[0].ncell):
                a = west.segments[0].nodes[node]
                b = east.segments[0].nodes[node]
                ax.plot([a.x, b.x], [a.y, b.y], 'k-', linewidth=0.3)

        # Update figure
        ifig(fig)

    # Return a block object with this edges.
    return Block(block_name, [south, north, west, east])


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


def seg_cells(nlines, IDIR, NICV, NJCV, edge):
    """Return the number of cells of this segment"""

    # If only one segment exists in this edge, the number of cells of this segment is equal to NICV or NJCV
    if nlines == 1:
        if IDIR == 1:
            if edge == "South" or edge == "North":
                seg_cells = NICV
            else:
                seg_cells = NJCV
        # If straight lines goes West-East, seg_cells equals to NJCV
        else:
            if edge == "South" or edge == "North":
                seg_cells = NJCV
            else:
                seg_cells = NICV
    else:
        print("Number of cells of this segment?")
        while True:
            try:
                seg_cells = int(input("seg_cells: "))
                break
            except:
                print("Only positive integers are accepted. Try again.")

    return seg_cells


def input_point(ax):
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
    ax.plot(x, y, 'ro')

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
