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
from matplotlib.patches import Arc
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

# A segment consists of a start, an end Point, a boolean specifying if it's an arc (only half circunference), the number of nodes and the list of nodes.


@dataclass
class Segment:
    start: Point
    end: Point
    arc: bool
    nnodes: int
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

    # Is there any arc?
    arc_in_edge = False

    # Loop through edges
    for edge in edges:
        # Ask for number of segments needed to describe this edge.
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

            # Number of cells of this segment
            # If this edge has only one segment, nseg is equal to NICV or NJCV
            nnodes = seg_cells(nlines, IDIR, NICV, NJCV, edge)

            # Coordinates (x,y) of the first point in this segment of this edge. (South-West point if first (or only) segment)
            print(
                f"Coordinates (x,y) of first point in segment {seg + 1} of {edge} edge")
            # Ask user for point and plot it.
            if nlines == 1 and not arc_in_edge:
                if edge == "West":
                    x1, y1 = south.segments[0].start.x, south.segments[0].start.y
                elif edge == "East":
                    x1, y1 = south.segments[0].end.x, south.segments[0].end.y
                    print(x1, y1)
                else:
                    # Ask user for coordinates of segment start point only if it is the first one
                    if seg == 0:
                        x1, y1 = input_point(ax)
                    # Otherwise, the endpoint of the previous segment must be the start of this one
                    else:
                        x1, y1 = x2, y2
            else:
                # Ask user for coordinates of segment start point only if it is the first one
                if seg == 0:
                    x1, y1 = input_point(ax)
                # Otherwise, the endpoint of the previous segment must be the start of this one
                else:
                    x1, y1 = x2, y2

            # Label the point if known
            if nlines == 1 and not arc_in_edge:
                if edge == "South":
                    ax.text(x1, y1, "SW", horizontalalignment="right")
                elif edge == "North":
                    ax.text(x1, y1, "NW", horizontalalignment="right")

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
            if nlines == 1 and not arc_in_edge:
                if edge == "West":
                    x2, y2 = north.segments[0].start.x, north.segments[0].start.y
                elif edge == "East":
                    x2, y2 = north.segments[0].end.x, north.segments[0].end.y
                    print(x2, y2)
                else:
                    x2, y2 = input_point(ax)
            else:
                x2, y2 = input_point(ax)

            # Create points objects
            p1 = Point(x1, y1)
            p2 = Point(x2, y2)

            # Label this point if known
            if nlines == 1:
                if edge == "South":
                    ax.text(x2, y2, "SE", horizontalalignment="left")
                elif edge == "North":
                    ax.text(x2, y2, "NE", horizontalalignment="left")

            # Ask user if segment is a straight line or arc (half circumference only)
            while True:
                is_arc = input("Is this segment an arc? (half circumference only) y/n")
                if is_arc == "y":
                    arc = True
                    arc_in_edge = True
                    while True:
                        try:
                            arc_dir = int(input("Specify the direction of the arc (-90 or 90 degrees)"))
                            print(arc_dir)
                            if arc_dir == 90 or arc_dir == -90:
                                break
                            else:
                                print("Only 90 or -90 int values accepted. Try again.")
                        except:
                            print("Only 90 or -90 int values accepted. Try again.")
                    break
                elif is_arc == "n":
                    arc = False
                    break
                else:
                    print("Only y/n answers. Try again.")


            # Plot the segment as a line or as an arc
            if arc:
                p_m_arc, nodes = build_arc(p1, p2, arc_dir, ax, nnodes)
            else:
                ax.plot([x1, x2], [y1, y2], 'k')

            # Distribution of cells along this segment (DX1, EXP):
            if not uniform_cells:
                # specify first cell size (dx1) or expansion (contraction) factor
                dx1, exp = cells_distro()
            else:
                # uniform cells
                dx1, exp = 0, 0

            # Get nodes if segment is a straight line
            # If segment is an arc nodes are already defined
            if not arc:
                # Get coordinates of this segment nodes
                x_cells = np.linspace(x1, x2, nnodes)
                y_cells = np.linspace(y1, y2, nnodes)

                # Create a nodes list
                nodes = []
                for node in range(0, nnodes):
                    nodes.append(Point(x_cells[node], y_cells[node]))

            # Create a segment object
            seg = Segment(p1, p2, arc, nnodes, nodes)

            # Append it to this edge segments list
            segments.append(seg)

            # If the segment is <45° in a cartesian grid, represent the nodes as a vertical line, otherwise as a horizontal line.
            if (seg.end.x - seg.start.x) >= (seg.end.y - seg.start.y):
                marker = '|'
            else:
                marker = '_'
            # Plot the nodes
            for node in range(0, nnodes):
                ax.plot(seg.nodes[node].x, seg.nodes[node].y, 'k' + marker)

            # Update figure
            ifig(fig)

        # Instantiate this edge
        if edge == "South":
            south = Edge(edge, nlines, segments)

        elif edge == "North":
            north = Edge(edge, nlines, segments)

            # Link the nodes visually (plot the mesh)
            if arc_in_edge:
                while True:
                    as_arc = input("Link segment nodes as an arc? y/n")
                    if as_arc == "y":
                        for node in range(north.segments[0].nnodes):
                            a = south.segments[0].nodes[node]
                            b = north.segments[0].nodes[nnodes-1-node]                        
                            _, _ = build_arc(a, b, arc_dir, ax, nnodes, build_nodes=False)
                        break
                    elif as_arc == "n":
                        for node in range(north.segments[0].nnodes):
                            a = south.segments[0].nodes[node]
                            b = north.segments[0].nodes[node]
                            ax.plot([a.x, b.x], [a.y, b.y], 'k-', linewidth=0.3)
                        break

        elif edge == "West":
            west = Edge(edge, nlines, segments)

        elif edge == "East":
            east = Edge(edge, nlines, segments)

            # Link the nodes


            if arc_in_edge:
                while True:
                    as_arc = input("Link segment nodes as an arc? y/n")
                    if as_arc == "y":
                        for node in range(east.segments[0].nnodes):
                            a = west.segments[0].nodes[node]
                            b = east.segments[0].nodes[nnodes-1-node] 
                            _, _ = build_arc(a, b, arc_dir, ax, nnodes, build_nodes=False)
                        break
                    elif as_arc == "n":
                        for node in range(north.segments[0].nnodes):
                            a = west.segments[0].nodes[node]
                            b = east.segments[0].nodes[node]
                            ax.plot([a.x, b.x], [a.y, b.y], 'k-', linewidth=0.3)
                        break

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

def build_arc(p1, p2, arc_dir, ax, n, build_nodes=True):
    """
    Find the arc (half-circumference) given two points in a 2D plane. Here we need to define the angle to draw the arc and its middle point to generate the *.grd file.

    Parameters
    ----------
    p1 : Point
        First point to construct the arc 

    p2 : Point
        Second point to construct the arc

    arc_dir : int
        Specifiy the direction of the arc (-90 or 90 degrees)

    ax: matplotlib.axes._subplots.AxesSubplot
        Ax of current figure being displayed

    n: int
        Number of nodes of segment

    Returns
    -------
    p_m_arc: Point
        Arc middle point. Needed in *.gin file

    nodes: List[Point]
        List of all nodes as Points
``
    """

    # Middle point between p1 and p2
    p_m = Point(p1.x + (p2.x-p1.x)/2,(p2.y-p1.y)/2 + p1.y)
    # Diameter of the circumference with center in p_m
    d = np.sqrt((p_m.x-p1.x)**2 + (p_m.y-p1.y)**2)*2

    # Find an angle relative to a cartesian grid
    cat_op = np.absolute(p_m.x-p1.x)
    hip = d/2
    alpha = np.rad2deg(np.arcsin(cat_op/hip))

    # Find the middle point of the arc
    if (p2.y-p1.y) != 0:
        # Slope of line perpendicular to the straight segment between p1 and p2
        m = -(p2.x-p1.x)/(p2.y-p1.y)
        # x of point at a d/2 distance of p_m
        x_m_arc = p_m.x - (d/2)/np.sqrt(1+m**2)
        # y of point at a d/2 distance of p_m
        y_m_arc = m*x_m_arc + p_m.y - m*p_m.x
        # Create the point
        p_m_arc = Point(x_m_arc, y_m_arc)
    else:
        p_m_arc = Point(p_m.x,p_m.y-d/2)

    if p2.x > p1.x:
        if p2.y > p1.y:
            theta = - alpha + arc_dir
        else:
            theta = alpha + arc_dir
    else:
        if p2.y > p1.y:
            theta = alpha + arc_dir
        elif p2.y == p1.y:
            theta = alpha + arc_dir
        else:
            theta = - alpha + arc_dir

    ax.add_patch(Arc((p_m.x,p_m.y), d, d, theta1=theta, theta2=theta+180));

    # Initialize nodes list
    nodes = []
    if build_nodes:
        for i in range(0, n):
            # The nodes are distributed evenly along the arc length
            s = ((np.pi * (d / 2)) / n) * i

            # this node angle
            beta = np.rad2deg(s / (d / 2))

            # Some of all angles of triangle (p_m.x-p1.x)^(d/2)^(p_m.y-p1.y) is 180°
            gamma = 180 - 90 - alpha

            # Coordinates of node
            x_node = p_m.x - np.cos(np.deg2rad(beta + gamma)) * (d / 2)
            y_node = p_m.y - np.sin(np.deg2rad(beta + gamma)) * (d / 2)


            # Append node Point to node list
            nodes.append(Point(x_node, y_node))

    return p_m_arc, nodes
    

