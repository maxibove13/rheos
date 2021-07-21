#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module with functions to test mayavi library"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "07/21"

# from mayavi.mlab import mesh, show
from numpy import pi, sin, cos, mgrid


def mayavi_flower():
    """Function that plots a 3D flower like function"""
    # Create the data.
    dphi, dtheta = pi/250.0, pi/250.0
    [phi, theta] = mgrid[0:pi+dphi*1.5:dphi, 0:2*pi+dtheta*1.5:dtheta]
    M0 = 4
    M1 = 3
    M2 = 2
    M3 = 3
    M4 = 6
    M5 = 2
    M6 = 6
    M7 = 4
    r = sin(M0*phi)**M1 + cos(M2*phi)**M3 + \
        sin(M4*theta)**M5 + cos(M6*theta)**M7
    x = r*sin(phi)*cos(theta)
    y = r*cos(phi)
    z = r*sin(phi)*sin(theta)

    # View it.
    s = mesh(x, y, z)
    show()


if __name__ == "__main__":
    mayavi_flower()
