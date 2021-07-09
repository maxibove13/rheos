#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module with functions to test mayavi library"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "07/21"

from mayavi import mlab
from numpy import pi, sin, cos, mgrid


def mayavi_flower():
    """Function that plots a 3D flower like function"""
    # Create the data.
    dphi, dtheta = pi/250.0, pi/250.0
    [phi, theta] = mgrid[0:pi+dphi*1.5:dphi, 0:2*pi+dtheta*1.5:dtheta]
    m0 = 4
    m1 = 3
    m2 = 2
    m3 = 3
    m4 = 6
    m5 = 2
    m6 = 6
    m7 = 4
    r = sin(m0*phi)**m1 + cos(m2*phi)**m3 + \
        sin(m4*theta)**m5 + cos(m6*theta)**m7
    x = r*sin(phi)*cos(theta)
    y = r*cos(phi)
    z = r*sin(phi)*sin(theta)

    # View it.
    s = mlab.mesh(x, y, z)
    mlab.show()


if __name__ == "__main__":
    mayavi_flower()
