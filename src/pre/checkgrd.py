#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

"""Module to read binary files containing grid block information"""

__author__ = "Maximiliano Bove"
__email__ = "mbove@fing.edu.uy"
__status__ = "Prototype"
__date__ = "06/21"

# Built-in modules
import sys

# Third-party modules
# import matlab.engine
import numpy as np

# Local modules


def checkgrd(grid_filename):
    '''Deprecated, use readgrd instead. Function that calls matlab function checkGridMBoo8o3008 that reads grd files containing grid blocks information.'''

    # start matlab
    eng = matlab.engine.start_matlab()

    # send matlab work folder to pre directory
    eng.cd("src/pre")

    # Call matlab function that reads the .grd binary files and returns the coordinates of the cell centers
    xc, yc, zc = eng.checkGridMBo8o3008(grid_filename, nargout=3)

    # Convert coordinates returned by matlab as tuples to np.arrays
    xc = np.asarray(xc)
    yc = np.asarray(yc)
    zc = np.asarray(zc)

    # return cell centers coordinates
    return xc, yc, zc


def readgrd(grd):
    '''Function to read <name>.grd file that contains the structured grid block information in binary format.'''

    # Define the path of the file relative to this working dir
    grd = 'src/pre/' + grd

    # Define the number of bytes of a int32: 4
    nb_int32 = np.int32().nbytes
    # Define the number of bytes of a int64 dtype: 8
    nb_int64 = np.int64().nbytes
    # Define the number of bytes of a single dtype: 4
    nb_single = np.single().nbytes

    # Open file with "read binary" permission
    f = open(grd, "rb")

    # Do the actual reading and return the cell centers
    XC, YC, ZC = readgrd_vars(nb_int32, nb_int64, nb_single, f)

    return XC, YC, ZC


def readgrd_vars(nb_int32, nb_int64, nb_single, f):
    '''Function to read the content of the grd file line by line, converting the binary output to an integer'''

    dum1 = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NXA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)

    if NXA == 0:
        dumword = nb_int64
        NXA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    else:
        dumword = nb_int32

    NYA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NZA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NXYZA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NIA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NOA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NSA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NWA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NUA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NGA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NOCA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NWALI = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NWALA = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NWALF = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NIAX = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NOAX = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NSAX = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NWAX = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NPRX = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NTGX = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    NOCX = int.from_bytes(f.read(nb_int32), byteorder=sys.byteorder)
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)

    # Read some indices
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    LI = np.frombuffer(f.read(nb_int32*NXA), dtype="int32")
    LK = np.frombuffer(f.read(nb_int32*NZA), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)

    # Read indices for B.C
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    IJI = np.frombuffer(f.read(nb_int32*NIAX), dtype="int32")
    IJPI = np.frombuffer(f.read(nb_int32*NIAX), dtype="int32")
    IJI1 = np.frombuffer(f.read(nb_int32*NIAX), dtype="int32")
    IJI2 = np.frombuffer(f.read(nb_int32*NIAX), dtype="int32")
    IJI3 = np.frombuffer(f.read(nb_int32*NIAX), dtype="int32")
    IJI4 = np.frombuffer(f.read(nb_int32*NIAX), dtype="int32")
    ITAGI = np.frombuffer(f.read(nb_int32*NIAX), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    #
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    IJO = np.frombuffer(f.read(nb_int32*NOAX), dtype="int32")
    IJPO = np.frombuffer(f.read(nb_int32*NOAX), dtype="int32")
    IJO1 = np.frombuffer(f.read(nb_int32*NOAX), dtype="int32")
    IJO2 = np.frombuffer(f.read(nb_int32*NOAX), dtype="int32")
    IJO3 = np.frombuffer(f.read(nb_int32*NOAX), dtype="int32")
    IJO4 = np.frombuffer(f.read(nb_int32*NOAX), dtype="int32")
    ITAGO = np.frombuffer(f.read(nb_int32*NOAX), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    #
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    IJS = np.frombuffer(f.read(nb_int32*NSAX), dtype="int32")
    IJPS = np.frombuffer(f.read(nb_int32*NSAX), dtype="int32")
    IJS1 = np.frombuffer(f.read(nb_int32*NSAX), dtype="int32")
    IJS2 = np.frombuffer(f.read(nb_int32*NSAX), dtype="int32")
    IJS3 = np.frombuffer(f.read(nb_int32*NSAX), dtype="int32")
    IJS4 = np.frombuffer(f.read(nb_int32*NSAX), dtype="int32")
    ITAGS = np.frombuffer(f.read(nb_int32*NSAX), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    #
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    IJW = np.frombuffer(f.read(nb_int32*NWAX), dtype="int32")
    IJPW = np.frombuffer(f.read(nb_int32*NWAX), dtype="int32")
    IJW1 = np.frombuffer(f.read(nb_int32*NWAX), dtype="int32")
    IJW2 = np.frombuffer(f.read(nb_int32*NWAX), dtype="int32")
    IJW3 = np.frombuffer(f.read(nb_int32*NWAX), dtype="int32")
    IJW4 = np.frombuffer(f.read(nb_int32*NWAX), dtype="int32")
    ITAGW = np.frombuffer(f.read(nb_int32*NWAX), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    #
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    IJU = np.frombuffer(f.read(nb_int32*NPRX), dtype="int32")
    IJPU = np.frombuffer(f.read(nb_int32*NPRX), dtype="int32")
    IJU1 = np.frombuffer(f.read(nb_int32*NPRX), dtype="int32")
    IJU2 = np.frombuffer(f.read(nb_int32*NPRX), dtype="int32")
    IJU3 = np.frombuffer(f.read(nb_int32*NPRX), dtype="int32")
    IJU4 = np.frombuffer(f.read(nb_int32*NPRX), dtype="int32")
    ITAGU = np.frombuffer(f.read(nb_int32*NPRX), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    #
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    IJG = np.frombuffer(f.read(nb_int32*NTGX), dtype="int32")
    IJPG = np.frombuffer(f.read(nb_int32*NTGX), dtype="int32")
    IJG1 = np.frombuffer(f.read(nb_int32*NTGX), dtype="int32")
    IJG2 = np.frombuffer(f.read(nb_int32*NTGX), dtype="int32")
    IJG3 = np.frombuffer(f.read(nb_int32*NTGX), dtype="int32")
    IJG4 = np.frombuffer(f.read(nb_int32*NTGX), dtype="int32")
    ITAGG = np.frombuffer(f.read(nb_int32*NTGX), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    #
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    IJL = np.frombuffer(f.read(nb_int32*NOCX), dtype="int32")
    IJR = np.frombuffer(f.read(nb_int32*NOCX), dtype="int32")
    IJOC1 = np.frombuffer(f.read(nb_int32*NOCX), dtype="int32")
    IJOC2 = np.frombuffer(f.read(nb_int32*NOCX), dtype="int32")
    IJOC3 = np.frombuffer(f.read(nb_int32*NOCX), dtype="int32")
    IJOC4 = np.frombuffer(f.read(nb_int32*NOCX), dtype="int32")
    ITAGOC = np.frombuffer(f.read(nb_int32*NOCX), dtype="int32")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    #
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)
    X = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    Y = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    Z = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    XC = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    YC = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    ZC = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FEE = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FEN = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FET = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FNE = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FNN = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FNT = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FTE = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FTN = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    FTT = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    VOL = np.frombuffer(f.read(nb_single*NXYZA), dtype="single")
    SRDW = np.frombuffer(f.read(nb_single*NWAX), dtype="single")
    XNW = np.frombuffer(f.read(nb_single*NWAX), dtype="single")
    YNW = np.frombuffer(f.read(nb_single*NWAX), dtype="single")
    ZNW = np.frombuffer(f.read(nb_single*NWAX), dtype="single")
    SRDS = np.frombuffer(f.read(nb_single*NSAX), dtype="single")
    XNS = np.frombuffer(f.read(nb_single*NSAX), dtype="single")
    YNS = np.frombuffer(f.read(nb_single*NSAX), dtype="single")
    ZNS = np.frombuffer(f.read(nb_single*NSAX), dtype="single")
    dum1 = int.from_bytes(f.read(dumword), byteorder=sys.byteorder)

    # Finihed reading. Close the file
    f.close()

    # Rearrange some variables
    X = np.reshape(X, (NYA, NXA, NZA))
    Y = np.reshape(Y, (NYA, NXA, NZA))
    Z = np.reshape(Z, (NYA, NXA, NZA))
    XC = np.reshape(XC, (NYA, NXA, NZA))
    YC = np.reshape(YC, (NYA, NXA, NZA))
    ZC = np.reshape(ZC, (NYA, NXA, NZA))
    FEE = np.reshape(FEE, (NYA, NXA, NZA))
    FEN = np.reshape(FEN, (NYA, NXA, NZA))
    FET = np.reshape(FET, (NYA, NXA, NZA))
    FNE = np.reshape(FNE, (NYA, NXA, NZA))
    FNN = np.reshape(FNN, (NYA, NXA, NZA))
    FNT = np.reshape(FNT, (NYA, NXA, NZA))
    FTE = np.reshape(FTE, (NYA, NXA, NZA))
    FTN = np.reshape(FTN, (NYA, NXA, NZA))
    FTT = np.reshape(FTT, (NYA, NXA, NZA))
    VOL = np.reshape(VOL, (NYA, NXA, NZA))

    return XC, YC, ZC
