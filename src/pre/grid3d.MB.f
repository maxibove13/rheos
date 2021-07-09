C#########################################################
      PROGRAM GRGEN
C#########################################################
C     This version of GRGEN has been extended to 3D,
C     please see below.
C
C     Original coments :
C
C     This code generates non-orthogonal body-fitted grids
C     (one block only). The grid is automatically refined
C     up to the number of grid levels prescribed. This
C     version includes C- and O-type grids. See 
C     readme-file for a description.
C
C     This is Version 1.3 of the code, December 1996.
C
C     The user may modify the code and give it to third
C     parties, provided that an acknowledgement to the
C     source of the original version is retained.
C
C                M. Peric, Hamburg, 1996
C                peric@schiffbau.uni-hamburg.de
C
C
C     Coments added by G.Usera Oct/2004 :
C
C     This version has been addapted for generation of
C     3D grids for the 3D version of Caffa.
C     Multiple grid levels have been disabled and the notation
C     was removed. 
C     The treatment of OC cuts is now done by the grid
C     postprocessor 'block3d.f', where treatment has been
C     generalized to multi-block structured grids.
C     Each grid block should be generated separately with this
C     grid generator.
C     Please note that the solver accepts general 
C     curvilinear structured grid blocks. However this grid
C     generator currently is only able to 'extrude' a 2D grid
C     in the third ( 'K' or 'Z' ) direction. Pleas see the
C     read.me file
C     Also input is now only from text file (not keyboard)
C     and there are no plotting options
C
C               G. Usera, Montevideo/Tarragona, 2004
C               
C               @ Universitat Rovira i Virgili, Catalunia
C                 gabusera@etseq.urv.es
C
C               @ Universidad de la Republica, Uruguay
C                 gusera@fing.edu.uy
C
C=========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'bound3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....DEFINE FILES
C
      PRINT *, ' ENTER PROBLEM NAME (SIX CHARACTERS):  '
      READ(*,'(A6)') NAME
C
      WRITE( FILIN,'(A6,4H.gin)') NAME
      OPEN (UNIT=5,FILE=FILIN)
      REWIND 5
C
      WRITE(FILGRD,'(A6,4H.grd)') NAME
C
c     OPEN (UNIT=7,FILE=FILGRD,FORM='binary')
      OPEN (UNIT=7,FILE=FILGRD,FORM='unformatted')
      REWIND 7
C
C.....GENERATE THE GRID AND SMOOTH IT
C
      CALL GRIDGN
C
C.....Specific Rotational distortion
C
C     CALL ROTATEDIST
C
      PRINT *,'Here0'
      IF(LSMOG) CALL SMOG
C
C.....Extension to 3D ( in z ) (Added by G.Usera 12/Oct/2004)
C
      PRINT *,'Here1'
      CALL GRIDZ
      CALL READBC
C
C.....Permute coordinates (Added by G.Usera Apr/2005)
C
      CALL PERMUTECOORD
C
C.....Rotate around coordinate axis (Added by G.Usera Jul/2009)
C
      CALL ROTATECOORD
C
C.....Offset origin (Added by G.Usera Apr/2005)
C
      CALL OFFSETORIGIN
C
C.....PERTURBATE GRID FOR TESTING PURPOSES
C
      CALL PERTURBATEGRID
C
C.....CALCULATE GRID DATA AND COLLECT BOUNDARY DATA
C
      CALL CALCG
      CALL SETBC
C
C.....CALCULATE BOUND. CELL FACE AREA DIVIDED BY DISTANCE AND UNIT VECTORS
C
      CALL SODW
      CALL SODS
C
C.....SET SOME MINIMUM B.C. ARRAY DIMENSIONS TO 1.
C
      NINX=MAX(NINL,1)
      NOUX=MAX(NOUT,1)
      NSYX=MAX(NSYM,1)
      NWAX=MAX(NWAL,1)
      NPRX=MAX(NPRU,1)
      NTGX=MAX(NTRG,1)
      NOCX=MAX(NOC,1)

C
C.....STORE GRID DATA
C
      IUNIT=7
C
      WRITE(IUNIT)
     *         NI,NJ,NK,NIJK,NINL,NOUT,NSYM,NWAL,
     *         NPRU,NTRG,NOC,
     *         NWALI,NWALA,NWALF,
     *         NINX,NOUX,NSYX,NWAX,NPRX,NTGX,NOCX
C
      WRITE(IUNIT)
     *        (LI(I),I=1,NI),(LK(K),K=1,NK)
C      
      WRITE(IUNIT)
     *        (IJI(I)  ,I=1,NINX),(IJPI(I) ,I=1,NINX),
     *        (IJI1(I) ,I=1,NINX),(IJI2(I) ,I=1,NINX),
     *        (IJI3(I) ,I=1,NINX),(IJI4(I) ,I=1,NINX),
     *        (ITAGI(I),I=1,NINX)
      WRITE(IUNIT)
     *        (IJO(I)  ,I=1,NOUX),(IJPO(I) ,I=1,NOUX),
     *        (IJO1(I) ,I=1,NOUX),(IJO2(I) ,I=1,NOUX),
     *        (IJO3(I) ,I=1,NOUX),(IJO4(I) ,I=1,NOUX),
     *        (ITAGO(I),I=1,NOUX)
      WRITE(IUNIT)
     *        (IJS(I)  ,I=1,NSYX),(IJPS(I) ,I=1,NSYX),
     *        (IJS1(I) ,I=1,NSYX),(IJS2(I) ,I=1,NSYX),
     *        (IJS3(I) ,I=1,NSYX),(IJS4(I) ,I=1,NSYX),
     *        (ITAGS(I),I=1,NSYX)
      WRITE(IUNIT)
     *        (IJW(I)  ,I=1,NWAX),(IJPW(I) ,I=1,NWAX),
     *        (IJW1(I) ,I=1,NWAX),(IJW2(I) ,I=1,NWAX),
     *        (IJW3(I) ,I=1,NWAX),(IJW4(I) ,I=1,NWAX),
     *        (ITAGW(I),I=1,NWAX)
      WRITE(IUNIT)
     *        (IJU(I)  ,I=1,NPRX),(IJPU(I) ,I=1,NPRX),
     *        (IJU1(I) ,I=1,NPRX),(IJU2(I) ,I=1,NPRX),
     *        (IJU3(I) ,I=1,NPRX),(IJU4(I) ,I=1,NPRX),
     *        (ITAGU(I),I=1,NPRX)
      WRITE(IUNIT)
     *        (IJG(I)  ,I=1,NTGX),(IJPG(I) ,I=1,NTGX),
     *        (IJG1(I) ,I=1,NTGX),(IJG2(I) ,I=1,NTGX),
     *        (IJG3(I) ,I=1,NTGX),(IJG4(I) ,I=1,NTGX),
     *        (ITAGG(I),I=1,NTGX)
      WRITE(IUNIT)
     *        (IJL(I)  ,I=1,NOCX),(IJR(I)  ,I=1,NOCX),
     *        (IJOC1(I),I=1,NOCX),(IJOC2(I),I=1,NOCX),
     *        (IJOC3(I),I=1,NOCX),(IJOC4(I),I=1,NOCX),
     *        (ITAGOC(I),I=1,NOCX)

C
      WRITE(IUNIT)
     *        (X(I)  ,I=1,NIJK),(Y(I)  ,I=1,NIJK),(Z(I)  ,I=1,NIJK),
     *        (XC(I) ,I=1,NIJK),(YC(I) ,I=1,NIJK),(ZC(I) ,I=1,NIJK),
     *        (FEE(I),I=1,NIJK),(FEN(I),I=1,NIJK),(FET(I),I=1,NIJK),
     *        (FNE(I),I=1,NIJK),(FNN(I),I=1,NIJK),(FNT(I),I=1,NIJK),
     *        (FTE(I),I=1,NIJK),(FTN(I),I=1,NIJK),(FTT(I),I=1,NIJK),
     *        (VOL(I),I=1,NIJK),
     *        (SRDW(I),I=1,NWAX),
     *        (XNW(I),I=1,NWAX),(YNW(I),I=1,NWAX),(ZNW(I),I=1,NWAX),
     *        (SRDS(I),I=1,NSYX),
     *        (XNS(I),I=1,NSYX),(YNS(I),I=1,NSYX),(ZNS(I),I=1,NSYX)
C
      CLOSE(UNIT=5)
      CLOSE(UNIT=7)
C
      STOP
      END
C
C
C######################################################
      SUBROUTINE GRIDGN
C######################################################
C     This routine generates the grid.
C======================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'bound3d.ing'
C
C.....READ CONTROL PARAMETERS
C
      PRINT *,' ENTER> LSTORE, LCALG, LPRINT, LSMOG :  '
      READ(5,*) LSTORE,LCALG,LPRINT,LSMOG
      PRINT *,' SELECTION OF STRAIGHT LINES: 0 -> S-N, 1 -> W-E:  '
      READ(5,*) IDIR 
C
C.....new option, IDIR=3 for reading (x,y) distribution from file
C..... G. Usera 13/Sep/2007
C
      IF(IDIR.LT.3) THEN
C
C.......GENERATE GRID POINTS ALONG BOUNDARY
C
        CALL BGRID
C
C.......CALCULATE GRID POINTS IN THE INTERIOR
C
        PRINT *,'Here,GRIDGN,0'
        CALL CALXY(IDIR)
        PRINT *,'Here,GRIDGN,1'
C
      ELSE IF(IDIR==3) THEN
C
C......( IDIR=3 )
C
        CALL READXY
C
      ELSE IF(IDIR==4) THEN
C
        CALL READXYZ
C
      ENDIF
C
      RETURN
      END
C
C##########################################################
      BLOCK DATA
C##########################################################
      COMMON /CHCS/ CHS(4)
      CHARACTER CHS*5
      DATA CHS /'SOUTH','NORTH','WEST ','EAST '/
      END
C
C
C##########################################################
      SUBROUTINE BGRID 
C##########################################################
C     This routine generates grid points along solution
C     domain boundaries. Boundaries are subdivided into
C     line segments (straight, circle, or arbitrary); data
C     is provided for each line interactivly.
C==========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'lines3d.ing'
      LOGICAL LREPT
C
C.....NUMBER OF CONTROL VOLUMES IN I AND J DIRECTION
C
      PRINT *,' ENTER NO. OF CVs IN I AND J DIRECTION:  '
      READ(5,*) NICV,NJCV
C
C.....INDEX CONTROL AND CONVERSION
C
      NI=NICV+2
      NJ=NJCV+2
      NIM=NI-1
      NJM=NJ-1
      DO I=1,NI
        LI(I)=(I-1)*NJ
      END DO
C
   10 CONTINUE
      NCIR=0
      NARB=0
C
C.....LINES ALONG BOUNDARY; COORD. OF BEGIN OF LINE 1
C
      DO L=1,4
        NCF(L)=0
        PRINT *,' NO. OF LINES ON ',CHS(L),' SIDE:  '
        READ(5,*) NLINES(L)
        PRINT *,' COORDINATES OF BEGIN OF LINE 1:  '
        READ(5,*) XLS,YLS
C
C.....COORD. OF LINE END 
C
        DO LL=1,NLINES(L)
          PRINT *,' COORDINATES OF LINE ',LL,'  END:  '
          READ(5,*) XLE,YLE
C
C.....READ LINE SPECIFICATIONS
C
   50     PRINT *,' NUMBER OF SEGMENTS, LINE TYPE :  '
          READ(5,*) NSEG,LTYP
          NCF(L)=NCF(L)+NSEG
          IF((L.LE.2.AND.NCF(L).GT.NICV).OR.
     *       (L.GT.2.AND.NCF(L).GT.NJCV)) THEN
            PRINT *,' TOO MANY SEGMENTS - MORE THAN NICV OR NJCV !!! '
            PRINT *,' REDUCE NSEG TO MATCH NICV OR NJCV AND RE-TYPE!!'
            GO TO 50
          ENDIF
          PRINT *,' SIZE OF SEGMENT AT LINE BEGIN, EXPANSION FACTOR:  '
          READ(5,*) DX1,EXP
C
C.....STRAIGHT LINE: CALCULATE COORD. OF BOUNDARY GRID POINTS
C
          IF(LTYP.EQ.1) CALL STRLINE(L)
C
C.....CIRCLE LINES: DEFINE CIRCLE SEGMENT AND CALCULATE GRID COORD. 
C
          IF(LTYP.EQ.2) THEN
            PRINT *,' ENTER COORDINATES OF A THIRD POINT ON CIRCLE;'
            PRINT *,' IF FULL CIRCLE, COORD. OF CENTER INSTEAD:  '
            READ(5,*) XLM,YLM
            PRINT *,' ENTER ANGLES AT LINE BEGIN AND END:  '
            READ(5,*) FIS,FIE
C
            CALL CIRCLIN(L)
          ENDIF
C
C.....ARBITRARY LINE: READ COORDINATES OF GRID POINTS
C
          IF(LTYP.EQ.3) CALL ARBLINE(L)
C
C.....STRAIGHT LINE with grouping: CALCULATE COORD. OF BOUND...
C
          IF(LTYP.EQ.11) CALL STRLINE11(L)
C
C.....SET COORD. OF THE BEGIN OF NEXT LINE
C
          XLS=XLE
          YLS=YLE
        END DO
C
      END DO
C
C.....CHECK NUMBERS OF CELL FACES ON OPOSITE BLOCK SIDES
C
      LREPT=.FALSE.
C
      DO NL=1,3,2
        IF(NCF(NL).NE.NCF(NL+1)) THEN
          PRINT *,' ERROR: NUMBER OF CV-FACES ON SIDES ',CHS(NL),
     *            ' AND ',CHS(NL+1),' DO NOT MATCH  '
          PRINT *,' REPEAT DATA INPUT!!! '
          LREPT=.TRUE.
        ENDIF
      END DO
C
      IF(NCF(1).NE.NICV.OR.NCF(3).NE.NJCV) THEN
        PRINT *,' NUMBER OF SEGMENTS ON BOUNDARIES DOES NOT MATCH'
        PRINT *,' SPECIFIED NICV AND NJCV; ADJUST EITHER AND RE-TYPE'
        LREPT=.TRUE.
      ENDIF
C
      IF(LREPT) GO TO 10
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE STRLINE(L)
C############################################################
C     This routine deals with straight lines.
C============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....CALCULATE COORDINATES OF GRID POINTS ALONG THE LINE
C
      XYL=SQRT((XLE-XLS)**2+(YLE-YLS)**2)
C
      CALL DIVLINE(XYL)
C
C.....ASSIGN LINE POINTS TO GRID POINTS
C
      CALL SETPT(L,IE,IS,JE,JS)
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE STRLINE11(L)
C############################################################
C     This routine deals with straight lines.
C
C     STRLINE11 : This option allows for cell grouping
C     within Line Segment. Usefull for matching streched
C     lines with many-to-one interfaces
C     Added by G.Usera 21/Oct/2005
C
C============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....Read grouping parameter
C
      READ(5,*) NGROUP
C
C.....Keep copy of NSEG and DX1
C
      NSEGAUX=NSEG
      DX1AUX=DX1
C
      NSEG=NSEG*NGROUP
      DX1=DX1/NGROUP
C
C.....CALCULATE COORDINATES OF GRID POINTS ALONG THE LINE
C
      XYL=SQRT((XLE-XLS)**2+(YLE-YLS)**2)
C
      CALL DIVLINE(XYL)
C
C.....Apply cell grouping
C
      NSEG=NSEGAUX
      DX1=DX1AUX
      DO I=2,NSEG+1
        IGROUP=NGROUP*(I-1)+1
        XPT(I)=XPT(IGROUP)
        YPT(I)=YPT(IGROUP)
      END DO
C
C.....ASSIGN LINE POINTS TO GRID POINTS
C
      CALL SETPT(L,IE,IS,JE,JS)
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE SETPT(L,IE,IS,JE,JS)
C##########################################################
C     This routine assignes line points to boundary points.
C==========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'lines3d.ing'
      INCLUDE 'grid3d.ing'
C
C.....ASSIGN LINE POINTS TO GRID POINTS FOR SOUTH AND NORTH BOUND.
C
      IF(L.LE.2) THEN
        JS=1
        IF(L.EQ.2) JS=NJ-1
        IE=NCF(L)+1
        IS=IE-NSEG
        JE=JS
C
        DO I=IS,IE
          IJ=LI(I)+JS
          X(IJ)=XPT(I-IS+1)
          Y(IJ)=YPT(I-IS+1)
        END DO
C
      ELSE
C
C.....ASSIGN LINE POINTS TO GRID POINTS FOR WEST AND EAST BOUND.
C
        IS=1
        IF(L.EQ.4) IS=NI-1
        JE=NCF(L)+1
        JS=JE-NSEG
        IE=IS
        IND=L-2
C
        DO J=JS,JE
          IJ=LI(IS)+J
          X(IJ)=XPT(J-JS+1)
          Y(IJ)=YPT(J-JS+1)
        END DO
C
      ENDIF
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE CIRCLIN(L)
C############################################################
C     This routine deals with lines which are circle segments.
C============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'lines3d.ing'
      INCLUDE 'circl3d.ing'
C
C.....CHECK IF FULL CIRCLE, SET CENTER COORDINATES AND RADIUS
C
      XYL=SQRT((XLE-XLS)**2+(YLE-YLS)**2)
      IF(XYL.LT.1.E-20) THEN
        XCC=XLM
        YCC=YLM
        RCC=SQRT((XLM-XLS)**2+(YLM-YLS)**2)
      ELSE
C
C.....FIND CIRCLE CENTER AND RADIUS
C
        C1=(XLM-XLS)/((YLM-YLS)+1.E-20)
        C2=(XLE-XLM)/((YLE-YLM)+1.E-20)
        X1=0.5*(XLS+XLM)
        Y1=0.5*(YLS+YLM)
        X2=0.5*(XLE+XLM)
        Y2=0.5*(YLE+YLM)
        XCC=(Y1-Y2+C1*X1-C2*X2)/((C1-C2)+1.E-20)
        IF(ABS(C1).LT.ABS(C2)) THEN
          YCC=Y1-C1*(XCC-X1)
        ELSE
          YCC=Y2-C2*(XCC-X2)
        ENDIF
C
        RCC=SQRT((XCC-XLS)**2+(YCC-YLS)**2)
      ENDIF
C
      XYL=ABS(FIE-FIS)      
C
C.....CALCULATE COORDINATES OF GRID POINTS ALONG THE LINE
C
      CALL DIVLINE(XYL)
C
C.....ASSIGN LINE POINTS TO GRID POINTS 
C
      CALL SETPT(L,IE,IS,JE,JS)
C
C.....STORE CIRCLE DATA FOR GRID REFINEMENT
C
      NCIR=NCIR+1
      JSCIR(NCIR)=JS
      ISCIR(NCIR)=IS
      JECIR(NCIR)=JE
      IECIR(NCIR)=IE
      RCIR(NCIR)=RCC     
      XCCIR(NCIR)=XCC
      YCCIR(NCIR)=YCC
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE ARBLINE(L)
C############################################################
C     This routine deals with arbitrary lines which are 
C     defined by specifying coordinates of each point.
C============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'lines3d.ing'
      INCLUDE 'circl3d.ing'
C
C.....READ COORDINATES OF GRID POINTS ALONG THE LINE
C
      DO I=1,NSEG+1
        PRINT*,' ENTER COORD. (X,Y) OF POINT NO.',I,' :  '
        READ(5,*) XPT(I),YPT(I)
      END DO
C
C.....ASSIGN LINE POINTS TO GRID POINTS
C
      CALL SETPT(L,IE,IS,JE,JS)
C
C.....STORE LINE DATA FOR SMOOTHING OF REFINED GRIDS
C
      NARB=NARB+1
      ISARB(NARB)=IS
      JSARB(NARB)=JS
      JEARB(NARB)=JE
      IEARB(NARB)=IE
C
      RETURN
      END
C
C
C#########################################################
      SUBROUTINE DIVLINE(XYL)
C#########################################################
C     This routine subdivides a line into a specified
C     number of segments and creates coordinates of points
C     along the line.
C=========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'lines3d.ing'
      INCLUDE 'circl3d.ing'
      DIMENSION DL(NXYA),FD(NXYA)
C
C.....UNIFORM SUBDIVISION (ZERO INPUT FOR BOTH DX1 AND EXP)
C
      IF(DX1.LT.1.E-20.AND.EXP.LT.1.E-20) THEN
        DX1=XYL/REAL(NSEG)
        EXP=1.0
C
C.....SPECIFIED SIZE OF THE FIRST SEGMENT
C
      ELSEIF(DX1.GT.1.E-20) THEN
        CALL EXPAND(XYL)
C
C.....SPECIFIED EXPANSION RATIO
C
      ELSEIF(EXP.GT.1.E-20) THEN
        IF(ABS(EXP-1.0).LT.0.001) THEN
          DX1=XYL/REAL(NSEG)
        ELSE
          DX1=XYL*(1.-EXP)/(1.-EXP**NSEG)
        ENDIF
      ENDIF
C
C.....CALCULATE SIZE OF INTERVALS AND SCALE TO THE RANGE (0 ... 1)
C
      XYLR=1./(XYL+1.E-20)
      DL(1)=DX1
      FD(1)=0.
      SUML=0.
C
      DO I=2,NSEG
        DL(I)=DL(I-1)*EXP
        SUML=SUML+DL(I-1)
        FD(I)=MIN(SUML*XYLR,1.0)
      END DO
      FD(NSEG+1)=1.0 
C
C.....CALCULATE COORDINATES OF GRID POINTS FOR STRAIGHT LINES
C
      IF((LTYP.EQ.1).OR.(LTYP.EQ.11)) THEN
        DO I=1,NSEG+1
          XPT(I)=XLS+FD(I)*(XLE-XLS)
          YPT(I)=YLS+FD(I)*(YLE-YLS)
        END DO
C
C.....CALCULATE COORDINATES OF GRID POINTS FOR CIRCLE LINES 
C
      ELSEIF(LTYP.EQ.2) THEN
        FIP=ATAN(1.)/45.
        XPT(1)=XLS
        YPT(1)=YLS
        XPT(NSEG+1)=XLE
        YPT(NSEG+1)=YLE
        DO I=2,NSEG
          ANGLE=(FIS+FD(I)*(FIE-FIS))*FIP
          XPT(I)=XCC+RCC*COS(ANGLE)
          YPT(I)=YCC+RCC*SIN(ANGLE)
        ENDDO
      ENDIF
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE EXPAND(XYL)
C########################################################
C     This routine calculates the expansion factor along
C     a line when the size of the first segment is 
C     specified.
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'lines3d.ing'
      FI(A,B,G,N)=A*G**N-B*G+B-A
C
C.....TOLERANCE, RANGE
C
      EPS=1.E-10
      EMAX=2.
      EMIN=.5
      I=0
C 
C.....CALC. AVERAGE DX AND DETERMINE WHETHER SUBDIVISION IS UNIFORM
C
      AR=XYL/REAL(NSEG)
      IF(ABS(DX1-AR)/(AR+1.E-20).LT.0.01) THEN
        EXP=1.0
        DX1=AR
C
C.....NON-UNIFORM DISTRIBUTION, FIND WHETHER EXPANSION OR CONTRACTION
C
      ELSE
        IF(DX1.LT.AR) THEN
          E2=EMAX
          E1=1.0001
        ELSE
          E1=EMIN
          E2=0.9999
        ENDIF
C
C.....CALCULATE EXPANSION RATIO USING INTERVAL HALVING METHOD
C
        FI1=FI(DX1,XYL,E1,NSEG)
        FI2=FI(DX1,XYL,E2,NSEG)
        IF(SIGN(1.,FI1).NE.SIGN(1.,FI2)) THEN
C
C.....ITERATE UNTIL EXP IS FOUND WHICH FITS SPECIFIED DX1 AND XYL
C
          MAXITER=1000
   10     I=I+1
          E=0.5*(E1+E2)
          FIP=FI(DX1,XYL,E,NSEG)
          IF(SIGN(1.,FIP).EQ.SIGN(1.,FI2)) THEN
            E2=E
            FI2=FIP
          ELSE
            E1=E
            FI1=FIP
          ENDIF
          IF(ABS(FI2-FI1).GT.EPS.AND.I.LT.MAXITER) GO TO 10
C
C.....PRINT MESSAGE (EXP OR OUT OF RANGE)
C
          EXP=E
          PRINT *, '   EXP. FACTOR FOR THIS LINE:  EXP = ',EXP
          PRINT *, '     ',I,'  ITERATIONS PERFORMED '
        ELSE
          PRINT *, '  *** ERROR: EXPANSION FACTOR > 2 OR < 0.5 ***'
          PRINT *, '  I TAKE EXP=1.0; CHANGE DATA FOR THIS LINE?  '
          EXP=1.0
          DX1=AR
        ENDIF
C
      ENDIF
C
      RETURN
      END
C
C
C###########################################################
      SUBROUTINE CALXY(IDD) 
C###########################################################
C     This routine calculates the coordinates of grid points
C     in the interior of the solution domain once the 
C     coordinates of grid points along boundaries are known.
C===========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      DIMENSION FL(NXYA),FR(NXYA),FLN(NXYA),FRN(NXYA)
C
C.....OFFSETS AT WEST AND EAST BOUNDARY
C
      PRINT *,'Here.CALCXY.00'
C
      IIE=LI(NIM)
      IIW=LI(1)
C
C==================================================================
C.....CALCULATE COORDINATES OF INTERIOR POINTS (STRAIGHT LINES N-S)
C==================================================================
C
      PRINT *,'Here.CALCXY.0'
C
      IF(IDD.EQ.0) THEN
C
C.....DISTANCE BETWEEN CORNER POINTS (NORTH - SOUTH)
C.....Modified by G.Usera to allow for null length sides. Mar/2006
C
        DLR=SQRT((X(IIE+NJM)-X(IIE+1))**2+(Y(IIE+NJM)-Y(IIE+1))**2)
        DLL=SQRT((X(IIW+NJM)-X(IIW+1))**2+(Y(IIW+NJM)-Y(IIW+1))**2)
C
C.....Modified by G.Usera to allow for more than straight lines. Mar/2006
C
C.....Parallel and normal directions to WEST and EAST sides
C
        DTRX=(X(IIE+NJM)-X(IIE+1))/(DLR+1.E-20)
        DTRY=(Y(IIE+NJM)-Y(IIE+1))/(DLR+1.E-20)
        DTLX=(X(IIW+NJM)-X(IIW+1))/(DLL+1.E-20)
        DTLY=(Y(IIW+NJM)-Y(IIW+1))/(DLL+1.E-20)
C
C.....DISTRIBUTION FUNCTION ON WEST AND EAST SIDE
C
        PRINT *,'Here.CALCXY.1'
C
        DO J=1,NJM
          FR(J) =+(X(IIE+J)-X(IIE+1))*DTRX+(Y(IIE+J)-Y(IIE+1))*DTRY
          FR(J)=FR(J)/(DLR+1.E-20)
          FRN(J)=-(X(IIE+J)-X(IIE+1))*DTRY+(Y(IIE+J)-Y(IIE+1))*DTRX
          FL(J) =+(X(IIW+J)-X(IIW+1))*DTLX+(Y(IIW+J)-Y(IIW+1))*DTLY
          FL(J)=FL(J)/(DLL+1.E-20)
          FLN(J)=-(X(IIW+J)-X(IIW+1))*DTLY+(Y(IIW+J)-Y(IIW+1))*DTLX
C
          IF(DLR.LT.1.E-20) FR(J) =REAL(J-1)/REAL(NJM-1)
          IF(DLR.LT.1.E-20) FRN(J)=0.
          IF(DLL.LT.1.E-20) FL(J) =REAL(J-1)/REAL(NJM-1)
          IF(DLL.LT.1.E-20) FLN(J)=0.
        END DO
C
C.....DISTANCE BETWEEN OPOSITE POINTS ON SOUTH AND NORTH SIDE
C
        RNIM=1./REAL(NIM-1)
        DO I=2,NIM-1
          II=LI(I)
          DX=X(II+NJM)-X(II+1)
          DY=Y(II+NJM)-Y(II+1)
C
C.....Parallel and normal direction S-N for this line
C
          DTPX=DX/(SQRT(DX**2+DY**2)+1.E-20)
          DTPY=DY/(SQRT(DX**2+DY**2)+1.E-20)
C
C.....DISTRIBUTE POINTS USING INTERPOLATED WEST AND EAST DISTRIBUTIONS
C
          DO J=2,NJM-1
            FAC=(REAL(I-1)* FR(J)+REAL(NIM-I)* FL(J))*RNIM
            DNN=(REAL(I-1)*FRN(J)+REAL(NIM-I)*FLN(J))*RNIM
            X(II+J)=X(II+1)+FAC*DX-DTPY*DNN
            Y(II+J)=Y(II+1)+FAC*DY+DTPX*DNN
          END DO
        END DO
C
C==================================================================
C.....CALCULATE COORDINATES OF INTERIOR POINTS (STRAIGHT LINES E-W)
C==================================================================
C
      PRINT *,'Here.CALCXY.2'
C
      ELSEIF(IDD.EQ.1) THEN
C
      PRINT *,'Here.CALCXY.3'
C
C
C.....DISTANCE BETWEEN CORNERS (EAST - WEST)
C.....Modified by G.Usera to allow for null length sides. Mar/2006
C
        DLR=SQRT((X(IIE+NJM)-X(IIW+NJM))**2+(Y(IIE+NJM)-Y(IIW+NJM))**2)
        DLL=SQRT((X(IIE+1  )-X(IIW+1  ))**2+(Y(IIE+  1)-Y(IIW+  1))**2)
C
C.....Modified by G.Usera to allow for more than straight lines. Mar/2006
C
C.....Parallel and normal directions to SOUTH and NORTH sides
C
        DTRX=(X(IIE+NJM)-X(IIW+NJM))/(DLR+1.E-20)
        DTRY=(Y(IIE+NJM)-Y(IIW+NJM))/(DLR+1.E-20)
        DTLX=(X(IIE+1  )-X(IIW+1  ))/(DLL+1.E-20)
        DTLY=(Y(IIE+1  )-Y(IIW+1  ))/(DLL+1.E-20)
C
C.....DISTRIBUTION FUNCTION ALONG SOUTH AND NORTH BOUNDARY
C
        PRINT *,'Here.CALCXY.4'
C
        DO I=1,NIM
          II=LI(I)
          FR(I) =+(X(II+NJM)-X(IIW+NJM))*DTRX
     *           +(Y(II+NJM)-Y(IIW+NJM))*DTRY
          FR(I)=FR(I)/(DLR+1.E-20)
          FRN(I)=-(X(II+NJM)-X(IIW+NJM))*DTRY
     *           +(Y(II+NJM)-Y(IIW+NJM))*DTRX
C
          FL(I) =+(X(II+1  )-X(IIW+  1))*DTLX
     *           +(Y(II+1  )-Y(IIW+  1))*DTLY
          FL(I)=FL(I)/(DLL+1.E-20)
          FLN(I)=-(X(II+1  )-X(IIW+  1))*DTLY
     *           +(Y(II+1  )-Y(IIW+  1))*DTLX
C
          IF(DLR.LT.1.E-20) FR(I) =REAL(I-1)/REAL(NIM-1)
          IF(DLR.LT.1.E-20) FRN(I)=0.
          IF(DLL.LT.1.E-20) FL(I) =REAL(I-1)/REAL(NIM-1)
          IF(DLL.LT.1.E-20) FLN(I)=0.
        END DO
C
C.....DISTANCE BETWEEN OPOSITE POINTS ON EAST AND WEST BOUNDARY
C
        PRINT *,'Here.CALCXY.5'
C
        RNJM=1./REAL(NJM-1)
        DO J=2,NJM-1
          DX=X(IIE+J)-X(IIW+J)
          DY=Y(IIE+J)-Y(IIW+J)
C
C.....Parallel and normal direction W-E for this line
C
          DTPX=DX/(SQRT(DX**2+DY**2)+1.E-20)
          DTPY=DY/(SQRT(DX**2+DY**2)+1.E-20)
C
C.....DISTRIBUTE POINTS USING INTERPOLATED SOUTH AND NORTH DISTRIBUTIONS
C
          DO I=2,NIM-1
            IJ=LI(I)+J
            FAC=(REAL(J-1)* FR(I)+REAL(NJM-J)* FL(I))*RNJM
            DNN=(REAL(J-1)*FRN(I)+REAL(NJM-J)*FLN(I))*RNJM
            X(IJ)=X(IIW+J)+FAC*DX-DTPY*DNN
            Y(IJ)=Y(IIW+J)+FAC*DY+DTPX*DNN
          END DO
        END DO
C
      ENDIF
C
      RETURN
      END
C
C
C###########################################################
      SUBROUTINE ROTATEDIST 
C###########################################################
C     This routine distorts the grid with a 
C     user programmed rotation
C===========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
C
      PINUM=3.1415926535898
      FIP=ATAN(1.)/45.
C
C
      DO I=1,NI
      DO J=1,NJ
        IJ=LI(I)+J
        XP=X(IJ)
        YP=Y(IJ)
        RP=SQRT(XP*XP+YP*YP)
        THETA=(RP-0.05)*(5.*40.*FIP)
C
        X(IJ)=XP*COS(THETA)-YP*SIN(THETA)
        Y(IJ)=XP*SIN(THETA)+YP*COS(THETA)
C
      END DO
      END DO
C
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SMOG
C########################################################
C     This routine smooths grid in the interior.
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
C
C.....SMOOTH WEST-EAST LINES
C
      CALL SMOGP(NJ)

C.....SMOOTH SOUTH-NORTH LINES
C
      IND=1
      CALL SMOGP(IND)
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SMOGP(IND)
C########################################################
C     This routine adjusts coordinates of interior grid
C     points to make the grid smoother. Two neighbor grid
C     points (L and R) of point P are connected and an 
C     auxilliary point is set on this line at a location
C     corresponding to the position of P between L and R.
C     The new grid point is put midway between the old
C     location P and the auxilliary point. The points are
C     moving inwards towards center of curvature.
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'indexg3d.ing'
      DIMENSION XN(NXYA),YN(NXYA)
C
C.....COPY OLD COORDINATES
C
      DO I=2,NIM-1
        DO J=2,NJM-1
          IJ=LI(I)+J
          XN(IJ)=X(IJ)
          YN(IJ)=Y(IJ)
        END DO
      END DO
C
C.....SMOOTH LINES
C
       DO I=2,NIM-1
        DO J=2,NJM-1
          IJ=LI(I)+J
          IJL=IJ-IND
          IJR=IJ+IND
C
C.....CONNECTIONS IJ <--> IJL AND IJ <--> IJR
C
          TL=SQRT((X(IJ)-X(IJL))**2+(Y(IJ)-Y(IJL))**2)
          TR=SQRT((X(IJ)-X(IJR))**2+(Y(IJ)-Y(IJR))**2)
C
C.....Auxilliary point on line IJL - IJR, modified grid node
C
          FL=TL/((TL+TR)+1.E-20)
          XP=X(IJL)+FL*(X(IJR)-X(IJL))
          YP=Y(IJL)+FL*(Y(IJR)-Y(IJL))
          XN(IJ)=0.5*(X(IJ)+XP)
          YN(IJ)=0.5*(Y(IJ)+YP)
        END DO
      END DO
C
C.....RESET COORDINATES OF GRID NODES
C
      DO I=2,NIM-1
        DO J=2,NJM-1
          IJ=LI(I)+J
          X(IJ)=XN(IJ)
          Y(IJ)=YN(IJ)
        END DO
      END DO
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE GRIDZ
C########################################################
C     This routine was added by G.Usera (12/Oct/2004) for
C     extension of the grid to 3D ( in z direction ).
C
C     Modified by G.Usera (26/Feb/2006) to accomodate
C     surfaces of revolution.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....**************
C.....1. Read input 
C.....**************
C
C.....Number of control volumes in K direction
C
      PRINT *,' ENTER NO. OF CVs IN K DIRECTION:  '
      READ(5,*) NKCV
C
C.....Index control and coversion
C
      NK=NKCV+2
      NKM=NK-1
      NIJ=NI*NJ
      NIJK=NI*NJ*NK
      DO K=1,NK
        LK(K)=(K-1)*NIJ
      END DO
C
C.....Extrusion, Z-data file, or Revolution ?
C
      READ(5,*) IREVOL
C
      IF(IREVOL.EQ.0 ) CALL EXTRUDEZ
C
      IF(IREVOL.EQ.11) CALL TOPOZ
C
      IF(IREVOL.EQ.1 ) CALL REVOLUTZ(IREVOL)
      IF(IREVOL.EQ.2 ) CALL REVOLUTZ(IREVOL)
      IF(IREVOL.EQ.3 ) CALL REVOLUTZ(IREVOL)
C
      IF(IREVOL.EQ.21) THEN
C     ! Do Nothing
      ENDIF
C
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE EXTRUDEZ
C########################################################
C     This routine was added by G.Usera (12/Oct/2004) for
C     extension of the grid to 3D ( in z direction ).
C
C     The 3D grid is constructed from the bottom IJ plane
C     extending vertical lines in the Z (K) direction
C     The bottom plane is also assigned a distribution of
C     Z coordinates ( topography )
C     Vertical distribution of CVs is specified for the
C     (I=1,J=1) corner and extended to the rest of the
C     domain.
C     
C     Three steps are performed : 
C     1. Compute vertical CVs distribution
C     2. Read input, compute Z coord. at bottom IJ plane 
C     3. Compute (X,Y,Z) for all nodes extending 1. & 2.
C      
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....*************************************
C.....1. Compute vertical CVs distribution
C.....*************************************
C
C.....Lines along vertical edge through SW corner
C.....Begin of Line # 1
C
      L=5
      NCF(L)=0      
      PRINT *,' NO. OF LINES ON VERTICAL EDGE:  '
      READ(5,*) NLINES(L)
      PRINT *,' COORDINATES OF BEGIN OF LINE 1:  '
      READ(5,*) XLS
C
C.....Coord. of line end, for each line
C
      DO LL=1,NLINES(L)
        PRINT *,' COORDINATES OF LINE ',LL,'  END:  '
        READ(5,*) XLE
C
C.....Read line specifications
C
   50   PRINT *,' NUMBER OF SEGMENTS, LINE TYPE, BOUNDARY TYPE:  '
        READ(5,*) NSEG,LTYP
        NCF(L)=NCF(L)+NSEG
        IF(NCF(L).GT.NK-2) THEN
          PRINT *,' TOO MANY SEGMENTS - MORE THAN NKCV !!!  '
          PRINT *,' REDUCE NSEG TO MATCH NKCV AND RE-TYPE !!'
          GO TO 50
        ENDIF
        PRINT *,' SIZE OF SEGMENT AT LINE BEGIN, EXPANSION FACTOR:  '
        READ(5,*) DX1,EXP
C
C.....Force line type to straight line (LTYP=1)
C
        LTYP=1
        YLS=0
        YLE=0
C
C.....Vertical edge made only of straight lines. Compute coord. of
C.....grid points along line. XYL=Length(Line) and call DIVLINE
C
        XYL=SQRT((XLE-XLS)**2)
        CALL DIVLINE(XYL)
C
C.....Assign line points to grid points in (auxiliary) edge
C
        I=NIM+1
        J=NJM+1
        KE=NCF(L)+1
        KS=KE-NSEG
        DO K=KS,KE
          IJK=LK(K)+LI(I)+J
          Z(IJK)=XPT(K-KS+1)
        END DO
C
        XLS=XLE
        YLS=YLE
      END DO
C
C.....***************************************************
C.....2. Read input, compute Z coord. at bottom IJ plane 
C.....***************************************************
C
      PRINT *,' NO. OF REGIONS FOR BOTTOM Z COORD. SPECIFICATION:  '
      READ(5,*) NZREGIONS
      DO LL=1,NZREGIONS
        PRINT *,' INDEXES FOR REGION ',LL,'  :  '
        READ(5,*) IS,IE,JS,JE
        READ(5,*) A0,A1,A2,A3,A4,A5,A6
        READ(5,*) B0,B1,B2,B3,B4,B5,B6
C
C.....Compute values of Z within region
C
        DO I=IS,IE
        DO J=JS,JE
          IF((I.LT.(NIM+1)).OR.(J.LT.(NJM+1))) THEN
            IJB=LK(1)+LI(I)+J
            IJT=LK(NKM)+LI(I)+J
            XX=X(IJB)
            YY=Y(IJB)
            Z(IJB)=A0+A1*XX+A2*YY+A3*XX**2+A4*YY**2+A5*XX**3+A6*YY**3
            Z(IJT)=B0+B1*XX+B2*YY+B3*XX**2+B4*YY**2+B5*XX**3+B6*YY**3
          ENDIF
        END DO
        END DO
C
      END DO
C
C.....***************************************
C.....3. Compute (X,Y,Z) for all grid points
C.....***************************************
C
      ZB=Z(LK(1)+LI(NIM+1)+NJM+1)
      ZT=Z(LK(NKM)+LI(NIM+1)+NJM+1)
      DO K=1,NKM
        ZK=Z(LK(K)+LI(NIM+1)+NJM+1)
        FZK=(ZK-ZB)/((ZT-ZB)+1.E-20)
        DO I=1,NIM
          DO J=1,NJM
            IJB=LK(1)+LI(I)+J
            IJK=LK(K)+LI(I)+J
            IJT=LK(NKM)+LI(I)+J
            X(IJK)=X(IJB)
            Y(IJK)=Y(IJB)
            Z(IJK)=(1.-FZK)*Z(IJB)+FZK*Z(IJT)
          END DO
        END DO
      END DO
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE REVOLUTZ(IAXIS)
C########################################################
C     This routine was added by G.Usera (26/Feb/2006) for
C     extension of the grid to 3D by revolution.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'lines3d.ing'
      PINUM=3.1415926535898
      FIP=ATAN(1.)/45.
C
C.....Read input for revolution parameters
C
      READ(5,*) FIS, FIE
      FID=(FIE-FIS)/(NK-2)*FIP
      FIS=FIS*FIP
      READ(5,*) DZ1
C
C.....Apply revolution around X axis
C
      DO I=1,NIM
        DO J=1,NJM
          IJB=LK(1)+LI(I)+J
          RIJ=SQRT(Y(IJB)**2)
          XAUX0=X(IJB)
          DO K=1,NKM
            IJK=LK(K)+LI(I)+J
            XAUX=XAUX0+DZ1*(K-1)
            YAUX=RIJ*COS(FIS+FID*(K-1))
            ZAUX=RIJ*SIN(FIS+FID*(K-1))
              IF(IAXIS.EQ.1) THEN
                X(IJK)=XAUX
                Y(IJK)=YAUX
                Z(IJK)=ZAUX
              ENDIF
              IF(IAXIS.EQ.2) THEN
                X(IJK)=ZAUX
                Y(IJK)=XAUX
                Z(IJK)=YAUX
              ENDIF
              IF(IAXIS.EQ.3) THEN
                X(IJK)=YAUX
                Y(IJK)=ZAUX
                Z(IJK)=XAUX
              ENDIF
          END DO
        END DO
      END DO
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE TOPOZ
C########################################################
C     This routine was added by G.Usera (09/Mar/2006) for
C     extension of the grid to 3D ( in z direction ).
C
C     The 3D grid is constructed from the bottom IJ plane
C     extending vertical lines in the Z (K) direction
C     The bottom and top planes are assigned a distribution of
C     Z coordinates ( topography ) through an ascii data file
C     Vertical distribution of CVs is specified in the file
C     for each IJ point, through ZB, ZT, DX1, EXP
C     
C     Two steps are performed in one single loop : 
C     1. Read ZB, ZT, DX1, EXP from 'name.tpz'
C     2. For each IJ compute Z distribution
C      
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....************************
C.....1. Read 'name.tpz' file
C.....************************
C
C.....Open file
C
      WRITE( FILTPZ,'(A6,4H.tpz)') NAME
      OPEN (UNIT=2,FILE=FILTPZ)
      REWIND 2
C
C.....Loop through IJ nodes
C
      DO I=1,NIM
        DO J=1,NJM
          IJB=LK(1)+LI(I)+J
          IJT=LK(NKM)+LI(I)+J
          READ(2,*) ZB,ZT,DX1,EXP
C
C.....Option for reading Z points from file
C
          IF((DX1.EQ.0.).AND.(EXP.EQ.0.)) THEN
C
C.....Assign line points to grid points in vertical IJ line
C
            DO K=1,NKM
              IJK=LK(K)+LI(I)+J
              X(IJK)=X(IJB)
              Y(IJK)=Y(IJB)
              READ(2,*) ZPOINT
              Z(IJK)=ZPOINT
            END DO
C
          ELSE
C
C.....Force line type to straight line (LTYP=1)
C
            LTYP=1
            YLS=0;  YLE=0
            XLS=ZB; XLE=ZT;
            NSEG=NK-2
C
C.....Vertical lines made only of one straight line each.
C.....Compute coord. of grid points along line.
C.... XYL=Length(Line) and call DIVLINE
C
            XYL=SQRT((XLE-XLS)**2)
            CALL DIVLINE(XYL)
C
C.....Assign line points to grid points in vertical IJ line
C
            DO K=1,NKM
              IJK=LK(K)+LI(I)+J
              X(IJK)=X(IJB)
              Y(IJK)=Y(IJB)
              Z(IJK)=XPT(K)
            END DO
C
          ENDIF
        END DO
      END DO
C
C.....Close file 'name.tpz'
C
      CLOSE(UNIT=2)
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE READBC
C########################################################
C     This routine was added by G.Usera (12/Oct/2004) for
C     extension of the grid to 3D ( in z direction ).
C
C     It reads the boundary condition types.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'lines3d.ing'
C
      PRINT *,' NO. OF REGIONS FOR BOUNDARY CONDITION SPECIFICATION:  '
      READ(5,*) NBREGIONS
      DO LL=1,NBREGIONS
        PRINT *,' INDEXES FOR REGION ',LL,'  :  '
        READ(5,*) IS,IE,JS,JE,KS,KE
        READ(5,*) LBTYP,LBTAG
C
C.....Assign boundary types...West and East
C
        DO K=KS,KE
        DO J=JS,JE
          KJ=(K-1)*NJ+J
          IF(IS.EQ.IE.AND.IS.EQ.1) THEN
            BTYW(KJ)=LBTYP
            BTAW(KJ)=LBTAG
          ENDIF
          IF(IS.EQ.IE.AND.IS.GT.1) THEN
            BTYE(KJ)=LBTYP
            BTAE(KJ)=LBTAG
          ENDIF
        END DO
        END DO
C
C.....Assign boundary types...South and North
C
        DO K=KS,KE
        DO I=IS,IE
          KI=(K-1)*NI+I
          IF(JS.EQ.JE.AND.JS.EQ.1) THEN
            BTYS(KI)=LBTYP
            BTAS(KI)=LBTAG
          ENDIF
          IF(JS.EQ.JE.AND.JS.GT.1) THEN
            BTYN(KI)=LBTYP
            BTAN(KI)=LBTAG
          ENDIF
        END DO
        END DO
C
C.....Assign boundary types...Bottom and Top
C
        DO I=IS,IE
        DO J=JS,JE
          IJ=(I-1)*NJ+J
          IF(KS.EQ.KE.AND.KS.EQ.1) THEN
            BTYB(IJ)=LBTYP
            BTAB(IJ)=LBTAG
          ENDIF
          IF(KS.EQ.KE.AND.KS.GT.1) THEN
            BTYT(IJ)=LBTYP
            BTAT(IJ)=LBTAG
          ENDIF
        END DO
        END DO
C
      END DO
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE PERTURBATEGRID
C########################################################
C     This routine calculates grid data for flow solver.
C
C     Modified by G.Usera (12/Oct/2004) to acomodate 3D
C     grid data computations.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'propcell3d.ing'
C
      PRINT *,' PERTURBATION RADIUS :  '
      READ(5,*) RPO
C
      IF(RPO.GT.0.) THEN
C
      DO K=2,NKM-1
        DO I=2,NIM-1
          DO J=2,NJM-1
            IJK=LK(K)+LI(I)+J
            CALL RANDOM_NUMBER(xrandom)
            xran=xrandom-.5
            CALL RANDOM_NUMBER(xrandom)
            yran=xrandom-.5
            CALL RANDOM_NUMBER(xrandom)
            zran=xrandom-.5
            rran=SQRT(xran**2+yran**2+zran**2)
            IF(rran.LT.1.E-20) THEN
              rran=0.
            ELSE
              rran=1./rran
            ENDIF
            CALL RANDOM_NUMBER(xrandom)
            rran=rran*xrandom
            X(IJK)=X(IJK)+xran*rran*RPO
            Y(IJK)=Y(IJK)+yran*rran*RPO
            Z(IJK)=Z(IJK)+zran*rran*RPO
          END DO
        END DO
      END DO
C
      ENDIF
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE CALCG
C########################################################
C     This routine calculates grid data for flow solver.
C
C     Modified by G.Usera (12/Oct/2004) to acomodate 3D
C     grid data computations.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'propcell3d.ing'
C
C.....Compute node coordinates : cell centers
C
      DO K=2,NKM
      DO I=2,NIM
      DO J=2,NJM
        IJK=LK(K)+LI(I)+J
C
        XC(IJK)=0.125*(X(IJK)+X(IJK-1)+X(IJK-NJ)+X(IJK-NJ-1)+
     *      X(IJK-NIJ)+X(IJK-NIJ-1)+X(IJK-NIJ-NJ)+X(IJK-NIJ-NJ-1))
C
        YC(IJK)=0.125*(Y(IJK)+Y(IJK-1)+Y(IJK-NJ)+Y(IJK-NJ-1)+
     *      Y(IJK-NIJ)+Y(IJK-NIJ-1)+Y(IJK-NIJ-NJ)+Y(IJK-NIJ-NJ-1))  
C
        ZC(IJK)=0.125*(Z(IJK)+Z(IJK-1)+Z(IJK-NJ)+Z(IJK-NJ-1)+
     *      Z(IJK-NIJ)+Z(IJK-NIJ-1)+Z(IJK-NIJ-NJ)+Z(IJK-NIJ-NJ-1))  
      END DO
      END DO
      END DO
C
C
C.....Compute cell volumes in 3D and cell centers. See 8.6.4
C
      NEGVOL=0
      DO K=2,NKM
      DO I=2,NIM
      DO J=2,NJM
        IJK=LK(K)+LI(I)+J
        CALL CALCVOL(IJK,VOL(IJK),XC(IJK),YC(IJK),ZC(IJK))
        IF(VOL(IJK).LT.0.) NEGVOL=1
      END DO
      END DO
      END DO
C
      IF(NEGVOL.EQ.1) PRINT *,'WARNING, THERE ARE NEGATIVE VOLUMES!!!!'
C
C.....Compute node coordinates : Corner (dummy) nodes
C
C.....Bottom-West-South corner
      IJK=LK(1)+LI(1)+1
      XC(IJK)=X(IJK)
      YC(IJK)=Y(IJK)
      ZC(IJK)=Z(IJK)
C
C.....Bottom-West-North corner
      IJK=LK(1)+LI(1)+NJM
      XC(IJK+1)=X(IJK)
      YC(IJK+1)=Y(IJK)
      ZC(IJK+1)=Z(IJK)
C
C.....Bottom-East-South corner
      IJK=LK(1)+LI(NIM)+1
      XC(IJK+NJ)=X(IJK)
      YC(IJK+NJ)=Y(IJK)
      ZC(IJK+NJ)=Z(IJK)
C
C.....Bottom-East-North corner
      IJK=LK(1)+LI(NIM)+NJM
      XC(IJK+NJ+1)=X(IJK)
      YC(IJK+NJ+1)=Y(IJK)
      ZC(IJK+NJ+1)=Z(IJK)
C
C.....Top-West-South corner
      IJK=LK(NKM)+LI(1)+1
      XC(IJK+NIJ)=X(IJK)
      YC(IJK+NIJ)=Y(IJK)
      ZC(IJK+NIJ)=Z(IJK)
C
C.....Top-West-North corner
      IJK=LK(NKM)+LI(1)+NJM
      XC(IJK+NIJ+1)=X(IJK)
      YC(IJK+NIJ+1)=Y(IJK)
      ZC(IJK+NIJ+1)=Z(IJK)
C
C.....Top-East-South corner
      IJK=LK(NKM)+LI(NIM)+1
      XC(IJK+NIJ+NJ)=X(IJK)
      YC(IJK+NIJ+NJ)=Y(IJK)
      ZC(IJK+NIJ+NJ)=Z(IJK)
C
C.....Top-East-North corner
      IJK=LK(NKM)+LI(NIM)+NJM
      XC(IJK+NIJ+NJ+1)=X(IJK)
      YC(IJK+NIJ+NJ+1)=Y(IJK)
      ZC(IJK+NIJ+NJ+1)=Z(IJK)
C
C.....Compute node coordinates : Edge (dummy) nodes
C
      DO K=2,NKM
C.....South-West Edge
        IJKP=LK(K)+LI(1)+1
        IJKB=IJKP
        XC(IJKP)=0.5*(X(IJKB-NIJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NIJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NIJ)+Z(IJKB))
C.....South-East Edge
        IJKP=LK(K)+LI(NI)+1
        IJKB=IJKP-NJ
        XC(IJKP)=0.5*(X(IJKB-NIJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NIJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NIJ)+Z(IJKB))
C.....North-West Edge
        IJKP=LK(K)+LI(1)+NJ
        IJKB=IJKP-1
        XC(IJKP)=0.5*(X(IJKB-NIJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NIJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NIJ)+Z(IJKB))
C.....North-East Edge
        IJKP=LK(K)+LI(NI)+NJ
        IJKB=IJKP-NJ-1
        XC(IJKP)=0.5*(X(IJKB-NIJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NIJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NIJ)+Z(IJKB))
      END DO
C
      DO I=2,NIM
C.....Bottom-South Edge
        IJKP=LK(1)+LI(I)+1
        IJKB=IJKP
        XC(IJKP)=0.5*(X(IJKB-NJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NJ)+Z(IJKB))
C.....Bottom-North Edge
        IJKP=LK(1)+LI(I)+NJ
        IJKB=IJKP-1
        XC(IJKP)=0.5*(X(IJKB-NJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NJ)+Z(IJKB))
C.....Top-South Edge
        IJKP=LK(NK)+LI(I)+1
        IJKB=IJKP-NIJ
        XC(IJKP)=0.5*(X(IJKB-NJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NJ)+Z(IJKB))
C.....Top-North Edge
        IJKP=LK(NK)+LI(I)+NJ
        IJKB=IJKP-NIJ-1
        XC(IJKP)=0.5*(X(IJKB-NJ)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-NJ)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-NJ)+Z(IJKB))
      END DO
C
      DO J=2,NJM
C.....Bottom-West Edge
        IJKP=LK(1)+LI(1)+J
        IJKB=IJKP
        XC(IJKP)=0.5*(X(IJKB-1)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-1)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-1)+Z(IJKB))
C.....Bottom-East Edge
        IJKP=LK(1)+LI(NI)+J
        IJKB=IJKP-NJ
        XC(IJKP)=0.5*(X(IJKB-1)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-1)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-1)+Z(IJKB))
C.....Top-West Edge
        IJKP=LK(NK)+LI(1)+J
        IJKB=IJKP-NIJ
        XC(IJKP)=0.5*(X(IJKB-1)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-1)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-1)+Z(IJKB))
C.....Top-East Edge
        IJKP=LK(NK)+LI(NI)+J
        IJKB=IJKP-NIJ-NJ
        XC(IJKP)=0.5*(X(IJKB-1)+X(IJKB))
        YC(IJKP)=0.5*(Y(IJKB-1)+Y(IJKB))
        ZC(IJKP)=0.5*(Z(IJKB-1)+Z(IJKB))
      END DO
C
C.....Compute node coordinates : Boundary faces
C
C.....East and West Boundaries
      DO K=2,NKM
      DO J=2,NJM
        IJK=LK(K)+LI(NIM)+J
        CALL CALCFACE(IJK,IJK-1,IJK-NIJ-1,IJK-NIJ,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        XC(IJK+NJ)=XXC
        YC(IJK+NJ)=YYC
        ZC(IJK+NJ)=ZZC
C
        IJK=LK(K)+LI(1)+J
        CALL CALCFACE(IJK-1,IJK,IJK-NIJ,IJK-NIJ-1,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        XC(IJK)=XXC
        YC(IJK)=YYC
        ZC(IJK)=ZZC
      END DO
      END DO
C
C.....North and South Boundaries
      DO K=2,NKM
      DO I=2,NIM
        IJK=LK(K)+LI(I)+NJM
        CALL CALCFACE(IJK,IJK-NIJ,IJK-NIJ-NJ,IJK-NJ,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        XC(IJK+1)=XXC
        YC(IJK+1)=YYC
        ZC(IJK+1)=ZZC
C
        IJK=LK(K)+LI(I)+1
        CALL CALCFACE(IJK-NIJ,IJK,IJK-NJ,IJK-NJ-NIJ,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        XC(IJK)=XXC
        YC(IJK)=YYC
        ZC(IJK)=ZZC
      END DO
      END DO
C
C.....Top and Bottom Boundaries
      DO I=2,NIM
      DO J=2,NJM
        IJK=LK(NKM)+LI(I)+J
        CALL CALCFACE(IJK,IJK-NJ,IJK-NJ-1,IJK-1,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        XC(IJK+NIJ)=XXC
        YC(IJK+NIJ)=YYC
        ZC(IJK+NIJ)=ZZC
C
        IJK=LK(1)+LI(I)+J
        CALL CALCFACE(IJK-NJ,IJK,IJK-1,IJK-1-NJ,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        XC(IJK)=XXC
        YC(IJK)=YYC
        ZC(IJK)=ZZC
      END DO
      END DO
C
C.....Compute node coordinates : cell centers
C
c     DO K=2,NKM
c     DO I=2,NIM
c     DO J=2,NJM
c       IJK=LK(K)+LI(I)+J
C
c       XC(IJK)=0.125*(X(IJK)+X(IJK-1)+X(IJK-NJ)+X(IJK-NJ-1)+
c    *      X(IJK-NIJ)+X(IJK-NIJ-1)+X(IJK-NIJ-NJ)+X(IJK-NIJ-NJ-1))
C
c       YC(IJK)=0.125*(Y(IJK)+Y(IJK-1)+Y(IJK-NJ)+Y(IJK-NJ-1)+
c    *      Y(IJK-NIJ)+Y(IJK-NIJ-1)+Y(IJK-NIJ-NJ)+Y(IJK-NIJ-NJ-1))  
C
c       ZC(IJK)=0.125*(Z(IJK)+Z(IJK-1)+Z(IJK-NJ)+Z(IJK-NJ-1)+
c    *      Z(IJK-NIJ)+Z(IJK-NIJ-1)+Z(IJK-NIJ-NJ)+Z(IJK-NIJ-NJ-1))  
c     END DO
c     END DO
c     END DO
C
C 
C==========================================================     
C.....Compute interpolation factors       
C==========================================================     
C
C.....Interpolation in I-direction : FX = Pe/PE
C
      DO K=2,NKM
      DO I=2,NIM-1
      DO J=2,NJM
        IJK=LK(K)+LI(I)+J
        CALL CALCFACE(IJK,IJK-1,IJK-NIJ-1,IJK-NIJ,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        CALL CALCTSE(IJK,IJK+NJ,1,NIJ,XXC,YYC,ZZC,
     *               FEE(IJK),FEN(IJK),FET(IJK))
      END DO
      END DO
      END DO
C
C.....East and West boundary faces
C
      DO K=2,NKM
      DO J=2,NJM
        IJK=LK(K)+LI(1)+J
        FEE(IJK)=0.; FEN(IJK)=0.; FET(IJK)=0.;
        IJK=LK(K)+LI(NIM)+J
        FEE(IJK)=1.; FEN(IJK)=0.; FET(IJK)=0.;
      END DO
      END DO
C
C.....Interpolation in J-direction : FY = Pn/PN
C
      DO K=2,NKM
      DO I=2,NIM
      DO J=2,NJM-1
        IJK=LK(K)+LI(I)+J
        CALL CALCFACE(IJK,IJK-NIJ,IJK-NIJ-NJ,IJK-NJ,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        CALL CALCTSE(IJK,IJK+1,NIJ,NJ,XXC,YYC,ZZC,
     *               FNN(IJK),FNT(IJK),FNE(IJK))
      END DO
      END DO
      END DO
C
C.....North and South boundary faces
C
      DO K=2,NKM
      DO I=2,NIM
        IJK=LK(K)+LI(I)+1
        FNN(IJK)=0.; FNT(IJK)=0.; FNE(IJK)=0.;
        IJK=LK(K)+LI(I)+NJM
        FNN(IJK)=1.; FNT(IJK)=0.; FNE(IJK)=0.;
      END DO
      END DO
C
C.....Interpolation in K-direction : FZ = Pt/PT
C
      DO K=2,NKM-1
      DO I=2,NIM
      DO J=2,NJM
        IJK=LK(K)+LI(I)+J
        CALL CALCFACE(IJK,IJK-NJ,IJK-NJ-1,IJK-1,
     *                XXC,YYC,ZZC,XCR,YCR,ZCR)
        CALL CALCTSE(IJK,IJK+NIJ,NJ,1,XXC,YYC,ZZC,
     *               FTT(IJK),FTE(IJK),FTN(IJK))
      END DO
      END DO
      END DO
C
C.....Top and Bottom boundary faces
C
      DO I=2,NIM
      DO J=2,NJM
        IJK=LK(1)+LI(I)+J
        FTT(IJK)=0.; FTE(IJK)=0.; FTN(IJK)=0.;
        IJK=LK(NKM)+LI(I)+J
        FTT(IJK)=1.; FTE(IJK)=0.; FTN(IJK)=0.;
      END DO
      END DO
C
      RETURN
      END    
C
C
C########################################################
      SUBROUTINE CALCVOL(IJK,XVOL,XXVO,YYVO,ZZVO)
C########################################################
C     This routine calculates the volume of cell IJK.
C
C     Added by G.Usera (12/Oct/2004) to acomodate 3D
C     grid data computations.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'propcell3d.ing'
C
      XXG=XXVO; YYG=YYVO; ZZG=ZZVO;
      XVOL=0.; XXVO=0.; YYVO=0.; ZZVO=0.
C
C.....Compute volume as in 8.6.4.
C
C.....East Face
      IJKB=IJK
      CALL CALCFACE(IJKB,IJKB-1,IJKB-NIJ-1,IJKB-NIJ,
     *              XXC,YYC,ZZC,XCR,YCR,ZCR)
C
      XXC=XXC-XXG; YYC=YYC-YYG; ZZC=ZZC-ZZG;
C
      XVOL=XVOL+XXC*XCR
C
      XVOLP=(1./3.)*(XXC*XCR+YYC*YCR+ZZC*ZCR)
      XXVO=XXVO+0.75*XXC*XVOLP
      YYVO=YYVO+0.75*YYC*XVOLP
      ZZVO=ZZVO+0.75*ZZC*XVOLP
C
C.....West Face
      IJKB=IJK-NJ
      CALL CALCFACE(IJKB-1,IJKB,IJKB-NIJ,IJKB-NIJ-1,
     *                  XXC,YYC,ZZC,XCR,YCR,ZCR)
C
      XXC=XXC-XXG; YYC=YYC-YYG; ZZC=ZZC-ZZG;
C
      XVOL=XVOL+XXC*XCR
C
      XVOLP=(1./3.)*(XXC*XCR+YYC*YCR+ZZC*ZCR)
      XXVO=XXVO+0.75*XXC*XVOLP
      YYVO=YYVO+0.75*YYC*XVOLP
      ZZVO=ZZVO+0.75*ZZC*XVOLP
C
C.....North Face
      IJKB=IJK
      CALL CALCFACE(IJKB,IJKB-NIJ,IJKB-NIJ-NJ,IJKB-NJ,
     *                  XXC,YYC,ZZC,XCR,YCR,ZCR)
C
      XXC=XXC-XXG; YYC=YYC-YYG; ZZC=ZZC-ZZG;
C
      XVOL=XVOL+XXC*XCR
C
      XVOLP=(1./3.)*(XXC*XCR+YYC*YCR+ZZC*ZCR)
      XXVO=XXVO+0.75*XXC*XVOLP
      YYVO=YYVO+0.75*YYC*XVOLP
      ZZVO=ZZVO+0.75*ZZC*XVOLP
C
C.....South Face
      IJKB=IJK-1
      CALL CALCFACE(IJKB-NIJ,IJKB,IJKB-NJ,IJKB-NJ-NIJ,
     *                  XXC,YYC,ZZC,XCR,YCR,ZCR)
C
      XXC=XXC-XXG; YYC=YYC-YYG; ZZC=ZZC-ZZG;
C
      XVOL=XVOL+XXC*XCR
C
      XVOLP=(1./3.)*(XXC*XCR+YYC*YCR+ZZC*ZCR)
      XXVO=XXVO+0.75*XXC*XVOLP
      YYVO=YYVO+0.75*YYC*XVOLP
      ZZVO=ZZVO+0.75*ZZC*XVOLP
C
C.....Top Face
      IJKB=IJK
      CALL CALCFACE(IJKB,IJKB-NJ,IJKB-NJ-1,IJKB-1,
     *                  XXC,YYC,ZZC,XCR,YCR,ZCR)
C
      XXC=XXC-XXG; YYC=YYC-YYG; ZZC=ZZC-ZZG;
C
      XVOL=XVOL+XXC*XCR
C
      XVOLP=(1./3.)*(XXC*XCR+YYC*YCR+ZZC*ZCR)
      XXVO=XXVO+0.75*XXC*XVOLP
      YYVO=YYVO+0.75*YYC*XVOLP
      ZZVO=ZZVO+0.75*ZZC*XVOLP
C
C.....Bottom Face
      IJKB=IJK-NIJ
      CALL CALCFACE(IJKB-NJ,IJKB,IJKB-1,IJKB-1-NJ,
     *                  XXC,YYC,ZZC,XCR,YCR,ZCR)
C
      XXC=XXC-XXG; YYC=YYC-YYG; ZZC=ZZC-ZZG;
C
      XVOL=XVOL+XXC*XCR
C
      XVOLP=(1./3.)*(XXC*XCR+YYC*YCR+ZZC*ZCR)
      XXVO=XXVO+0.75*XXC*XVOLP
      YYVO=YYVO+0.75*YYC*XVOLP
      ZZVO=ZZVO+0.75*ZZC*XVOLP
C
      XXVO=XXVO/XVOL
      YYVO=YYVO/XVOL
      ZZVO=ZZVO/XVOL
C
C.....Shift back to absolute origin
C
      XXVO=XXVO+XXG; YYVO=YYVO+YYG; ZZVO=ZZVO+ZZG
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE CALCFACE(IJK1,IJK2,IJK3,IJK4,
     *                    XXXC,YYYC,ZZZC,XXCR,YYCR,ZZCR)
C########################################################
C     This routine calculates the surface vector and
C     centre of cell IJK.
C
C     Added by G.Usera (12/Oct/2004) to acomodate 3D
C     grid data computations.
C
C     Modified by G.Usera (Mar/2006) to improve computation
C     by averaging both posible sets of triangles.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
C
      CALL CALCFACE1(IJK1,IJK2,IJK3,IJK4,
     *               XXXC1,YYYC1,ZZZC1,XXCR1,YYCR1,ZZCR1)
C
      CALL CALCFACE1(IJK2,IJK3,IJK4,IJK1,
     *               XXXC2,YYYC2,ZZZC2,XXCR2,YYCR2,ZZCR2)
C
      XXXC=0.5*(XXXC1+XXXC2)
      YYYC=0.5*(YYYC1+YYYC2)
      ZZZC=0.5*(ZZZC1+ZZZC2)
      XXCR=0.5*(XXCR1+XXCR2)
      YYCR=0.5*(YYCR1+YYCR2)
      ZZCR=0.5*(ZZCR1+ZZCR2)
C
      RETURN
      END
C
C########################################################
      SUBROUTINE CALCFACE1(IJK1,IJK2,IJK3,IJK4,
     *                     XXXC,YYYC,ZZZC,XXCR,YYCR,ZZCR)
C########################################################
C     This routine calculates the surface vector and
C     centre of cell IJK.
C
C     Added by G.Usera (12/Oct/2004) to acomodate 3D
C     grid data computations.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
C
C.....Vectors to vertices 
      DX12=X(IJK2)-X(IJK1)
      DY12=Y(IJK2)-Y(IJK1)
      DZ12=Z(IJK2)-Z(IJK1)
C
      DX13=X(IJK3)-X(IJK1)
      DY13=Y(IJK3)-Y(IJK1)
      DZ13=Z(IJK3)-Z(IJK1)
C
      DX14=X(IJK4)-X(IJK1)
      DY14=Y(IJK4)-Y(IJK1)
      DZ14=Z(IJK4)-Z(IJK1)
C
C.....Cross Products for triangle surface vectors
      XR23=DY12*DZ13-DZ12*DY13
      YR23=DZ12*DX13-DX12*DZ13
      ZR23=DX12*DY13-DY12*DX13
C
      XR34=DY13*DZ14-DZ13*DY14
      YR34=DZ13*DX14-DX13*DZ14
      ZR34=DX13*DY14-DY13*DX14
C
C.....Face surface vectors
      XXCR=0.5*(XR23+XR34)
      YYCR=0.5*(YR23+YR34)
      ZZCR=0.5*(ZR23+ZR34)
C
C.....Baricenters of triangles
C
C.....Shift origin to X(IJK1),Y(IJK1),Z(IJK1)
C.....to prevent round of error. Added by G.Usera. Mar/2006
C
      DX12=(X(IJK1)+X(IJK2)+X(IJK3))/3.-X(IJK1)
      DY12=(Y(IJK1)+Y(IJK2)+Y(IJK3))/3.-Y(IJK1)
      DZ12=(Z(IJK1)+Z(IJK2)+Z(IJK3))/3.-Z(IJK1)
C
      DX14=(X(IJK1)+X(IJK3)+X(IJK4))/3.-X(IJK1)
      DY14=(Y(IJK1)+Y(IJK3)+Y(IJK4))/3.-Y(IJK1)
      DZ14=(Z(IJK1)+Z(IJK3)+Z(IJK4))/3.-Z(IJK1)
C
C.....Area of triangles
      S23=SQRT(XR23**2+YR23**2+ZR23**2)
      S34=SQRT(XR34**2+YR34**2+ZR34**2)
C
C.....Baricenter of face
      XXXC=(DX12*S23+DX14*S34)/((S23+S34)+1.E-20)
      YYYC=(DY12*S23+DY14*S34)/((S23+S34)+1.E-20)
      ZZZC=(DZ12*S23+DZ14*S34)/((S23+S34)+1.E-20)
C
C.....Shift origin back to absolute (0,0,0)
C
      XXXC=XXXC+X(IJK1); YYYC=YYYC+Y(IJK1); ZZZC=ZZZC+Z(IJK1);
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE CALCTSE(IJKP,IJKE,NLN,NLT,XXXC,YYYC,ZZZC,
     *                   FGME,FGMN,FGMT)
C########################################################
C     This routine calculates the interpolation factors
C     following ideas of Lehnhauser and Schafer, 2002.
C
C     Added by G.Usera (15/Mar/2005) to acomodate 3D
C     grid data computations.
C
C========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
C
      DIMENSION PE(1,3),SN(1,3),BT(1,3),Pc(1,3)
C
      IJKN=IJKP+NLN; IJKS=IJKP-NLN
      IJKT=IJKP+NLT; IJKB=IJKP-NLT
C
      PE(1,1)=XC(IJKP)-XC(IJKE)
      PE(1,2)=YC(IJKP)-YC(IJKE)
      PE(1,3)=ZC(IJKP)-ZC(IJKE)
      SN(1,1)=XC(IJKS)-XC(IJKN)
      SN(1,2)=YC(IJKS)-YC(IJKN)
      SN(1,3)=ZC(IJKS)-ZC(IJKN)
      BT(1,1)=XC(IJKB)-XC(IJKT)
      BT(1,2)=YC(IJKB)-YC(IJKT)
      BT(1,3)=ZC(IJKB)-ZC(IJKT)
      Pc(1,1)=XC(IJKP)-XXXC
      Pc(1,2)=YC(IJKP)-YYYC
      Pc(1,3)=ZC(IJKP)-ZZZC
C
      CALL MIXED3(PE,SN,BT,THETA)
      CALL MIXED3(Pc,SN,BT,FGME)
      CALL MIXED3(PE,Pc,BT,FGMN)
      CALL MIXED3(PE,SN,Pc,FGMT)
C
      FGME=FGME/(THETA+1.E-20)
      FGMN=FGMN/(THETA+1.E-20)
      FGMT=FGMT/(THETA+1.E-20)
C
      RETURN
      END
C
C########################################################
      SUBROUTINE CROSS3(V3,W3,Z3)
C########################################################
C     This routine computes the cross product
C     between 3D vectors V3 and W3, into Z3
C
C     Added by G.Usera (15/Mar/2005) to acomodate
C     TSE computations. (copied from block3d.MB:CROSS3 )
C
C========================================================
      DIMENSION V3(1,3),W3(1,3),Z3(1,3)
C
C.....Cross product
      Z3(1,1)=V3(1,2)*W3(1,3)-V3(1,3)*W3(1,2)
      Z3(1,2)=V3(1,3)*W3(1,1)-V3(1,1)*W3(1,3)
      Z3(1,3)=V3(1,1)*W3(1,2)-V3(1,2)*W3(1,1)
C
      RETURN
      END
C
C########################################################
      SUBROUTINE MIXED3(U3,V3,W3,XZ)
C########################################################
C     This routine computes the mixed product
C     between 3D vectors U3, V3 and W3, into XZ
C
C     Added by G.Usera (15/Mar/2005) to acomodate
C     TSE computations.
C
C========================================================
      DIMENSION U3(1,3),V3(1,3),W3(1,3),Z3(1,3)
C
C.....Cross product
      CALL CROSS3(V3,W3,Z3)
C
C.....Scalar product
      XZ=SUM(U3*Z3)
C
      RETURN
      END
C

C############################################################
      SUBROUTINE SETBC
C############################################################
C     This routine collects information about boundary cell
C     faces of the same type.
C============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'bound3d.ing'
C
C.....SEARCH GRID FOR BOUNDARIES OF GIVEN TYPE
C
      NINL=0
      NOUT=0
      NSYM=0
      NPRU=0
      NWAL=0
      NOC=0
      NWALI=0
      NWALA=0
      NTRG=0
C
C.....INLET BOUNDARIES
C
      CALL DEFBC(1,NINL,ITAGI,IJI,IJPI,IJI1,IJI2,IJI3,IJI4)
C
C.....OUTLET BOUNDARIES
C
      CALL DEFBC(2,NOUT,ITAGO,IJO,IJPO,IJO1,IJO2,IJO3,IJO4)
C
C.....SYMMETRIY BOUNDARIES
C
      CALL DEFBC(3,NSYM,ITAGS,IJS,IJPS,IJS1,IJS2,IJS3,IJS4)
C
C.....ISOTHERMAL WALLS
C
      CALL DEFBC(4,NWAL,ITAGW,IJW,IJPW,IJW1,IJW2,IJW3,IJW4)
      NWALI=NWAL
C
C.....ADIABATIC WALLS
C
      CALL DEFBC(5,NWAL,ITAGW,IJW,IJPW,IJW1,IJW2,IJW3,IJW4)
      NWALA=NWAL-NWALI
C
C.....FLUX-SPECIFIED WALLS
C
      CALL DEFBC(6,NWAL,ITAGW,IJW,IJPW,IJW1,IJW2,IJW3,IJW4)
      NWALF=NWAL-NWALI-NWALA
C
C.....PRESSURE BOUNDARIES
C
      CALL DEFBC(8,NPRU,ITAGU,IJU,IJPU,IJU1,IJU2,IJU3,IJU4)
C
C.....INTERPOLATION BOUNDARIES ( Nested Overlapping Grids )
C
      CALL DEFBC(9,NTRG,ITAGG,IJG,IJPG,IJG1,IJG2,IJG3,IJG4)
C
C.....INTERFACE BOUNDARIES  ( Intra-Block or Inter-block )
C
      CALL DEFBC(10,NOC,ITAGOC,IJR,IJL,IJOC1,IJOC2,IJOC3,IJOC4)
C
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE DEFBC(LT,NB,IBTAG,IJB,IJBP,IJ1,IJ2,IJ3,IJ4)
C############################################################
C     This routine prepares data about boundary conditions
C     as required by the flow solver.
C
C     Modified by G.Usera (12/Oct/2004) for extension to 3D
C
C============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'bound3d.ing'
      INCLUDE 'lines3d.ing'
C
      DIMENSION IBTAG(NXYZA),IJB(NXYZA),IJBP(NXYZA),
     *          IJ1(NXYZA),IJ2(NXYZA),IJ3(NXYZA),IJ4(NXYZA)
C
C.....COLLECT BOUNDARY CELL FACES OF TYPE 'LT' IN A LIST 
C
C.....West side
      DO K=2,NKM
      DO J=2,NJM
        KJ=(K-1)*NJ+J
        IF(BTYW(KJ).EQ.LT) THEN
          NB=NB+1
          IJB(NB) =LK(K)+LI(1)+J
          IJBP(NB)=IJB(NB)+NJ
          IJ1(NB) =IJB(NB)-1
          IJ2(NB) =IJ1(NB)+1
          IJ3(NB) =IJ2(NB)-NIJ
          IJ4(NB) =IJ3(NB)-1
          IBTAG(NB)=BTAW(KJ)
        ENDIF
      END DO
      END DO
C
C.....East side
      DO K=2,NKM
      DO J=2,NJM
        KJ=(K-1)*NJ+J
        IF(BTYE(KJ).EQ.LT) THEN
          NB=NB+1
          IJB(NB) =LK(K)+LI(NI)+J
          IJBP(NB)=IJB(NB)-NJ
          IJ1(NB) =IJB(NB)-NJ
          IJ2(NB) =IJ1(NB)-1
          IJ3(NB) =IJ2(NB)-NIJ
          IJ4(NB) =IJ3(NB)+1
          IBTAG(NB)=BTAE(KJ)
        ENDIF
      END DO
      END DO
C
C.....South side
      DO K=2,NKM
      DO I=2,NIM
        KI=(K-1)*NI+I
        IF(BTYS(KI).EQ.LT) THEN
          NB=NB+1
          IJB(NB) =LK(K)+LI(I)+1
          IJBP(NB)=IJB(NB)+1
          IJ1(NB) =IJB(NB)-NIJ
          IJ2(NB) =IJ1(NB)+NIJ
          IJ3(NB) =IJ2(NB)-NJ
          IJ4(NB) =IJ3(NB)-NIJ
          IBTAG(NB)=BTAS(KI)
        ENDIF
      END DO
      END DO
C
C.....North side
      DO K=2,NKM
      DO I=2,NIM
        KI=(K-1)*NI+I
        IF(BTYN(KI).EQ.LT) THEN
          NB=NB+1
          IJB(NB) =LK(K)+LI(I)+NJ
          IJBP(NB)=IJB(NB)-1
          IJ1(NB) =IJB(NB)-1
          IJ2(NB) =IJ1(NB)-NIJ
          IJ3(NB) =IJ2(NB)-NJ
          IJ4(NB) =IJ3(NB)+NIJ
          IBTAG(NB)=BTAN(KI)
        ENDIF
      END DO
      END DO
C
C.....Bottom side
      DO I=2,NIM
      DO J=2,NJM
        IJ=(I-1)*NJ+J
        IF(BTYB(IJ).EQ.LT) THEN
          NB=NB+1
          IJB(NB) =LK(1)+LI(I)+J
          IJBP(NB)=IJB(NB)+NIJ
          IJ1(NB) =IJB(NB)-NJ
          IJ2(NB) =IJ1(NB)+NJ
          IJ3(NB) =IJ2(NB)-1
          IJ4(NB) =IJ3(NB)-NJ
          IBTAG(NB)=BTAB(IJ)
        ENDIF
      END DO
      END DO
C
C.....Top side
      DO I=2,NIM
      DO J=2,NJM
        IJ=(I-1)*NJ+J
        IF(BTYT(IJ).EQ.LT) THEN
          NB=NB+1
          IJB(NB) =LK(NK)+LI(I)+J
          IJBP(NB)=IJB(NB)-NIJ
          IJ1(NB) =IJB(NB)-NIJ
          IJ2(NB) =IJ1(NB)-NJ
          IJ3(NB) =IJ2(NB)-1
          IJ4(NB) =IJ3(NB)+NJ
          IBTAG(NB)=BTAT(IJ)
        ENDIF
      END DO
      END DO
C
      RETURN
      END
C
C               
C###########################################################
      SUBROUTINE SODW
C###########################################################
C     This routine calculates components of the unit vector
C     normal to wall boundary faces and area of that face
C     divided by the distance of cell center to the wall.
C
C     Modified by G.Usera (13/Oct/2004) for extension to 3D
C
C===========================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'bound3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'propcell3d.ing'
C
      DO IW=1,NWAL
        IJB=IJW(IW);  IJP=IJPW(IW)
        IJ1=IJW1(IW); IJ2=IJW2(IW)
        IJ3=IJW3(IW); IJ4=IJW4(IW)
C
C.....Compute components of the surface normal vector
C
        CALL CALCFACE(IJ1,IJ2,IJ3,IJ4,XXC,YYC,ZZC,XCR,YCR,ZCR)
        XNW(IW)=XCR ; YNW(IW)=YCR; ZNW(IW)=ZCR
        AR=SQRT(XCR**2+YCR**2+ZCR**2)
C
C.....Compute face normal distance from face center to cell center
C
        DN=(XC(IJB)-XC(IJP))*XCR+(YC(IJB)-YC(IJP))*YCR+               
     *     (ZC(IJB)-ZC(IJP))*ZCR
C
C.....Cell face area divided by distance to the cell center
C
        SRDW(IW)=(AR**2)/(DN+1.E-20)
C
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE SODS
C#############################################################
C     This routine calculates components of the unit vector
C     parallel to symmetry boundary faces and area of that 
C     face divided by the distance of cell center to boundary.
C
C     Modified by G.Usera (13/Oct/2004) for extension to 3D
C
C=============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'bound3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'propcell3d.ing'
C
      DO IS=1,NSYM
        IJB=IJS(IS);  IJP=IJPS(IS)
        IJ1=IJS1(IS); IJ2=IJS2(IS)
        IJ3=IJS3(IS); IJ4=IJS4(IS)
C
C.....Compute components of the surface normal vector
C
        CALL CALCFACE(IJ1,IJ2,IJ3,IJ4,XXC,YYC,ZZC,XCR,YCR,ZCR)
        XNS(IS)=XCR; YNS(IS)=YCR; ZNS(IS)=ZCR
        AR=SQRT(XCR**2+YCR**2+ZCR**2)
C
C.....Compute face normal distance from face center to cell center
C
        DN=(XC(IJB)-XC(IJP))*XCR+(YC(IJB)-YC(IJP))*YCR+               
     *     (ZC(IJB)-ZC(IJP))*ZCR
C
C.....Cell face area divided by distance to the cell center
C
        SRDS(IS)=(AR**2)/(DN+1.E-20)
C
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE PERMUTECOORD
C#############################################################
C     This routine performs coordinates permutations
C     as requested in the *.gin file
C
C     Added by G.Usera (Apr/2005) 
C
C=============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'bound3d.ing'
      INCLUDE 'lines3d.ing'
      DIMENSION NPERMUTE(3)
      DIMENSION XYZV(3)
C
C.....READ COORDINATES ORDERING VECTOR
C
      PRINT *,' ENTER COORDINATE ORDER (1 2 3):  '
      READ(5,*) (NPERMUTE(I),I=1,3)
C
      DO IJK=1,NIJK
        XYZV=(/  X(IJK),  Y(IJK),  Z(IJK)/)
        XYZV=XYZV(NPERMUTE)
          X(IJK)=XYZV(1);   Y(IJK)=XYZV(2);   Z(IJK)=XYZV(3)
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE OFFSETORIGIN
C#############################################################
C     This routine offsets the coordinate origin
C     as requested in the *.gin file
C
C     Added by G.Usera (Apr/2005) 
C
C=============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'bound3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....READ COORDINATES ORIGIN OFFSET 
C
      PRINT *,' ENTER ORIGIN OFFSET :  '
      READ(5,*) XO,YO,ZO
C
      X =X +XO; Y = Y+YO; Z =Z +ZO;
c     XC=XC+XO; YC=YC+YO; ZC=ZC+ZO;
C
      PRINT *,' ENTER SCALE FACTOR :  '
      READ(5,*) xFAC
      X=X*xFAC; Y=Y*xFAC; Z=Z*xFAC
C 
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE ROTATECOORD
C#############################################################
C     This routine rotates the grid around the coordinate
C     axis, with center in the origin
C
C     Added by G.Usera (Jul/2009) 
C
C=============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'logico3d.ing'
      INCLUDE 'grid3d.ing'
      INCLUDE 'bound3d.ing'
      INCLUDE 'lines3d.ing'
C
C.....READ COORDINATES ORIGIN OFFSET 
C
      PRINT *,' ENTER ROTATION ANGLES (DEG) :  '
      READ(5,*) XO,YO,ZO
      PII=4*ATAN(1.)
C
C.....ROTATE AROUND X AXIS
C
      XO=XO/360.*2.*PII
      DO IJK=1,NIJK
        YAUX=Y(IJK)
        ZAUX=Z(IJK)
        Y(IJK)=YAUX*COS(XO)-ZAUX*SIN(XO)
        Z(IJK)=ZAUX*COS(XO)+YAUX*SIN(XO)
      END DO
C.....ROTATE AROUND X AXIS
C
      YO=YO/360.*2.*PII
      DO IJK=1,NIJK
        ZAUX=Z(IJK)
        XAUX=X(IJK)
        Z(IJK)=ZAUX*COS(YO)-XAUX*SIN(YO)
        X(IJK)=XAUX*COS(YO)+ZAUX*SIN(YO)
      END DO
C
C.....ROTATE AROUND Z AXIS
C
      ZO=ZO/360.*2.*PII
      DO IJK=1,NIJK
        XAUX=X(IJK)
        YAUX=Y(IJK)
        X(IJK)=XAUX*COS(ZO)-YAUX*SIN(ZO)
        Y(IJK)=YAUX*COS(ZO)+XAUX*SIN(ZO)
      END DO
C 
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE READXY
C#############################################################
C     This routine added to read (x,y) coords from
C     file (allows external generation of the grid)
C
C     Added by G.Usera (Set/2007)
C
C=============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
C
C.....********************
C.....Read 'name.pxy' file
C.....********************
C
C.....Open file
C
      WRITE( FILTPZ,'(A6,4H.pxy)') NAME
      OPEN (UNIT=2,FILE=FILTPZ)
      REWIND 2
C
C.....Read parameters and build indexes
C
C.....NUMBER OF CONTROL VOLUMES IN I AND J DIRECTION
C
      READ(2,*) NICV,NJCV
C
C.....INDEX CONTROL AND CONVERSION
C
      NI=NICV+2
      NJ=NJCV+2
      NIM=NI-1
      NJM=NJ-1
      DO I=1,NI
        LI(I)=(I-1)*NJ
      END DO
C
C.....Read data
C
      DO I=1,NIM
        DO J=1,NJM
          IJ=LI(I)+J
          READ(2,*) XDAT,YDAT
          X(IJ)=XDAT
          Y(IJ)=YDAT
        END DO
      END DO
C
C.....Close file 'name.pxy'
C
      CLOSE(UNIT=2)
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE READXYZ
C#############################################################
C     This routine added to read (x,y,z) coords from
C     file (allows external generation of the grid)
C
C     Added by G.Usera (May/2013)
C
C=============================================================
      INCLUDE 'param3d.ing'
      INCLUDE 'indexg3d.ing'
      INCLUDE 'grid3d.ing'
C
C.....********************
C.....Read 'name.pxy' file
C.....********************
C
C.....Open file
C
      WRITE( FILTPZ,'(A6,4H.pxy)') NAME
      OPEN (UNIT=2,FILE=FILTPZ)
      REWIND 2
C
C.....Read parameters and build indexes
C
C.....NUMBER OF CONTROL VOLUMES IN I AND J DIRECTION
C
      READ(2,*) NICV,NJCV,NKCV
C
C.....INDEX CONTROL AND CONVERSION
C
      NI=NICV+2
      NJ=NJCV+2
      NIM=NI-1
      NJM=NJ-1
      DO I=1,NI
        LI(I)=(I-1)*NJ
      END DO
C
      NK=NKCV+2
      NKM=NK-1
      NIJ=NI*NJ
      NIJK=NI*NJ*NK
      DO K=1,NK
        LK(K)=(K-1)*NIJ
      END DO
C
C.....Read data
C
      DO K=1,NKM
      DO I=1,NIM
        DO J=1,NJM
          IJ=LK(K)+LI(I)+J
          READ(2,*) XDAT,YDAT,ZDAT
          X(IJ)=XDAT
          Y(IJ)=YDAT
          Z(IJ)=ZDAT
        END DO
      END DO
      END DO
C
C.....Close file 'name.pxy'
C
      CLOSE(UNIT=2)
C
      RETURN
      END
C
C

