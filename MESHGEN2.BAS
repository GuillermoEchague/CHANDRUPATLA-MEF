     '**************  PROGRAM MESHGEN2  **************
     '*  MESH GENERATOR FOR TWO DIMENSIONAL REGIONS  *
     '*     (C) T.R.CHANDRUPATLA & A.D.BELEGUNDU     *
     '************************************************
     DEFINT I-N: CLS : COLOR 1, 3
     LOCATE 1, 1: PRINT " MESH GENERATOR"; SPACE$(36);
     PRINT "(C)  Chandrupatla & Belegundu": : COLOR 7, 0
     NDIM = 2
     VIEW PRINT 2 TO 25
     DO
        CLS : LOCATE 3, 30: PRINT "TYPE OF ELEMENTS"
        LOCATE 5, 25: PRINT "1. Triangular Elements";
        LOCATE 6, 25: PRINT "2. Quadrilateral Elements";
        LOCATE 8, 25: PRINT "Your Choice < 1 or 2 > ";
        INPUT "", NTMP
        IF NTMP = 1 OR NTMP = 2 THEN EXIT DO
     LOOP
     IF NTMP = 1 THEN NEN = 3 ELSE NEN = 4
     CLS
     LOCATE 15, 1: FOR I = 1 TO 80: PRINT CHR$(196); : NEXT I
     LOCATE 16, 1: PRINT "Hints: * A region is divided into 4-cornered";
     PRINT " blocks viewed as a mapping";
     LOCATE 17, 10: PRINT "from a Checkerboard pattern of S- ";
     PRINT "and W- Sides"; : LOCATE 18, 8
     PRINT "* S- Side is one with lower number of final divisions"; ""
     LOCATE 19, 8: PRINT "* Blocks, Corners, S- and W- Sides are";
     PRINT " labeled as shown in Fig. 12.2"; : LOCATE 20, 8
     PRINT "* Make a sketch and identify void blocks and merging sides";
     LOCATE 3, 32: PRINT "BLOCK DATA";
     LOCATE 5, 25: INPUT "Number of S - Spans = ", NS
     LOCATE 6, 25: INPUT "Number of W - Spans = ", NW
     LOCATE 7, 25: INPUT "Number of Pairs of Sides to be Merged = ", NSJ
     NSW = NS * NW: NGN = (NS + 1) * (NW + 1): NM = 1
     DIM IDBLK(NSW), NSD(NS), NWD(NW), NGCN(NGN)
     '------------- Span Divisions ---------------
     VIEW PRINT 16 TO 25: CLS
     LOCATE 16, 1
     PRINT "Hint:  * Provide number of divisions for each S-Span";
     VIEW PRINT 2 TO 14: CLS
     LOCATE 3, 32: PRINT "SPAN DIVISIONS";
     LOCATE 5, 23: PRINT "S-DIRECTION";
     LOCATE 6, 25: PRINT "S-Span#  Num-Divisions";
     VIEW PRINT 7 TO 14
     NNS = 1: NNW = 1
     FOR KS = 1 TO NS
     PRINT SPACE$(25); KS; SPACE$(9);
     INPUT "", NSD(KS)
     NNS = NNS + NSD(KS): NEXT KS
     VIEW PRINT 16 TO 25: CLS
     LOCATE 16, 1
     PRINT "Hint:  * Provide number of divisions for each W-Span";
     VIEW PRINT 5 TO 14: CLS
     LOCATE 5, 23: PRINT "W-DIRECTION";
     LOCATE 6, 25: PRINT "W-Span#  Num-Divisions";
     VIEW PRINT 7 TO 14
     FOR KW = 1 TO NW
     PRINT SPACE$(25); KW; SPACE$(9);
     INPUT "", NWD(KW)
     NNW = NNW + NWD(KW): NEXT KW
     VIEW PRINT 2 TO 14: CLS
     LOCATE 3, 32: PRINT "BLOCK MATERIAL DATA";
     LOCATE 5, 22: PRINT "Block#   Material Number (Void = 0)"
     VIEW PRINT 16 TO 25: CLS
     LOCATE 16, 1: PRINT "Hints: * Block number = 0 exits this mode";
     LOCATE 17, 8: PRINT "* Provide Material Number for those blocks with";
     LOCATE 18, 10: PRINT "material number other than  1"; : LOCATE 19, 8
     PRINT "* The material number for a void block is  0 (ZERO)";
     LOCATE 20, 8: PRINT "* Block number must be <= "; NSW;
     VIEW PRINT 6 TO 14
     '-------- Block Identifier / Material# --------
     FOR I = 1 TO NSW: IDBLK(I) = 1: NEXT I
     DO
        PRINT SPACE$(23); : ILIN = CSRLIN: INPUT "", NTMP
        IF ILIN = 14 THEN ILIN = 13
        IF NTMP = 0 THEN EXIT DO
        IF NTMP < 0 OR NTMP > NSW THEN
           LOCATE ILIN, 1: PRINT SPACE$(80); : LOCATE ILIN, 1
        ELSE
           LOCATE ILIN, 35
           INPUT "", IDBLK(NTMP)
           IF NM < IDBLK(NTMP) THEN NM = IDBLK(NTMP)
        END IF
     LOOP
     '-----------------   Block Corner Data  ---------------
     NSR = NS * (NW + 1): NWR = NW * (NS + 1)
     DIM XB(NGN, 2), SR(NSR, 2), WR(NWR, 2)
     VIEW PRINT 2 TO 14: CLS
     LOCATE 3, 32: PRINT "BLOCK CORNER DATA";
     LOCATE 5, 22: PRINT "Corner#   X-Coordinate   Y-Coordinate"
     VIEW PRINT 16 TO 25: CLS
     LOCATE 16, 1: PRINT "Hints: * Corner number = 0 exits this mode";
     LOCATE 17, 8: PRINT "* X and Y are the coordinates of the corner";
     LOCATE 18, 10: PRINT "location in the 2-D Region. See Fig.12.2";
     LOCATE 19, 10: PRINT "for corner numbering.": LOCATE 20, 8
     PRINT "* Void block corners outside the region are NOT needed";
     VIEW PRINT 6 TO 14
     DO
        PRINT SPACE$(23); : ILIN = CSRLIN: INPUT "", NTMP
        IF ILIN = 14 THEN ILIN = 13
        IF NTMP = 0 THEN EXIT DO
        LOCATE ILIN, 34: INPUT "", XB(NTMP, 1)
        LOCATE ILIN, 49: INPUT "", XB(NTMP, 2)
     LOOP
     '---------- Evaluate Mid-points of S-Sides -------------
     FOR I = 1 TO NW + 1: FOR J = 1 TO NS
     IJ = (I - 1) * NS + J
     SR(IJ, 1) = .5 * (XB(IJ + I - 1, 1) + XB(IJ + I, 1))
     SR(IJ, 2) = .5 * (XB(IJ + I - 1, 2) + XB(IJ + I, 2))
     NEXT J: NEXT I
     '---------- Evaluate Mid-points of W-Sides -------------
     FOR I = 1 TO NW: FOR J = 1 TO NS + 1
     IJ = (I - 1) * (NS + 1) + J
     WR(IJ, 1) = .5 * (XB(IJ, 1) + XB(IJ + NS + 1, 1))
     WR(IJ, 2) = .5 * (XB(IJ, 2) + XB(IJ + NS + 1, 2))
     NEXT J: NEXT I
     '------ Input Locations of Points on Sides ------
     VIEW PRINT 2 TO 14: CLS
     LOCATE 3, 32: PRINT "MID-POINT DATA";
     LOCATE 5, 22: PRINT "S-Side#   X-Coordinate   Y-Coordinate"
     VIEW PRINT 16 TO 25: CLS
     LOCATE 16, 1: PRINT "Hints: * S-Side number = 0 exits this mode";
     LOCATE 17, 8: PRINT "* The mid-point data is needed for those sides";
     PRINT " which are curved";
     LOCATE 18, 10: PRINT "or divisions are not equal";
     LOCATE 19, 8: PRINT "* X and Y are the coordinates of mid-side";
     PRINT " point of the 2-D Region"; : LOCATE 20, 8
     PRINT "* S-Side number is as shown in Fig. 12.2";
     VIEW PRINT 6 TO 14
     DO
        PRINT SPACE$(23); : ILIN = CSRLIN: INPUT "", NTMP
        IF ILIN = 14 THEN ILIN = 13
        IF NTMP = 0 THEN EXIT DO
        LOCATE ILIN, 34: INPUT "", SR(NTMP, 1)
        LOCATE ILIN, 49: INPUT "", SR(NTMP, 2)
     LOOP
     VIEW PRINT 2 TO 14: CLS
     LOCATE 3, 32: PRINT "MID-POINT DATA";
     LOCATE 5, 22: PRINT "W-Side#   X-Coordinate   Y-Coordinate"
     VIEW PRINT 16 TO 25: CLS
     LOCATE 16, 1: PRINT "Hints: * W-Side number = 0 exits this mode";
     LOCATE 17, 8: PRINT "* The mid-point data is needed for those sides";
     PRINT " which are curved";
     LOCATE 18, 10: PRINT "or divisions are not equal";
     LOCATE 19, 8: PRINT "* X and Y are the coordinates of mid-side";
     PRINT " point of the 2-D Region"; : LOCATE 20, 8
     PRINT "* S-Side number is as shown in Fig. 12.2";
     VIEW PRINT 6 TO 14
     DO
        PRINT SPACE$(23); : ILIN = CSRLIN: INPUT "", NTMP
        IF ILIN = 14 THEN ILIN = 13
        IF NTMP = 0 THEN EXIT DO
        LOCATE ILIN, 34: INPUT "", WR(NTMP, 1)
        LOCATE ILIN, 49: INPUT "", WR(NTMP, 2)
     LOOP
     '------------------ Merging Sides ----------------------
     IF NSJ > 0 THEN
        DIM MERG(NSJ, 4)
        VIEW PRINT 2 TO 14: CLS
        LOCATE 3, 32: PRINT "MERGING SIDES";
        LOCATE 5, 22: PRINT "           SIDE1          SIDE2";
        LOCATE 6, 22: PRINT "PAIR#  Node1  Node2    Node1  Nodes";
        VIEW PRINT 16 TO 25: CLS
        LOCATE 16, 1: PRINT "Hints: * SIDE1 is the side with lower node";
        PRINT " numbers"; : LOCATE 17, 8: PRINT "* The data is to be";
        PRINT " provided for number of merging pairs = "; NSJ
        VIEW PRINT 7 TO 14
        FOR I = 1 TO NSJ
           ILIN = CSRLIN:   PRINT SPACE$(21); I;
           IF ILIN = 14 THEN ILIN = 13
           LOCATE ILIN, 30: INPUT "", L1
           LOCATE ILIN, 37: INPUT "", L2
           I1 = L1: I2 = L2: GOSUB SIDEDIV: II1 = IDIV
           LOCATE ILIN, 46: INPUT "", L3
           LOCATE ILIN, 53: INPUT "", L4
           I1 = L3: I2 = L4: GOSUB SIDEDIV: II2 = IDIV
           IF II1 <> II2 THEN
              PRINT "#Div don't match, Check Merge data ": END
           END IF
           MERG(I, 1) = L1: MERG(I, 2) = L2
           MERG(I, 3) = L3: MERG(I, 4) = L4
        NEXT I
     END IF
     VIEW PRINT 2 TO 25: CLS
     '------- Global Node Locations of Corner Nodes ---------
     NTMPI = 1
     FOR I = 1 TO NW + 1
     IF I = 1 THEN IINC = 0 ELSE IINC = NNS * NWD(I - 1)
     NTMPI = NTMPI + IINC: NTMPJ = 0
     FOR J = 1 TO NS + 1
     IJ = (NS + 1) * (I - 1) + J
     IF J = 1 THEN JINC = 0 ELSE JINC = NSD(J - 1)
     NTMPJ = NTMPJ + JINC
     NGCN(IJ) = NTMPI + NTMPJ: NEXT J: NEXT I
     '---------------- Node Point Array --------------------
     NNT = NNS * NNW
     DIM NNAR(NNT)
     FOR I = 1 TO NNT
     NNAR(I) = -1: NEXT I
     '--------- Zero Non-Existing Node Locations ---------
     FOR KW = 1 TO NW: FOR KS = 1 TO NS
     KSW = NS * (KW - 1) + KS
     IF IDBLK(KSW) > 0 GOTO STEP1
     '-------- Operation within an Empty Block --------
     K1 = (KW - 1) * (NS + 1) + KS: N1 = NGCN(K1)
     NS1 = 2: IF KS = 1 THEN NS1 = 1
     NW1 = 2: IF KW = 1 THEN NW1 = 1
     NS2 = NSD(KS) + 1
     IF KS < NS THEN
        IF IDBLK(KSW + 1) > 0 THEN NS2 = NSD(KS)
     END IF
     NW2 = NWD(KW) + 1
     IF KW < NW THEN
        IF IDBLK(KSW + NS) > 0 THEN NW2 = NWD(KW)
     END IF
     FOR I = NW1 TO NW2: IN1 = N1 + (I - 1) * NNS
     FOR J = NS1 TO NS2: IJ = IN1 + J - 1
     NNAR(IJ) = 0: NEXT J: NEXT I
     IF NS2 = NSD(KS) OR NW2 = NWD(KW) GOTO STEP1
     IF KS = NS OR KW = NW GOTO STEP1
     IF IDBLK(KSW + NS + 1) > 0 THEN NNAR(IJ) = -1
STEP1:
     NEXT KS: NEXT KW
     '--------  Node Identification for Side Merging ------
     IF NSJ > 0 THEN
        FOR I = 1 TO NSJ
        I1 = MERG(I, 1): I2 = MERG(I, 2): GOSUB SIDEDIV
        IA1 = NGCN(I1): IA2 = NGCN(I2): IASTP = (IA2 - IA1) / IDIV
        I1 = MERG(I, 3): I2 = MERG(I, 4): GOSUB SIDEDIV
        IB1 = NGCN(I1): IB2 = NGCN(I2): IBSTP = (IB2 - IB1) / IDIV
        IAA = IA1 - IASTP
        FOR IBB = IB1 TO IB2 STEP IBSTP
        IAA = IAA + IASTP
        IF IBB = IAA THEN NNAR(IAA) = -1 ELSE NNAR(IBB) = IAA
        NEXT IBB: NEXT I
     END IF
     '----------  Final Node Numbers in the Array  --------
     NODE = 0
     FOR I = 1 TO NNT
        IF NNAR(I) > 0 THEN
           II = NNAR(I): NNAR(I) = NNAR(II)
        ELSEIF NNAR(I) < 0 THEN
           NODE = NODE + 1: NNAR(I) = NODE
        END IF
     NEXT I
     '------------  Nodal Coordinates  ---------------
     NN = NODE: NELM = 0
     DIM X(NN, 2), XP(8, 2), NOC(2 * NNT, NEN), MAT(2 * NNT)
     FOR KW = 1 TO NW: FOR KS = 1 TO NS
     KSW = NS * (KW - 1) + KS
     IF IDBLK(KSW) = 0 GOTO STEP3
     '---------  Extraction of Block Data  ----------
     NODW = NGCN(KSW + KW - 1) - NNS - 1
     FOR JW = 1 TO NWD(KW) + 1
     ETA = -1 + 2 * (JW - 1) / NWD(KW)
     NODW = NODW + NNS: NODS = NODW
     FOR JS = 1 TO NSD(KS) + 1
     XI = -1 + 2 * (JS - 1) / NSD(KS)
     NODS = NODS + 1: NODE = NNAR(NODS)
     GOSUB BLOCKXY: GOSUB SHAPE
     FOR J = 1 TO 2: C1 = 0: FOR I = 1 TO 8
     C1 = C1 + SH(I) * XP(I, J): NEXT I
     X(NODE, J) = C1: NEXT J
     '--------------------  Connectivity  -----------------------
     IF JS = NSD(KS) + 1 OR JW = NWD(KW) + 1 GOTO STEP2
     N1 = NODE: N2 = NNAR(NODS + 1)
     N4 = NNAR(NODS + NNS): N3 = NNAR(NODS + NNS + 1)
     NELM = NELM + 1
     IF NEN = 3 THEN
     '------------------- Triangular Elements -------------------
        NOC(NELM, 1) = N1: NOC(NELM, 2) = N2
        NOC(NELM, 3) = N3: MAT(NELM) = IDBLK(KSW)
        NELM = NELM + 1: NOC(NELM, 1) = N3: NOC(NELM, 2) = N4
        NOC(NELM, 3) = N1: MAT(NELM) = IDBLK(KSW)
     ELSE
     '------------------- Quadrilateral Elements ----------------
        NOC(NELM, 1) = N1: NOC(NELM, 2) = N2: MAT(NELM) = IDBLK(KSW)
        NOC(NELM, 3) = N3: NOC(NELM, 4) = N4
     END IF
STEP2:
     NEXT JS: NEXT JW
STEP3:
     NEXT KS: NEXT KW
     NE = NELM
     IF NEN = 3 THEN
     '--------- Readjustment for Triangle Connectivity ----------
        NE2 = NE / 2
        FOR I = 1 TO NE2
           I1 = 2 * I - 1: N1 = NOC(I1, 1): N2 = NOC(I1, 2)
           N3 = NOC(I1, 3): N4 = NOC(2 * I, 2)
           X13 = X(N1, 1) - X(N3, 1): Y13 = X(N1, 2) - X(N3, 2)
           X24 = X(N2, 1) - X(N4, 1): Y24 = X(N2, 2) - X(N4, 2)
           IF (X13 * X13 + Y13 * Y13) > 1.1 * (X24 * X24 + Y24 * Y24) THEN
              NOC(I1, 3) = N4: NOC(2 * I, 3) = N2
           END IF
        NEXT I
     END IF
     GOSUB SAVDAT
     END
SIDEDIV:
     '===========  Number of Divisions  for Side I1,I2  ===========
     IMIN = I1: IMAX = I2: IF IMIN > I2 THEN IMIN = I2: IMAX = I1
     IF (IMAX - IMIN) = 1 THEN IDIV = NGCN(IMAX) - NGCN(IMIN): RETURN
     IDIV = (NGCN(IMAX) - NGCN(IMIN)) / NNS
     RETURN
BLOCKXY:
     '======  Coordinates of 8-Nodes of the Block  ======
     N1 = KSW + KW - 1
     XP(1, 1) = XB(N1, 1): XP(1, 2) = XB(N1, 2)
     XP(3, 1) = XB(N1 + 1, 1): XP(3, 2) = XB(N1 + 1, 2)
     XP(5, 1) = XB(N1 + NS + 2, 1): XP(5, 2) = XB(N1 + NS + 2, 2)
     XP(7, 1) = XB(N1 + NS + 1, 1): XP(7, 2) = XB(N1 + NS + 1, 2)
     XP(2, 1) = SR(KSW, 1): XP(2, 2) = SR(KSW, 2)
     XP(6, 1) = SR(KSW + NS, 1): XP(6, 2) = SR(KSW + NS, 2)
     XP(8, 1) = WR(N1, 1): XP(8, 2) = WR(N1, 2)
     XP(4, 1) = WR(N1 + 1, 1): XP(4, 2) = WR(N1 + 1, 2)
     RETURN
SHAPE:
     '==============  Shape Functions  ================
     SH(1) = -(1 - XI) * (1 - ETA) * (1 + XI + ETA) / 4
     SH(2) = (1 - XI * XI) * (1 - ETA) / 2
     SH(3) = -(1 + XI) * (1 - ETA) * (1 - XI + ETA) / 4
     SH(4) = (1 - ETA * ETA) * (1 + XI) / 2
     SH(5) = -(1 + XI) * (1 + ETA) * (1 - XI - ETA) / 4
     SH(6) = (1 - XI * XI) * (1 + ETA) / 2
     SH(7) = -(1 - XI) * (1 + ETA) * (1 + XI - ETA) / 4
     SH(8) = (1 - ETA * ETA) * (1 - XI) / 2
     RETURN
SAVDAT:
'========================  SAVE DATA  =========================
     LOCATE 4, 1
     INPUT "File Name for Saving Data <path fn.dat> ", FILE2$
     OPEN FILE2$ FOR OUTPUT AS #1
     NDIM = 2: NDN = 2
     INPUT "Problem Title = ", TITLE$
     STTL$(1) = "    'Next Line is the Problem Title"
     STTL$(2) = "NN  NE  NM  NDIM  NEN  NDN "
     STTL$(3) = "ND   NL   NCH   NPR  NMPC"
     STTL$(4) = "Node#   X    Y"
     STTL$(5) = "Elem#  Node1  Node2  Node3"
     IF NEN = 3 THEN STTL$(5) = STTL$(5) + "  Material#"
     IF NEN = 4 THEN STTL$(5) = STTL$(5) + "  Node4  Material#"
     PRINT #1, STTL$(1): PRINT #1, TITLE$
     PRINT #1, STTL$(2): PRINT #1, NN; NE; NM; NDIM; NEN; NDN
     PRINT #1, STTL$(3): PRINT #1, ND; NL; NCH; NPR; NMPC
     PRINT #1, STTL$(4)
     FOR I = 1 TO NN: PRINT #1, I;
        FOR J = 1 TO NDIM: PRINT #1, X(I, J); : NEXT J
        PRINT #1, : NEXT I
     PRINT #1, STTL$(5)
     FOR I = 1 TO NE: PRINT #1, I;
        FOR J = 1 TO NEN: PRINT #1, NOC(I, J); : NEXT J
        PRINT #1, MAT(I): NEXT I
     CLOSE #1
     PRINT "Data has been stored in the file "; FILE2$
     RETURN
    

