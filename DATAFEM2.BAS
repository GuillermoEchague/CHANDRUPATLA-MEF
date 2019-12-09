     '**************  PROGRAM DATAFEM2  **************
     '*   DATA HANDLER FOR FINITE ELEMENT ANALYSIS   *
     '*     (C) T.R.CHANDRUPATLA & A.D.BELEGUNDU     *
     '************************************************
     'Note: This program supports upto 8 nodes per element
     DEFINT I-N: CLS : COLOR 1, 3
     LOCATE 1, 1: PRINT "FEA DATA HANDLER"; SPACE$(35);
     PRINT "(C)  Chandrupatla & Belegundu": : COLOR 7, 0
     DIM STTL$(9)
     PNUM$ = "0123456789-.EeDd": F1$ = "####": F2$ = " ####.#### "
     F3$ = "     #     ": F4$ = " #.####^^^^"
     LOCATE 7, 34: PRINT "CREATE or EDIT";
     LOCATE 10, 30: PRINT "1. Create New Data"
     LOCATE 11, 30: PRINT "2. Edit Data"
     LOCATE 12, 30: PRINT "3. Edit Data from MESHGEN"
     LOCATE 14, 25: PRINT "Your Choice <1,2,3>"
     DO: LOCATE 14, 47: PRINT "    "; : LOCATE 14, 47: INPUT "", IFL1
     LOOP WHILE IFL1 < 1 OR IFL1 > 3
     VIEW PRINT 2 TO 25: CLS
     IF IFL1 = 2 OR IFL1 = 3 THEN
        LOCATE 3, 35: PRINT "DATA FILE NAME";
        LOCATE 4, 30: IF IFL1 = 3 THEN PRINT "AND OTHER ARRAY SIZE DATA";
        LOCATE 6, 25: INPUT "Edit File Name <path fn.ext> ", FILE1$
        ON ERROR GOTO STEP1
        OPEN FILE1$ FOR INPUT AS #1
        LINE INPUT #1, STTL$(1)
        INPUT #1, TITLE$
        LINE INPUT #1, STTL$(2)
        INPUT #1, NN, NE, NM, NDIM, NEN, NDN
        LINE INPUT #1, STTL$(3)
        INPUT #1, ND, NL, NCH, NPR, NMPC
     END IF
     IF IFL1 = 1 THEN
        LOCATE 3, 27: PRINT "TITLE AND ARRAY SIZE DATA"
        LOCATE 5, 22: INPUT "Problem Title .............. ", TITLE$
        LOCATE 6, 22: INPUT "Number of Nodes ................... ", NN
        LOCATE 7, 22: INPUT "Number of Elements ................ ", NE
        LOCATE 8, 22: INPUT "Number of Materials ............... ", NM
        LOCATE 9, 22: INPUT "Dimension of the Problem <1,2,3>... ", NDIM
        LOCATE 10, 22: INPUT "Number of Nodes per Element ....... ", NEN
        LOCATE 11, 22: INPUT "Number of DOF per Node ............ ", NDN
     END IF
     IF IFL1 = 1 OR IFL1 = 3 THEN
        LOCATE 12, 22: INPUT "Number of Constrained DOF ......... ", ND
        LOCATE 13, 22: INPUT "Number of Component Loads ......... ", NL
        LOCATE 21, 4
        PRINT "Note: Element Characterisics e.g. Area, Thickness, ";
        PRINT "Mom_Inertia, Temp etc.";
        LOCATE 22, 4: PRINT "NCH to be input: FEM1D(2) TRUSS(2) CST(2)";
        PRINT " AXISYM(1) QUAD(2) BEAM(1)";
        LOCATE 23, 6: PRINT "FRAME2D(3) FRAME3D(6) TETRA(1) HEXA(1) ";
        PRINT "HEAT2D(1) BEAMKM(2) CSTKM(2)";
        NCHMAX = INT((70 - 5 * NEN) / 12)
        LOCATE 24, 8: PRINT "If NCH > "; NCHMAX;
        PRINT " use NCH = "; NCHMAX; " and edit data file later";
        LOCATE 14, 22: INPUT "Number of Element Characteristics.. ", NCH
        VIEW PRINT 21 TO 25: CLS : VIEW PRINT 2 TO 25
        LOCATE 22, 4: PRINT "NPR to be input: FEM1D(2) TRUSS(2) CST(3)";
        PRINT " AXISYM(3) QUAD(3) BEAM(1)";
        LOCATE 23, 6: PRINT "FRAME2D(1) FRAME3D(1) TETRA(3) HEXA(3) ";
        PRINT "HEAT2D(1) BEAMKM(2) CSTKM(3)";
        LOCATE 15, 22: INPUT "Number of Properties per Material.. ", NPR
        VIEW PRINT 21 TO 25: CLS : VIEW PRINT 2 TO 25
        LOCATE 21, 10: PRINT "Note: Constraints of the type  " + CHR$(225);
        PRINT "1*Qi+" + CHR$(225) + "2*Qj = " + CHR$(225) + "0";
        LOCATE 16, 22: INPUT "Number of Multi-point Constraints.. ", NMPC
        VIEW PRINT 21 TO 25: CLS : VIEW PRINT 2 TO 25
     END IF
     VIEW PRINT
     DIM X(NN, NDIM), NOC(NE, NEN), NU(ND), U(ND), NF(NL), F(NL)
     DIM MAT(NE), PM(NM, NPR), CHAR(NE, NCH)
     IF NMPC > 0 THEN DIM MPC(NMPC, 2), BT(NMPC, 3)
     IF IFL1 = 2 OR IFL1 = 3 THEN GOSUB GETDAT
     IF IFL1 = 1 THEN                         'Default Material# is 1
        FOR I = 1 TO NE: MAT(I) = 1: NEXT I
     END IF
     DIM SC$(6), JCOL(11), NWID(6, 11), CTL$(6, 11), FCOL$(6), ISIZ(6)
     GOSUB SCDATA
EDIT:
     ISCR = 1: IF IFL1 = 3 THEN ISCR = 3
     IST1 = 1: IMAX = ISIZ(ISCR): GOSUB NXT1
     DO
        A$ = INKEY$
        IF A$ = CHR$(27) THEN GOSUB ENTRY1                   'escape
        IF A$ = CHR$(13) THEN GOSUB PRLOC                    'return
        IF A$ = CHR$(0) + CHR$(72) THEN GOSUB UP             'up
        IF A$ = CHR$(0) + CHR$(80) THEN GOSUB DOWN           'down
        IF A$ = CHR$(0) + CHR$(75) THEN GOSUB LEFT           'left
        IF A$ = CHR$(0) + CHR$(77) THEN GOSUB RIGHT          'right
        IF A$ = CHR$(8) THEN GOSUB ENTRY3                    'Backspace
        IF A$ = CHR$(0) + CHR$(73) THEN GOSUB PGUP           'Page Up
        IF A$ = CHR$(0) + CHR$(81) THEN GOSUB PGDN           'Page Down
        IF A$ = CHR$(0) + CHR$(59) THEN GOSUB F1             'F1
        IF A$ = CHR$(0) + CHR$(60) THEN GOSUB F2             'F2
        IF A$ = CHR$(0) + CHR$(68) THEN EXIT DO              'F10
        GOSUB ENTRY2
     LOOP
     COLOR 7, 0: CLS : COLOR 1, 3
     LOCATE 1, 1: PRINT "FEA DATA HANDLER"; SPACE$(35);
     PRINT "(C)  Chandrupatla & Belegundu": : COLOR 7, 0
     LOCATE 7, 30: PRINT "EDIT / SAVE"
     LOCATE 9, 25: PRINT "1. Goto EDIT Mode";
     LOCATE 10, 25: PRINT "2. <SAVE DATA and EXIT>";
     LOCATE 11, 25: PRINT "3. EXIT without saving";
     LOCATE 13, 25: PRINT "Your Choice <1,2,3> ";
     DO: LOCATE 13, 47: PRINT "    ": LOCATE 13, 47: INPUT "", IFL2
     LOOP WHILE IFL2 < 1 OR IFL2 > 3
     IF IFL2 = 1 THEN
        GOTO EDIT
     ELSEIF IFL2 = 2 THEN
        LOCATE 15, 22
        INPUT "File Name for Saving Data <path fn.ext> ", FILE2$
        GOSUB SAVDAT
     END IF
     END
UP:
     IDR = IDR - 1
     IF IDR < IST1 THEN IDR = IST2
     IROW = IDR - IST1 + 5: GOSUB PRLOC
     RETURN
DOWN:
     IDR = IDR + 1
     IF IDR > IST2 THEN IDR = IST1
     IROW = IDR - IST1 + 5: GOSUB PRLOC
     RETURN
LEFT:
     IDC = IDC - 1
     IF IDC < 1 THEN IDC = JCOL(ISCR)
     ICOL = 6: I1 = 0
     FOR J = 1 TO IDC: ICOL = ICOL + I1: I1 = NWID(ISCR, J): NEXT J
     GOSUB PRLOC
     RETURN
RIGHT:
     IDC = IDC + 1
     IF IDC > JCOL(ISCR) THEN IDC = 1
     ICOL = 6: I1 = 0
     FOR J = 1 TO IDC: ICOL = ICOL + I1: I1 = NWID(ISCR, J): NEXT J
     GOSUB PRLOC
     RETURN
PGUP:
     IF IST1 - 20 > 0 THEN
        IST1 = IST1 - 20
     ELSE
        I1 = IMAX MOD 20: IF I1 = 0 THEN I1 = 20
        IST1 = IMAX - I1 + 1
     END IF
     GOSUB VALUE: GOSUB NXT1
     RETURN
PGDN:
     IF IST1 + 20 < IMAX THEN IST1 = IST1 + 20 ELSE IST1 = 1
     GOSUB VALUE: GOSUB NXT1
     RETURN
F1:
     GOSUB VALUE
     DO
        ISCR = ISCR + 1
        IF ISCR > 6 THEN ISCR = 1
        IMAX = ISIZ(ISCR): IF IMAX > 0 THEN EXIT DO
     LOOP
     IST1 = 1: GOSUB NXT1
     RETURN
F2:
     GOSUB VALUE
     DO
        ISCR = ISCR - 1
        IF ISCR < 1 THEN ISCR = 6
        IMAX = ISIZ(ISCR): IF IMAX > 0 THEN EXIT DO
     LOOP
     IST1 = 1: IMAX = ISIZ(ISCR): GOSUB NXT1
     RETURN
VALUE:
     IF X$ = "" THEN
        C = COLD: FA$ = FOLD$
     ELSE
        C = VAL(X$): GOSUB ENTRY1
        II = IDRO: JJ = IDCO
        GOSUB ASSIGN
     END IF
     RETURN
PRLOC:
     GOSUB VALUE
     '----- Print at Old Location
     COLOR 7, 0: LOCATE IRO, 1: PRINT USING F1$; IDRO;
     LOCATE 3, ICO: PRINT CTL$(ISCR, IDCO);
     LOCATE IRO, ICO: PRINT USING FA$; C;
     '----- Print at New Location
     II = IDR: JJ = IDC
     GOSUB ENTITY
     COLOR 0, 3: LOCATE IROW, 1: PRINT USING F1$; IDR;
     LOCATE 3, ICOL: PRINT CTL$(ISCR, IDC);
     LOCATE IROW, ICOL: PRINT USING FA$; C; : COLOR 7, 0
     IRO = IROW: ICO = ICOL: IDRO = IDR: IDCO = IDC
     FOLD$ = FA$: COLD = C
     RETURN
NXT1:
     IDCO = 1: IDRO = IST1: IRO = 5: ICO = 6
     IDC = 1: IDR = IST1: IROW = 5: ICOL = 6
     COLOR 7, 0
     GOSUB PRSCR
     GOSUB SCR0
     GOSUB SCRA
     GOSUB ENTRY1: COLOR 7, 0
     RETURN
SCR0:
     IST2 = IST1 + 19
     IF IST2 > IMAX THEN IST2 = IMAX
     FOR I = 1 TO 20
        LOCATE I + 4, 1
        IF IST1 + I - 1 > IMAX THEN
           PRINT SPACE$(4);
        ELSE
           PRINT USING F1$; IST1 + I - 1
        END IF
     NEXT I
     RETURN
SCRA:
   J1 = 0: JC = 6
   FOR J = 1 TO JCOL(ISCR)
     JC = JC + J1
     J1 = NWID(ISCR, J)
     FOR I = 1 TO 20
        ISTI = IST1 + I - 1
        IR = I + 4
        LOCATE IR, JC
        IF ISTI <= IMAX THEN
           II = ISTI: JJ = J
           GOSUB ENTITY
           PRINT USING FA$; C;
           IF I = 1 AND J = 1 THEN
              COLD = C: FOLD$ = FA$
           END IF
        ELSE
           PRINT FB$;
        END IF
     NEXT I
   NEXT J
   COLOR 0, 3: LOCATE IRO, 1: PRINT USING F1$; IDRO;
   LOCATE 3, ICO: PRINT CTL$(ISCR, IDCO);
   LOCATE IRO, ICO: PRINT USING FOLD$; COLD; : COLOR 7, 0
RETURN
ENTITY:
    IF ISCR = 1 THEN
       C = X(II, JJ): GOSUB FMT
    ELSEIF ISCR = 2 THEN
       IF JJ <= NEN THEN C = NOC(II, JJ): FA$ = F1$: FB$ = "    "
       IF JJ = NEN + 1 THEN C = MAT(II): FA$ = F1$: FB$ = "    "
       IF JJ > NEN + 1 THEN C = CHAR(II, JJ - NEN - 1): GOSUB FMT
    ELSEIF ISCR = 3 THEN
       IF JJ = 1 THEN C = NU(II): FA$ = F1$: FB$ = "    "
       IF JJ = 2 THEN C = U(II): GOSUB FMT
    ELSEIF ISCR = 4 THEN
       IF JJ = 1 THEN C = NF(II): FA$ = F1$: FB$ = "    "
       IF JJ = 2 THEN C = F(II): GOSUB FMT
    ELSEIF ISCR = 5 THEN
       C = PM(II, JJ): GOSUB FMT
    ELSE
       IF JJ = 1 THEN C = BT(II, 1): GOSUB FMT
       IF JJ = 2 THEN C = MPC(II, 1): FA$ = F1$: FB$ = "    "
       IF JJ = 3 THEN C = BT(II, 2): GOSUB FMT
       IF JJ = 4 THEN C = MPC(II, 2): FA$ = F1$: FB$ = "    "
       IF JJ = 5 THEN C = BT(II, 3): GOSUB FMT
    END IF
    RETURN
ASSIGN:
    IF ISCR = 1 THEN
       X(II, JJ) = C: GOSUB FMT
    ELSEIF ISCR = 2 THEN
       IF JJ <= NEN THEN NOC(II, JJ) = C: FA$ = F1$: FB$ = "    "
       IF JJ = NEN + 1 THEN MAT(II) = C: FA$ = F1$: FB$ = "    "
       IF JJ > NEN + 1 THEN CHAR(II, JJ - NEN - 1) = C: GOSUB FMT
    ELSEIF ISCR = 3 THEN
       IF JJ = 1 THEN NU(II) = C: FA$ = F1$: FB$ = "    "
       IF JJ = 2 THEN U(II) = C: GOSUB FMT
    ELSEIF ISCR = 4 THEN
       IF JJ = 1 THEN NF(II) = C: FA$ = F1$: FB$ = "    "
       IF JJ = 2 THEN F(II) = C: GOSUB FMT
    ELSEIF ISCR = 5 THEN
       PM(II, JJ) = C: GOSUB FMT
    ELSE
       IF JJ = 1 THEN BT(II, 1) = C: GOSUB FMT
       IF JJ = 2 THEN MPC(II, 1) = C: FA$ = F1$: FB$ = "    "
       IF JJ = 3 THEN BT(II, 2) = C: GOSUB FMT
       IF JJ = 4 THEN MPC(II, 2) = C: FA$ = F1$: FB$ = "    "
       IF JJ = 5 THEN BT(II, 3) = C: GOSUB FMT
    END IF
    RETURN
FMT:
    FA$ = F2$
    FB$ = "           "
    IF ABS(C) > 999 OR ABS(C) < .001 THEN FA$ = F4$
    IF C = 0 THEN FA$ = F3$
    RETURN
ENTRY1:
     X$ = ""
     COLOR 0, 7: LOCATE 2, 1: PRINT SPACE$(80);
     LOCATE 2, 1: PRINT "Enter Data"; : COLOR 16, 7: PRINT " >>";
     COLOR 0, 7
     LOCATE 2, 15
     RETURN
ENTRY2:
     FOR I = 1 TO 16
        IF A$ = MID$(PNUM$, I, 1) THEN
           X$ = X$ + A$
           EXIT FOR
        END IF
     NEXT I
     COLOR 0, 7: LOCATE 2, 15: PRINT X$;
     IF X$ <> "" THEN C = VAL(X$)
     RETURN
ENTRY3:
     L = LEN(X$): COLOR 0, 7: LOCATE 2, 15: PRINT SPACE$(L);
     IF L > 1 THEN
        X$ = LEFT$(X$, L - 1)
     ELSE
        X$ = ""
     END IF
     LOCATE 2, 15: PRINT X$;
     IF X$ <> "" THEN C = VAL(X$)
     RETURN
STEP1:
     NUMBER = ERR
     IF NUMBER = 53 THEN
        LOCATE 8, 18
        PRINT "<<< FILE NOT FOUND CHECK DIRECTORY AND FILE NAME >>>"
     END IF
     END
     RETURN
SCDATA:
     '===============  READ SCREEN & FORMAT DATA  ================
     '----- Column Widths and Column Titles -----
     FOR I = 1 TO 6: FOR J = 1 TO 11: NWID(I, J) = 0: NEXT J: NEXT I
     SC$(1) = "1. COORDINATES": ISIZ(1) = NN: JCOL(1) = NDIM
         FOR J = 1 TO JCOL(1): NWID(1, J) = 12: NEXT J
         FCOL$(1) = "NOD#": CTL$(1, 1) = "     X     "
         CTL$(1, 2) = "     Y     ": CTL$(1, 3) = "     Z     "
     SC$(2) = "2. CONNECTIVITY & ELEMENT CHARACTERISTICS"
         ISIZ(2) = NE: JCOL(2) = NEN + 1 + NCH
         FOR J = 1 TO NEN + 1: NWID(2, J) = 5: NEXT J
         FOR J = NEN + 2 TO NEN + 1 + NCH: NWID(2, J) = 12: NEXT J
         FCOL$(2) = "ELE#"
         FOR J = 1 TO NEN
            CTL$(2, J) = " N" + RIGHT$(STR$(J), 1) + " "
         NEXT J
         CTL$(2, NEN + 1) = "MAT#"
         FOR J = 1 TO NCH
            CTL$(2, NEN + 1 + J) = "   Char" + RIGHT$(STR$(J), 1) + "   "
         NEXT J
     SC$(3) = "3. SPECIFIED DOF": ISIZ(3) = ND: JCOL(3) = 2
         NWID(3, 1) = 5: NWID(3, 2) = 12
         FCOL$(3) = "Ser#": CTL$(3, 1) = "DOF#"
         CTL$(3, 2) = "   Displ.  "
     SC$(4) = "4. LOADS": ISIZ(4) = NL: JCOL(4) = 2
         NWID(4, 1) = 5: NWID(4, 2) = 12
         FCOL$(4) = "Ser#": CTL$(4, 1) = "DOF#"
         CTL$(4, 2) = "   Force   "
     SC$(5) = "5. MATERIAL PROPERTIES": ISIZ(5) = NM: JCOL(5) = NPR
         FOR J = 1 TO NPR: NWID(5, J) = 12: NEXT J
         FCOL$(5) = "Mat#"
         FOR J = 1 TO NPR
            CTL$(5, J) = "   Prop" + RIGHT$(STR$(J), 1) + "   "
         NEXT J
     SC$(6) = "6. MULTI-POINT CONSTRAINTS  " + CHR$(225) + "1*Qi+"
         SC$(6) = SC$(6) + CHR$(225) + "2+Qj=" + CHR$(225) + "0"
         ISIZ(6) = NMPC: JCOL(6) = 5
         NWID(6, 1) = 12: NWID(6, 2) = 5: NWID(6, 3) = 12
         NWID(6, 4) = 5: NWID(6, 5) = 12
         FCOL$(6) = "Ser#": CTL$(6, 1) = "    " + CHR$(225) + "1     "
         CTL$(6, 2) = " i  "
         CTL$(6, 3) = "    " + CHR$(225) + "2    ": CTL$(6, 4) = " j  "
         CTL$(6, 5) = "    " + CHR$(225) + "0    "
     RETURN
PRSCR:
'===============  SCREEN FOR DATA HANDLING  =================
     CLS
     COLOR 1, 3
     LOCATE 1, 1: PRINT SPACE$(80); : LOCATE 1, 1: PRINT SC$(ISCR);
     LOCATE 1, 50: PRINT "(C)  Chandrupatla & Belegundu"; : COLOR 7, 0
     LOCATE 4, 1: FOR I = 1 TO 78: PRINT CHR$(196); : NEXT I
     FOR I = 3 TO 24
        LOCATE I, 5: PRINT CHR$(179);
        JWID = 5
        FOR J = 1 TO JCOL(ISCR)
           JWID = JWID + NWID(ISCR, J)
           LOCATE I, JWID: PRINT CHR$(179);
        NEXT J
     NEXT I
     LOCATE 3, 1: PRINT FCOL$(ISCR);
     IADD = 5: I1 = 1
     FOR J = 1 TO JCOL(ISCR)
        I1 = I1 + IADD
        LOCATE 3, I1: PRINT CTL$(ISCR, J);
        IADD = NWID(ISCR, J)
     NEXT J
     COLOR 0, 7: LOCATE 25, 1
     PRINT "<"; CHR$(24); CHR$(25); CHR$(26); CHR$(27); " navigate>";
     PRINT " <Pg-Up/Pg-Dn>";
     PRINT " <F1/F2 Flip Data sets>  "; " < F10 - Exit Data Entry >";
     COLOR 7, 0
     IF IFL1 = 1 OR IFL1 = 3 THEN
        STTL$(1) = "    'Next Line is the Problem Title"
        STTL$(2) = "NN  NE  NM  NDIM  NEN  NDN "
        STTL$(3) = "ND   NL   NCH   NPR  NMPC"
        STTL$(4) = "Node#"
        FOR J = 1 TO JCOL(1): STTL$(4) = STTL$(4) + CTL$(1, J): NEXT J
        STTL$(5) = "Elem#"
        FOR J = 1 TO JCOL(2): STTL$(5) = STTL$(5) + CTL$(2, J): NEXT J
        STTL$(6) = "DOF#  Displacement": STTL$(7) = "DOF#  Load"
        STTL$(8) = "MAT#"
        FOR J = 1 TO JCOL(5): STTL$(8) = STTL$(8) + CTL$(5, J): NEXT J
        STTL$(9) = "B1  i  B2 j  B3  (Multi-point constr. B1*Qi+B2*Qj=B3)"
     END IF
     RETURN
GETDAT:
'========================  READ DATA  =========================
     LINE INPUT #1, STTL$(4)
     FOR I = 1 TO NN: INPUT #1, N: FOR J = 1 TO NDIM
     INPUT #1, X(N, J): NEXT J: NEXT I
     LINE INPUT #1, STTL$(5)
     FOR I = 1 TO NE: INPUT #1, N: FOR J = 1 TO NEN
     INPUT #1, NOC(N, J): NEXT J: INPUT #1, MAT(N)
        IF IFL1 = 2 THEN
           FOR J = 1 TO NCH: INPUT #1, CHAR(N, J): NEXT J
        END IF
     NEXT I
     IF IFL1 = 2 THEN
        LINE INPUT #1, STTL$(6)
        FOR I = 1 TO ND: INPUT #1, NU(I), U(I): NEXT I
        LINE INPUT #1, STTL$(7)
        FOR I = 1 TO NL: INPUT #1, NF(I), F(I): NEXT I
        LINE INPUT #1, STTL$(8)
        FOR I = 1 TO NM: INPUT #1, N: FOR J = 1 TO NPR
           INPUT #1, PM(N, J): NEXT J: NEXT I
        IF NMPC > 0 THEN
           LINE INPUT #1, STTL$(9)
           FOR I = 1 TO NMPC
           INPUT #1, BT(I, 1), MPC(I, 1), BT(I, 2), MPC(I, 2), BT(I, 3)
           NEXT I
        END IF
     END IF
     CLOSE #1
     RETURN
SAVDAT:
'========================  SAVE DATA  =========================
     OPEN FILE2$ FOR OUTPUT AS #1
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
        PRINT #1, MAT(I);
           FOR J = 1 TO NCH: PRINT #1, CHAR(I, J); : NEXT J
        PRINT #1, : NEXT I
        PRINT #1, STTL$(6)
        FOR I = 1 TO ND: PRINT #1, NU(I); U(I): NEXT I
        PRINT #1, STTL$(7)
        FOR I = 1 TO NL: PRINT #1, NF(I); F(I): NEXT I
        PRINT #1, STTL$(8)
        FOR I = 1 TO NM: PRINT #1, I;
           FOR J = 1 TO NPR: PRINT #1, PM(I, J); : NEXT J
        PRINT #1, : NEXT I
        PRINT #1, STTL$(9)
        FOR I = 1 TO NMPC
           PRINT #1, BT(I, 1); MPC(I, 1); BT(I, 2); MPC(I, 2); BT(I, 3)
        NEXT I
        CLOSE #1
     RETURN

