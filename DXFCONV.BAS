     '*************  DXFCON.BAS   ***********
     '*          CREATES .DXF FILE          *
     '*        FOR GRAPHICS EXCHANGE        *
     '* T.R.Chandrupatla and A.D.Belegundu  *
     '***************************************
     DEFINT I-N: CLS : COLOR 1, 3
     LOCATE 1, 1: PRINT "DXF FILE FROM CONNECTIVITY DATA";
     PRINT SPACE$(20); "(C)  Chandrupatla & Belegundu": : COLOR 7, 0
     VIEW PRINT 2 TO 25: PRINT
     '========  Screen for Graphics  =========
     DEFINT I-N: F$ = "####.##"
     LOCATE 7, 1
     INPUT "Plot File Name <path fn.ext>  ", FILE$
     OPEN FILE$ FOR INPUT AS #1
     LINE INPUT #1, D$: INPUT #1, TITLE$
     LINE INPUT #1, D$: INPUT #1, NN, NE, NM, NDIM, NEN, NDN
     LINE INPUT #1, D$: INPUT #1, ND, NL, NCH, NPR, NMPC
     IF NDIM <> 2 THEN
        PRINT "THE PROGRAM SUPPORTS TWO DIMENSIONAL PLOTS ONLY"
        PRINT "THE DIMENSION OF THE DATA IS  "; NDIM
        END
     END IF
     DIM X(NN, NDIM), NOC(NE, NEN), NCO(NE, NEN)
     LINE INPUT #1, D$
     FOR I = 1 TO NN: INPUT #1, N: FOR J = 1 TO NDIM
     INPUT #1, X(N, J): NEXT J: NEXT I
     LINE INPUT #1, D$
     FOR I = 1 TO NE: INPUT #1, N: FOR J = 1 TO NEN
     INPUT #1, NOC(N, J): NEXT J: INPUT #1, NTMP
           FOR J = 1 TO NCH: INPUT #1, C: NEXT J
     NEXT I
        FOR I = 1 TO NE
        FOR J = 1 TO NEN
        J1 = J - 1
        IF J1 = 0 THEN J1 = NEN
        NCO(I, J1) = NOC(I, J)
        NEXT J
        NEXT I
        CLOSE #1
        NL = NE * NEN
        FOR I = 1 TO NE - 1
           FOR J = 1 TO NEN
              I1 = NOC(I, J)
              I2 = NCO(I, J)
              IF I2 < I1 THEN
                 NT = I1: I1 = I2: I2 = NT
              END IF
              FOR K = I + 1 TO NE
                 FOR JK = 1 TO NEN
                    J1 = NOC(K, JK)
                    J2 = NCO(K, JK)
                    IF J2 > 0 THEN
                       IF J2 < J1 THEN
                          NT = J1: J1 = J2: J2 = NT
                       END IF
                       IF I1 = J1 AND I2 = J2 THEN
                          NCO(K, JK) = 0
                          NL = NL - 1
                       END IF
                   END IF
                 NEXT JK
              NEXT K
           NEXT J
        NEXT I
        DIM NLIN(NL, 2)
        LI = 0
        FOR I = 1 TO NE
           FOR J = 1 TO NEN
              IF NCO(I, J) > 0 THEN
                 LI = LI + 1
                 NLIN(LI, 1) = NOC(I, J)
                 NLIN(LI, 2) = NCO(I, J)
              END IF
           NEXT J
        NEXT I
        XMIN = X(1, 1): YMIN = X(1, 2)
        XMAX = XMIN: YMAX = YMIN
        FOR I = 1 TO NN
           IF XMIN > X(I, 1) THEN XMIN = X(I, 1)
           IF XMAX < X(I, 1) THEN XMAX = X(I, 1)
           IF YMIN > X(I, 2) THEN YMIN = X(I, 2)
           IF YMAX < X(I, 2) THEN YMAX = X(I, 2)
        NEXT I
        INPUT "File name for DXF file (WITHOUT EXTENSION) "; FILE$
        OPEN FILE$ + ".DXF" FOR OUTPUT AS #1
        PRINT #1, "0"
        PRINT #1, "SECTION"
        PRINT #1, "2"
        PRINT #1, "HEADER"
        PRINT #1, "9"
        PRINT #1, "$EXTMIN"
        PRINT #1, "10"
        PRINT #1, XMIN
        PRINT #1, "20"
        PRINT #1, YMIN
        PRINT #1, "9"
        PRINT #1, "$EXTMAX"
        PRINT #1, "10"
        PRINT #1, XMAX
        PRINT #1, "20"
        PRINT #1, YMAX
        PRINT #1, "0"
        PRINT #1, "ENDSEC"
        PRINT #1, "0"
        PRINT #1, "SECTION"
        PRINT #1, "2"
        PRINT #1, "TABLES"
        PRINT #1, "0"
        PRINT #1, "TABLE"
        PRINT #1, "2"
        PRINT #1, "STYLE"
        PRINT #1, "70"
        PRINT #1, "1"
        PRINT #1, "0"
        PRINT #1, "STYLE"
        PRINT #1, "2"
        PRINT #1, "STANDARD"
        PRINT #1, "70"
        PRINT #1, "0"
        PRINT #1, "40"
        PRINT #1, "0"
        PRINT #1, "41"
        PRINT #1, "1"
        PRINT #1, "50"
        PRINT #1, "0"
        PRINT #1, "71"
        PRINT #1, "0"
        PRINT #1, "42"
        PRINT #1, "1"
        PRINT #1, "3"
        PRINT #1, "txt"
        PRINT #1, "0"
        PRINT #1, "ENDTAB"
        PRINT #1, "0"
        PRINT #1, "TABLE"
        PRINT #1, "2"
        PRINT #1, "LAYER"
        PRINT #1, "70"
        PRINT #1, "2"
        PRINT #1, "0"
        PRINT #1, "LAYER"
        PRINT #1, "2"
        PRINT #1, "1"
        PRINT #1, "70"
        PRINT #1, "0"
        PRINT #1, "62"
        PRINT #1, "7"
        PRINT #1, "6"
        PRINT #1, "CONTINUOUS"
        PRINT #1, "0"
        PRINT #1, "ENDTAB"
        PRINT #1, "0"
        PRINT #1, "ENDSEC"
        PRINT #1, "0"
        PRINT #1, "SECTION"
        PRINT #1, "2"
        PRINT #1, "ENTITIES"
        FOR I = 1 TO NL
           I1 = NLIN(I, 1)
           I2 = NLIN(I, 2)
           PRINT #1, "0"
           PRINT #1, "LINE"
           PRINT #1, "8"
           PRINT #1, "1"
           PRINT #1, "10"
           PRINT #1, X(I1, 1)
           PRINT #1, "20"
           PRINT #1, X(I1, 2)
           PRINT #1, "11"
           PRINT #1, X(I2, 1)
           PRINT #1, "21"
           PRINT #1, X(I2, 2)
        NEXT I
        PRINT #1, "0"
        PRINT #1, "ENDSEC"
        PRINT #1, "0"
        PRINT #1, "EOF"
        CLOSE #1
        PRINT "Data has been stored in the file "; FILE$ + ".DXF"
        END

