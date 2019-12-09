     '**************  HGLCONV  *************
     '*          CREATES .HGL FILE         *
     '*        FOR GRAPHICS EXCHANGE       *
     '* T.R.Chandrupatla and A.D.Belegundu *
     '**************************************
     DEFINT I-N: CLS : COLOR 1, 3
     LOCATE 1, 1: PRINT "HGL FILE FROM CONNECTIVITY DATA";
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
        FOR I = 1 TO NN
           X(I, 1) = INT((X(I, 1) - XMIN) * 9999 / (XMAX - XMIN))
           X(I, 2) = INT((X(I, 2) - YMIN) * 9999 / (YMAX - YMIN))
        NEXT I
        INPUT "File name for HPG file -- Without Extension "; FILE$
        OPEN FILE$ + ".HGL" FOR OUTPUT AS #1
        PRINT #1, CHR$(27); ".(;IN;SC;PU;VS35;PU;SP1;";
        FOR I = 1 TO NL
           I1 = NLIN(I, 1)
           I2 = NLIN(I, 2)
           A$ = STR$(X(I1, 1))
           L = LEN(A$)
           A1$ = RIGHT$(A$, L - 1)
           A$ = STR$(X(I1, 2))
           L = LEN(A$)
           B1$ = RIGHT$(A$, L - 1)
           A$ = STR$(X(I2, 1))
           L = LEN(A$)
           A2$ = RIGHT$(A$, L - 1)
           A$ = STR$(X(I2, 2))
           L = LEN(A$)
           B2$ = RIGHT$(A$, L - 1)
           PRINT #1, "PA" + A1$ + "," + B1$ + ";PD;PA" + A2$ + "," + B2$ + ";PU;";
        NEXT I
        PRINT #1, "PA0,0;SP;"
        CLOSE #1
        PRINT "Data has been stored in the file "; FILE$ + ".HGL"
        END

