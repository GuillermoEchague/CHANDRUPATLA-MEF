     '********  PROGRAM SKYLINE2  ***********
     '*    SKYLINE METHOD FOR EQUATIONS     *
     '* T.R.Chandrupatla and A.D.Belegundu  *
     '***************************************
     DEFINT I-N: CLS : COLOR 1, 3
     LOCATE 1, 1: PRINT "SKYLINE METHOD to solve Ax=B";
     PRINT SPACE$(23); "(C)  Chandrupatla & Belegundu": COLOR 7, 0
     VIEW PRINT 2 TO 25: PRINT
     READ N
     NSUM = 0
     DIM ID(N)
     '--- Read Column Heights then convert to Diagonal Pointers
     READ ID(1)
     FOR I = 2 TO N
        READ ID(I)
        ID(I) = ID(I) + ID(I - 1)
     NEXT I
     NSUM = ID(N)
     DIM A(NSUM), B(N)
     NT = 0: NI = 1
     FOR I = 1 TO N
        IF I > 1 THEN
           NT = ID(I - 1)
           NI = ID(I) - ID(I - 1)
        END IF
        FOR J = 1 TO NI
           READ A(NT + J)
        NEXT J
     NEXT I
     FOR I = 1 TO N
        READ B(I)
     NEXT I
     PRINT "Original Matrix and Right Hand Side"
     NI = 1: NT = 0
     FOR I = 1 TO N
        IF I > 1 THEN
           NI = ID(I) - ID(I - 1)
           NT = ID(I - 1)
        END IF
        FOR J = 1 TO NI
           PRINT USING "##.####  "; A(NT + J);
        NEXT J
        PRINT
     NEXT I
     FOR I = 1 TO N: PRINT USING "##.####  "; B(I); : NEXT I: PRINT
     FOR J = 2 TO N
        NJ = ID(J) - ID(J - 1)
        IF NJ > 1 THEN
           K1 = 0: NJ = J - NJ + 1
           FOR K = NJ TO J - 1
              K1 = K1 + 1: KJ = ID(J - 1) + K1: KK = ID(K)
              C = A(KJ) / A(KK)
              FOR I = K + 1 TO J
                 NI = ID(I) - ID(I - 1)
                    IF (I - K + 1) <= NI THEN
                    IJ = ID(J) - J + I
                    KI = ID(I) - I + K
                    A(IJ) = A(IJ) - C * A(KI)
                 END IF
              NEXT I
           NEXT K
        END IF
     NEXT J
     FOR K = 1 TO N - 1
        KK = ID(K)
        C = B(K) / A(KK)
        FOR I = K + 1 TO N
           NI = ID(I) - ID(I - 1)
           IF (I - K + 1) <= NI THEN
               KI = ID(I) - I + K
               B(I) = B(I) - C * A(KI)
           END IF
        NEXT I
     NEXT K
     PRINT "Reduced Matrix and Right Hand Side"
     NI = 1: NT = 0
     FOR I = 1 TO N
         IF I > 1 THEN
             NI = ID(I) - ID(I - 1)
             NT = ID(I - 1)
         END IF
         FOR J = 1 TO NI
         PRINT USING "##.####  "; A(NT + J); : NEXT J: PRINT : NEXT I
         FOR I = 1 TO N: PRINT USING "##.####  "; B(I); : NEXT I: PRINT
     '----- Back-substitution -----
     NS = ID(N): B(N) = B(N) / A(NS)
     FOR I1 = 1 TO N - 1
        I = N - I1
        II = ID(I)
        C = 1 / A(II): B(I) = C * B(I)
        FOR J = I + 1 TO N
           J1 = J - I + 1: NJ = ID(J) - ID(J - 1)
           IF J1 <= NJ THEN
              IJ = ID(J) - J + I
              B(I) = B(I) - C * A(IJ) * B(J)
           END IF
        NEXT J
     NEXT I1
     PRINT : PRINT "   SOLUTION"
     FOR I = 1 TO N: PRINT USING " #.####^^^^  "; B(I): NEXT I
     END
     '--- Number of Equations ---
     DATA 8
     '--- Column Heights from Top to Diagonal ---
     DATA 1,1,3,4,3,3,7,8
     '--- Column Entries from Top to Diagonal ---
     DATA 6
     DATA 5
     DATA 1,1,6
     DATA 2,1,1,7
     DATA 2,1,6
     DATA 2,0,4
     DATA 2,3,1,1,2,1,5
     DATA 1,0,2,1,1,0,1,3
     '--- Right Hand Side ---
     DATA 1,1,1,1,1,1,1,1

