DECLARE SUB CG (A!(), B!(), X!(), N%, ITER%)
        '"*********   PROGRAM CGSOL   *********
        '*     CONJUGATE GRADIENT METHOD      *
        '*   FOR SOLVING AX=B, A Symmetric    *
        '* T.R.Chandrupatla and A.D.Belegundu *
        '**************************************
        DEFINT I-N: CLS : COLOR 1, 3
        LOCATE 1, 1: PRINT "CONJUGATE GRADIENT METHOD to solve Ax=B";
        PRINT SPACE$(12); "(C)  Chandrupatla & Belegundu": : COLOR 7, 0
        VIEW PRINT 2 TO 25: PRINT
        READ N
        DIM A(N, N), B(N), X(N)
        FOR I = 1 TO N
           FOR J = 1 TO N
              READ A(I, J)
           NEXT J
        NEXT I
        FOR I = 1 TO N
           READ B(I)
        NEXT I
        ITER = 0
        CALL CG(A(), B(), X(), N, ITER)
        PRINT "Iterations = "; ITER
        PRINT "X() = "
        FOR I = 1 TO N: PRINT USING "##.#### "; X(I); : NEXT I: PRINT
        END
     '--- Number of Equations ---
        DATA 8
     '--- Matrix A() in  Ax = B ---
        DATA 6,0,1,2,0,0,2,1
        DATA 0,5,1,1,0,0,3,0
        DATA 1,1,6,1,2,0,1,2
        DATA 2,1,1,7,1,2,1,1
        DATA 0,0,2,1,6,0,2,1
        DATA 0,0,0,2,0,4,1,0
        DATA 2,3,1,1,2,1,5,1
        DATA 1,0,2,1,1,0,1,3
     '--- Right hand side B() in Ax = B ---
        DATA 1,1,1,1,1,1,1,1

        SUB CG (A(), B(), X(), N, ITER)
        DIM G(N), D(N), AD(N)
        FOR I = 1 TO N
           X(I) = 0
           G(I) = -B(I)
           D(I) = B(I)
        NEXT I
        GG1 = 0
        FOR I = 1 TO N
           GG1 = GG1 + G(I) * G(I)
        NEXT I
DO WHILE GG1 > .000001
        ITER = ITER + 1
        DAD = 0
        FOR I = 1 TO N
           C = 0
           FOR J = 1 TO N
              C = C + A(I, J) * D(J)
           NEXT J
           AD(I) = C
           DAD = DAD + C * D(I)
        NEXT I
        AL = GG1 / DAD
        GG2 = 0
        FOR I = 1 TO N
           X(I) = X(I) + AL * D(I)
           G(I) = G(I) + AL * AD(I)
           GG2 = GG2 + G(I) * G(I)
        NEXT I
        BT = GG2 / GG1
        FOR I = 1 TO N
           D(I) = -G(I) + BT * D(I)
        NEXT I
        GG1 = GG2
    LOOP
        ERASE G, D, AD
END SUB

