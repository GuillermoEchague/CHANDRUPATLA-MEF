     '********  PROGRAM GAUSS_R2  **********
     '*      GAUSS ELIMINATION METHOD      *
     '*          GENERAL MATRIX            *
     '* T.R.Chandrupatla and A.D.Belegundu *
     '**************************************
     DEFINT I-N: CLS : COLOR 1, 3
     LOCATE 1, 1: PRINT "GAUSS ELIMINATION METHOD to solve Ax=B";
     PRINT SPACE$(13); "(C)  Chandrupatla & Belegundu": : COLOR 7, 0
     VIEW PRINT 2 TO 25: PRINT
     READ N
     DIM A(N, N), B(N)
     FOR I = 1 TO N
        FOR J = 1 TO N
           READ A(I, J)
        NEXT J
     NEXT I
     FOR I = 1 TO N
        READ B(I)
     NEXT I
     '----- Forward Elimination -----
     FOR K = 1 TO N - 1
        FOR I = K + 1 TO N
           C = A(I, K) / A(K, K)
           FOR J = K + 1 TO N
              A(I, J) = A(I, J) - C * A(K, J)
           NEXT J
           B(I) = B(I) - C * B(K)
        NEXT I
     NEXT K
     '----- Back-substitution -----
     B(N) = B(N) / A(N, N)
     FOR II = 1 TO N - 1
        I = N - II
        C = 1 / A(I, I): B(I) = C * B(I)
        FOR K = I + 1 TO N
           B(I) = B(I) - C * A(I, K) * B(K)
        NEXT K
     NEXT II
     PRINT "X() ="
     FOR I = 1 TO N: PRINT USING "##.#### "; B(I); : NEXT I: PRINT
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

