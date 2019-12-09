      DIMENSION A(100, 100), B(100), X(100)
      PRINT *, '*********   PROGRAM CGSOL   **********'
      PRINT *, '*     CONJUGATE GRADIENT METHOD      *'
      PRINT *, '*   FOR SOLVING AX=B, A Symmetric    *'
      PRINT *, '* T.R.Chandrupatla and A.D.Belegundu *'
      PRINT *, '**************************************'
      PRINT *, 'NUMBER OF EQUATIONS'    
      DATA N /8/
      PRINT *, 'Matrix Elements'
      DATA ((A(I,J), J=1,8), I=1,8) /6,0,1,2,0,0,2,1,
     C				     0,5,1,1,0,0,3,0,
     C				     1,1,6,1,2,0,1,2,
     C				     2,1,1,7,1,2,1,1,
     C				     0,0,2,1,6,0,2,1,
     C				     0,0,0,2,0,4,1,0,
     C				     2,3,1,1,2,1,5,1,
     C				     1,0,2,1,1,0,1,3/
      PRINT *,'Right Hand Side'
      DATA (B(I),I=1, 8) /1,1,1,1,1,1,1,1/
      ITER = 0
      CALL CG(A, B, X, N, ITER)
      PRINT *, 'Iterations = ', ITER
      PRINT *, 'Solution Vector'
      WRITE (*,'(I4,E12.4)')(I, X(I),I = 1, N)
      END

      SUBROUTINE CG (A, B, X, N, ITER)
      DIMENSION A(100,100), B(100), X(100), G(100), D(100), AD(100)
      DO 100 I = 1, N
	X(I) = 0.
        G(I) = -B(I)
        D(I) = B(I)
  100 CONTINUE
      GG1 = 0.
      DO 110 I = 1, N
         GG1 = GG1 + G(I) * G(I)
  110 CONTINUE
  120 IF (GG1.LE.1E-6) GO TO 170
	ITER = ITER + 1
	DAD = 0.
        DO 140 I = 1, N
	   C = 0.
           DO 130 J = 1, N
              C = C + A(I, J) * D(J)
  130      CONTINUE
           AD(I) = C
           DAD = DAD + C * D(I)
  140   CONTINUE
        AL = GG1 / DAD
	GG2 = 0.
        DO 150 I = 1, N
           X(I) = X(I) + AL * D(I)
           G(I) = G(I) + AL * AD(I)
           GG2 = GG2 + G(I) * G(I)
  150   CONTINUE
        BT = GG2 / GG1
        DO 160 I = 1, N
           D(I) = -G(I) + BT * D(I)
  160	CONTINUE
        GG1 = GG2
      GO TO 120
  170 CONTINUE
      END
