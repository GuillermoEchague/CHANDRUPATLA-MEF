C   *****  Gauss Elimination Method (General Matrix) *****
C   *****           Row-wise Elimination             *****
      DIMENSION A(100, 100), B(100)
      PRINT *, '======================================='
      PRINT *, '          PROGRAM GAUSS_R              '
      PRINT *, '  T.R.Chandrupatla and A.D.Belegundu   '
      PRINT *, '======================================='
      PRINT *, 'NUMBER OF EQUATIONS'    
      READ *, N
      PRINT *, 'Matrix Elements'
      DO 100 I = 1, N
      DO 100 J = 1, N
      PRINT *,'Row ',I,'  Column ',J
  100 READ *, A(I, J)
      PRINT *,'Right Hand Side'
      DO 110 I = 1, N
      PRINT *, 'Row ',I
  110 READ *, B(I)
      N1 = N - 1
      DO 130 K = 1, N1
      K1 = K + 1
      DO 130 I = K1, N
      C = A(I, K) / A(K, K)
      DO 120 J = K1, N
  120 A(I, J) = A(I, J) - C * A(K, J)
  130 B(I) = B(I) - C * B(K)
C   *** Back Substitution ***
      B(N) = B(N) / A(N, N)
      DO 140 II = 1, N1
      I = N - II
      I1 = I + 1
      C = 1 / A(I, I)
      B(I) = C * B(I)
      DO 140 K = I1, N
  140 B(I) = B(I) - C * A(I, K) * B(K)
      PRINT *, 'Solution Vector'
      WRITE (*,'(I4,E12.4)')(I, B(I),I = 1, N)
      END
  
