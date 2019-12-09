C   ****  Sky Line Method  ****
C   **** Gauss Elimination ****
      DIMENSION ID(200), A(5000), B(200)
      NMAX = 5000
      PRINT *,' ======================================='
      PRINT *,'            PROGRAM SKYLINE             '
      PRINT *,'   T.R.Chandrupatla and A.D.Belegundu   '
      PRINT *,' ======================================='
      PRINT '(A,$)',' Number of Equations '
      READ *, N
      NSUM = 0      
C   *** Read Column Heights then convert, Diagonal Pointers ***
      ID(1) = 1
      DO 100 I = 2, N
      PRINT '(A,I3,A,$)', ' Height of column  ',I,' = '
      READ *, ID(I)
  100 ID(I) = ID(I) + ID(I - 1)
      NSUM = ID(N)      
      NT = 0
      NI = 1
      DO 110 I = 1, N
      PRINT *,' Top non-zero element to the diagonal element'
      PRINT '(A,I3,A,$)',' Column(', I,')'
      IF( I .EQ. 1 ) GO TO 105
      NT = ID(I - 1)
      NI = ID(I) - ID(I - 1)
  105 IF(I.EQ.1) NI = 1
      PRINT '(A,I3,A)', ' give',NI,' element/s'
  110 READ *, (A(NT + J), J = 1, NI)
      PRINT *, ' Right Hand Side Vector'
      DO 120 I = 1, N
      PRINT '(1X,A,I3,A,$)', 'Row(', I,') => '
  120 READ *, B(I)
      PRINT *, 'Original Matrix and Right Hand Side'
      NI = 1
      NT = 0
      DO 140 I = 1, N
      IF( I .EQ. 1 ) GO TO 140
      NI = ID(I) - ID(I - 1)
      NT = ID(I - 1)
  140 PRINT '(1X,5E12.4)', (A(NT+J),J=1,NI)
      PRINT '(1X,5E12.4)', (B(I),I=1,N)
      DO 180 J = 2, N
      NJ = ID(J) - ID(J - 1)
      IF(NJ .EQ. 1) GO TO 180
      K1 = 0
      NJ = J - NJ + 1
      DO 170 K = NJ, J - 1
      K1 = K1 + 1
      KJ = ID(J - 1) + K1
      KK = ID(K)
      C = A(KJ) / A(KK)
      DO 160 I = K + 1, J
      NI = ID(I) - ID(I - 1)
      IF((I - K + 1) .GT. NI ) GO TO 160
      IJ = ID(J) - J + I
      KI = ID(I) - I + K
      A(IJ) = A(IJ) - C * A(KI)
  160 CONTINUE
  170 CONTINUE
  180 CONTINUE
      N1 = N - 1
      DO 190 K = 1, N1
      KK = ID(K)
      K1 = K + 1
      C = B(K) / A(KK)
      DO 190 I = K1, N
      NI = ID(I) - ID(I - 1)
      IF (I - K + 1.GT. NI) GO TO 190
      KI = ID(I) - I + K
      B(I) = B(I) - C * A(KI)
  190 CONTINUE
      PRINT *, ' Reduced Matrix and Right Hand Side'
      NI = 1
      NT = 0
      DO 200 I = 1, N
      IF(I .EQ. 1) GO TO 200
      NI = ID(I) - ID(I - 1)
      NT = ID(I - 1)
  200 PRINT'(1X,5E12.4)', (A(NT + J),J=1,NI)
      PRINT'(1X,5E12.4)', (B(I),I=1,N)
C    **** Back -substitution ****
      NS = ID(N)
      B(N) = B(N) / A(NS)
      DO 220 I1 = 1, N1
      I = N - I1
      II = ID(I)
      C = 1 / A(II)
      B(I) = C * B(I)
      II1=I + 1
      DO 210 J = II1, N
      J1 = J - I + 1
      NJ = ID(J) - ID(J - 1)
      IF( J1 .GT. NJ ) GO TO 210
      IJ = ID(J) - J + I
      B(I) = B(I) - C * A(IJ) * B(J)
  210 CONTINUE
  220 CONTINUE
      PRINT *,' SOLUTION'
      PRINT'(1X,E12.4)', (B(I),I=1,N)
      END
