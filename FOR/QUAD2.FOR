C   *****                   QUAD2                 *****
C   ***      2-D STRESS ANALYSIS USING 4-NODE       ***
C   ***            QUADRILATERAL ELEMENTS           ***
      DIMENSION X(100,2),NOC(100,4),MAT(100),PM(10,3)
      DIMENSION NU(50),U(50),S(200,50),F(200),D(3,3),TH(100)
      DIMENSION B(3,8),DB(3,8),SE(8,8),Q(8),STR(3),TL(8)
      DIMENSION DT(100),XNI(4,2),MPC(20,2),BT(20,3)
      CHARACTER*16 FILE1,FILE2,FILE3
      CHARACTER*81 DUMMY,TITLE
      IMAX = 200
      PRINT *, '********       PROGRAM QUAD2       **********'
      PRINT *, '*     2-D STRESS ANALYSIS USING 4-NODE      *'
      PRINT *, '*  QUADRILATERAL ELEMENTS WITH TEMPERATURE  *'
      PRINT *, '*    T.R.Chandrupatla and A.D.Belegundu     *'
      PRINT *, '*********************************************'
      PRINT *, 'PROBLEM TYPE'
      PRINT *, '  1) Plane Stress'
      PRINT *, '  2) Plane Strain'
      PRINT *, '     Choose 1 or 2'
      READ *, LC
C ------- Default is Plane Stress      
      IF ((LC .LT. 1) .OR. (LC .GT. 2)) LC = 1
      PRINT *, 'File Name for Input Data '
      READ '(A)', FILE1
      LINP=10
      OPEN (UNIT = 10, FILE = FILE1, STATUS = 'OLD')
      PRINT *, 'Give Name of Output File '
      READ '(A)', FILE2
      READ(LINP,'(A)') DUMMY
      READ(LINP,'(A)') TITLE
      READ(LINP,'(A)') DUMMY
      READ(LINP,*) NN, NE, NM, NDIM, NEN, NDN
      READ(LINP,'(A)') DUMMY      
      READ(LINP,*) ND, NL, NCH, NPR, NMPC
      IF (NPR .LT. 3) NPR = 3 
C    ----- Total dof is  NQ
      NQ = NDN * NN
      PRINT *, '      PLOT CHOICE'
      PRINT *, '  1) No Plot Data'
      PRINT *, '  2) Create Data File for in-plane Shear Stress'
      PRINT *, '  3) Create Data File for Von Mises Stress'
      PRINT *, '     Choose 1 or 2 or 3'
      READ *, IPL      
      IF ((IPL .LT. 1) .OR. (IPL .GT. 3)) IPL = 1
      IF (IPL .GT. 1) THEN 
         PRINT *, 'File Name for Plot Data'
	 READ '(A)', FILE3
      END IF
C   ============  READ DATA FROM FILE  ===========
C     ----- Coordinates -----
      READ(LINP,'(A)') DUMMY
      DO 900 I = 1, NN
  900 READ (LINP, *) N, X(N,1), X(N,2)
C     ----- Connectivity, Material, Thickness, Temp-change -----
      READ(LINP,'(A)') DUMMY
      DO 1000 I=1,NE
      READ (LINP, *) N,NOC(N,1),NOC(N,2),NOC(N,3),NOC(N,4),MAT(N),TH(N),
     1DT(N)
 1000 CONTINUE
C    ----- Specified Displacements -----
      READ(LINP,'(A)') DUMMY
      DO 1010  I = 1, ND
 1010 READ (LINP, *) NU(I), U(I)
C     ----- Component Loads -----
      READ(LINP,'(A)') DUMMY      
C     ----- First initialize and then read loads -----
      DO 1015 I=1, NQ
 1015 F(I) = 0
      DO 1020  I = 1, NL
 1020 READ (LINP, *) N, F(N)
C     ----- Material Properties -----
      READ(LINP,'(A)') DUMMY      
      DO 1030  I = 1, NM
 1030	READ (LINP, *) N, (PM(N, J), J = 1, NPR)
C     ----- Multi-point Constraints B1*Qi+B2*Qj=B0
      IF (NMPC .GT. 0) THEN
	READ(LINP,'(A)') DUMMY
        DO 1040  I = 1, NMPC
 1040	READ (LINP,*) BT(I,1),MPC(I,1),BT(I,2),MPC(I,2),BT(I,3)
      END IF
      CLOSE(LINP)
C     ----- Bandwidth Evaluation -----
      NBW = 0
      DO 1050 I = 1, NE
        NMIN = NOC(I, 1)
        NMAX = NOC(I, 1)
        DO 1045 J = 2, NEN
           IF (NMIN .GT. NOC(I, J)) NMIN = NOC(I, J)
           IF (NMAX .LT. NOC(I, J)) NMAX = NOC(I, J)
 1045   CONTINUE
        NTMP = NDN * (NMAX - NMIN + 1)
        IF (NBW .LT. NTMP) NBW = NTMP
 1050 CONTINUE
      DO 1060  I = 1, NMPC
        NABS = ABS(MPC(I, 1) - MPC(I, 2)) + 1
        IF (NBW .LT. NABS) NBW = NABS
 1060 CONTINUE
      PRINT *, 'Bandwidth = ', NBW
C    ----- First initialize stiffness and Mass matrices
      DO 1070 I = 1, NQ
      DO 1070 J = 1, NBW
 1070 S(I,J) = 0
C  ----- Global Stiffness Matrix -----
C  ----- Corner Nodes and Integration Points
      CALL INTEG(XNI)
      DO 1090 N = 1, NE
	  PRINT *, 'Forming Stiffness Matrix of Element ', N
	  CALL DMATX(N,PM,MAT,PNU,AL,LC,D)
	  CALL ELSTIF(N,LC,SE,TL,XNI,D,TH,DT,X,NOC,AL,PNU)
	  PRINT *, '.... Placing in Global Locations'
         DO 1090  II = 1, NEN
           NRT = NDN * (NOC(N, II) - 1)
	     DO 1090  IT = 1, NDN
              NR = NRT + IT
              I = NDN * (II - 1) + IT
              DO 1080  JJ = 1, NEN
                 NCT = NDN * (NOC(N, JJ) - 1)
                 DO 1080  JT = 1, NDN
                    J = NDN * (JJ - 1) + JT
                    NC = NCT + JT - NR + 1
                    IF (NC .GT. 0) THEN
                       S(NR, NC) = S(NR, NC) + SE(I, J)
                    END IF
 1080           CONTINUE
              F(NR) = F(NR) + TL(I)
 1090	     CONTINUE
C     ----- Decide Penalty Parameter CNST -----
      CNST = 0
      DO 1100  I = 1, NQ
        IF (CNST .LT. S(I, 1)) CNST = S(I, 1)
 1100 CONTINUE 
      CNST = CNST * 10000
C    ----- Modify for Boundary Conditions -----
C    --- Displacement BC ---
      DO 1110  I = 1, ND
        N = NU(I)
        S(N, 1) = S(N, 1) + CNST
        F(N) = F(N) + CNST * U(I)
 1110 CONTINUE
C    --- Multi-point Constraints ---
      DO 1120  I = 1, NMPC
        I1 = MPC(I, 1)
        I2 = MPC(I, 2)
        S(I1, 1) = S(I1, 1) + CNST * BT(I, 1) * BT(I, 1)
        S(I2, 1) = S(I2, 1) + CNST * BT(I, 2) * BT(I, 2)
        IR = I1
        IF (IR .GT.I2) IR = I2
        IC = ABS(I2 - I1) + 1
        S(IR, IC) = S(IR, IC) + CNST * BT(I, 1) * BT(I, 2)
        F(I1) = F(I1) + CNST * BT(I, 1) * BT(I, 3)
        F(I2) = F(I2) + CNST * BT(I, 2) * BT(I, 3)
 1120 CONTINUE
C   ----- Equation Solving
      PRINT *, '.... Solving Equations'
      CALL BAND(S, F, IMAX, NBW, NQ)
      LOUT = 11
      OPEN (UNIT = 11, FILE = FILE2)
      WRITE(LOUT,'(2A)') 'Output for Input Data from File ', FILE1
      PRINT *, 'Output for Input Data from File ', FILE1
      WRITE(LOUT,'(A)')  TITLE
      PRINT *, TITLE     
      IF (LC .EQ. 1) WRITE(LOUT,'(A)') 'Plane Stress Analysis'
      IF (LC .EQ. 2) WRITE(LOUT,'(A)') 'Plane Strain Analysis'
      PRINT *, 'Node#      X-Displ        Y-Displ'
      WRITE(LOUT,'(A)') 'Node#      X-Displ        Y-Displ'
      WRITE(LOUT,'(1X,I4,2E15.4)') (I,F(2*I-1),F(2*I),I=1,NN)
      WRITE(*,'(1X,I4,2E15.4)') (I,F(2*I-1),F(2*I),I=1,NN)
C     ----- Reaction Calculation -----
      PRINT *, 'DOF#      Reaction'
      WRITE(LOUT,'(A)')  'DOF#      Reaction'
      DO 1140  I = 1, ND
        N = NU(I)
        R = CNST * (U(I) - F(N))
        WRITE(*,'(1X,I4,E15.4)') N, R
        WRITE(LOUT,'(1X,I4,E15.4)') N, R
 1140 CONTINUE
      IF (IPL .GT. 1) THEN
        IOUT = 12
        OPEN (UNIT = 12, FILE = FILE3)      
        IF (IPL .EQ. 2) WRITE(IOUT,'(A)') 'Max. in-plane Shear Stress '
        IF (IPL .EQ. 3) WRITE(IOUT,'(A)') 'Von Mises Stress '
        WRITE(IOUT,*) '(Element) for Data in File ', FILE1
       END IF     
C  -----  Stress Calculations -----
C  --- Stresses at Integration Points
      WRITE(LOUT,'(A)') 'ELEM#	 von Mises Stresses at 4 Integ_points'
      DO 1190 N = 1, NE
	WRITE(LOUT,'(I4)') N
	DO 1180 IP = 1, 4
	   XI = XNI(IP, 1)
	   ETA = XNI(IP, 2)
	   CALL DMATX(N,PM,MAT,PNU,AL,LC,D)
	   CALL DBMAT(N,X,NOC,TH,THICK,D,B,DB,DJ,XI,ETA)
C  --- Stress Evaluation
           DO 1150 I = 1, NEN
              IN = NDN * (NOC(N, I) - 1)
              II = NDN * (I - 1)
              DO 1150 J = 1, NDN
 1150         Q(II + J) = F(IN + J)
           C1 = AL * DT(N)
           IF (LC .EQ. 2) C1 = C1 * (1 + PNU)
	   DO 1170 I = 1, 3
              C = 0
              DO 1160 K = 1, 8
 1160         C = C + DB(I, K) * Q(K)
 1170       STR(I) = C - C1 * (D(I, 1) + D(I, 2))
C  --- Von Mises Stress at Integration Point
           C = 0
           IF (LC .EQ. 2) C = PNU * (STR(1) + STR(2))
           C1 = (STR(1)-STR(2))**2 + (STR(2)-C)**2 + (C-STR(1))**2
	   SV = SQRT(.5 * C1 + 3 * STR(3)**2)
	   WRITE (LOUT,'(1X,E15.4)') SV
C  --- Maximum Shear Stress R
           R = SQRT(.25 * (STR(1) - STR(2))**2 + (STR(3))**2)
           IF (IPL .EQ. 2) WRITE(IOUT,*) R
           IF (IPL .EQ. 3) WRITE(IOUT,*) SV
 1180   CONTINUE
        WRITE(LOUT,*)
        IF (IPL .GT. 1) WRITE(LOUT,*)
 1190	CONTINUE
        CLOSE(LOUT)
      PRINT *, '-----    All Calculations are done    -----'
      PRINT *, 'The Results are available in the text file ', FILE2
      PRINT *, 'View using a text processor'
      IF (IPL .GT. 1) THEN
        CLOSE(IOUT)
	PRINT *, 'Element Stress Data in file ', FILE3
        PRINT *, 'Run BESTFIT2 and then CONTOUR1 or CONTOUR2 to plot str
     1esses'
      END IF
      END

      SUBROUTINE INTEG(XNI)
      DIMENSION XNI(4,2)
C  ------- Integration Points XNI() --------
      C = .57735026919
      XNI(1, 1) = -C
      XNI(1, 2) = -C
      XNI(2, 1) = C
      XNI(2, 2) = -C
      XNI(3, 1) = C
      XNI(3, 2) = C
      XNI(4, 1) = -C
      XNI(4, 2) = C
      RETURN
      END
      
      SUBROUTINE DMATX(N,PM,MAT,PNU,AL,LC,D)
      DIMENSION MAT(100),PM(10,3),D(3,3)
C -----  D() Matrix and Element Nodal Coordinates -----
C   --- Material Properties
      MATN = MAT(N)
      E = PM(MATN, 1)
      PNU = PM(MATN, 2)
      AL = PM(MATN, 3)
C   --- D() Matrix
      IF (LC .EQ. 1) THEN
C   --- Plane Stress
        C1 = E / (1 - PNU**2)
        C2 = C1 * PNU
      ELSE
C   --- Plane Strain
        C = E / ((1 + PNU) * (1 - 2 * PNU))
        C1 = C * (1 - PNU)
        C2 = C * PNU
      END IF
      C3 = .5 * E / (1 + PNU)
      D(1, 1) = C1
      D(1, 2) = C2
      D(1, 3) = 0
      D(2, 1) = C2
      D(2, 2) = C1
      D(2, 3) = 0
      D(3, 1) = 0
      D(3, 2) = 0
      D(3, 3) = C3
      RETURN
      END
      
      SUBROUTINE ELSTIF(N,LC,SE,TL,XNI,D,TH,DT,X,NOC,AL,PNU)
      DIMENSION X(100,2),NOC(100,4),D(3,3),TH(100),B(3,8)
      DIMENSION DB(3,8),SE(8,8),DT(100),TL(8),XNI(4,2)
C  --------  Element Stiffness and Temperature Load  -----
      DO 3000 I = 1, 8
        TL(I) = 0
      DO 3000 J = 1, 8
 3000 SE(I, J) = 0
      DTE = DT(N)
C  --- Weight Factor is ONE
C  --- Loop on Integration Points
      DO 3040 IP = 1, 4
C  ---  Get DB Matrix at Integration Point IP
        XI = XNI(IP, 1)
        ETA = XNI(IP, 2)
        CALL DBMAT(N,X,NOC,TH,THICK,D,B,DB,DJ,XI,ETA)
C  --- Element Stiffness Matrix  SE
        DO 3020 I = 1, 8
           DO 3020 J = 1, 8
              C = 0
              DO 3010 K = 1, 3
 3010         C = C + B(K, I) * DB(K, J) * DJ * THICK
              SE(I, J) = SE(I, J) + C
 3020      CONTINUE
C  --- Determine Temperature Load TL
        C = AL * DTE
        IF (LC .EQ. 2) C = (1 + PNU) * C
        DO 3030 I = 1, 8
 3030   TL(I) = TL(I) + THICK * DJ * C * (DB(1, I) + DB(2, I))
 3040 CONTINUE
      RETURN
      END
      
      SUBROUTINE DBMAT(N,X,NOC,TH,THICK,D,B,DB,DJ,XI,ETA)
      DIMENSION X(100,2),NOC(100,4),D(3,3),TH(100),B(3,8)
      DIMENSION DB(3,8),A(3,4),G(4,8)        
C  -------  DB()  MATRIX  ------
C  --- Nodal Coordinates
      THICK = TH(N)
      N1 = NOC(N, 1)
      N2 = NOC(N, 2)
      N3 = NOC(N, 3)
      N4 = NOC(N, 4)
      X1 = X(N1, 1)
      Y1 = X(N1, 2)
      X2 = X(N2, 1)
      Y2 = X(N2, 2)
      X3 = X(N3, 1)
      Y3 = X(N3, 2)
      X4 = X(N4, 1)
      Y4 = X(N4, 2)
C  --- Formation of Jacobian  TJ
      TJ11 = ((1 - ETA) * (X2 - X1) + (1 + ETA) * (X3 - X4)) / 4
      TJ12 = ((1 - ETA) * (Y2 - Y1) + (1 + ETA) * (Y3 - Y4)) / 4
      TJ21 = ((1 - XI) * (X4 - X1) + (1 + XI) * (X3 - X2)) / 4
      TJ22 = ((1 - XI) * (Y4 - Y1) + (1 + XI) * (Y3 - Y2)) / 4
C  --- Determinant of the JACOBIAN
      DJ = TJ11 * TJ22 - TJ12 * TJ21
C  --- A(3,4) Matrix relates Strains to
C  --- Local Derivatives of u
      A(1, 1) = TJ22 / DJ
      A(2, 1) = 0
      A(3, 1) = -TJ21 / DJ
      A(1, 2) = -TJ12 / DJ
      A(2, 2) = 0
      A(3, 2) = TJ11 / DJ
      A(1, 3) = 0
      A(2, 3) = -TJ21 / DJ
      A(3, 3) = TJ22 / DJ
      A(1, 4) = 0
      A(2, 4) = TJ11 / DJ
      A(3, 4) = -TJ12 / DJ
C  --- G(4,8) Matrix relates Local Derivatives of u
C  --- to Local Nodal Displacements q(8)
      DO 3500 I = 1, 4
      DO 3500 J = 1, 8
 3500 G(I, J) = 0
      G(1, 1) = -(1 - ETA) / 4
      G(2, 1) = -(1 - XI) / 4
      G(3, 2) = -(1 - ETA) / 4
      G(4, 2) = -(1 - XI) / 4
      G(1, 3) = (1 - ETA) / 4
      G(2, 3) = -(1 + XI) / 4
      G(3, 4) = (1 - ETA) / 4
      G(4, 4) = -(1 + XI) / 4
      G(1, 5) = (1 + ETA) / 4
      G(2, 5) = (1 + XI) / 4
      G(3, 6) = (1 + ETA) / 4
      G(4, 6) = (1 + XI) / 4
      G(1, 7) = -(1 + ETA) / 4
      G(2, 7) = (1 - XI) / 4
      G(3, 8) = -(1 + ETA) / 4
      G(4, 8) = (1 - XI) / 4
C  --- B(3,8) Matrix Relates Strains to q
      DO 3520 I = 1, 3
        DO 3520 J = 1, 8
           C = 0
           DO 3510 K = 1, 4
 3510      C = C + A(I, K) * G(K, J)
           B(I, J) = C
 3520 CONTINUE
C  --- DB(3,8) Matrix relates Stresses to q(8)
      DO 3540 I = 1, 3
        DO 3540 J = 1, 8
           C = 0
	   DO 3530 K = 1, 3
 3530      C = C + D(I, K) * B(K, J)
           DB(I, J) = C
 3540 CONTINUE
      RETURN
      END
      
      SUBROUTINE BAND(A, B, IMAX, NBW, N)
C ----- Equation Solving Using Banded Storage
      DIMENSION A(IMAX,NBW), B(IMAX)
      N1 = N - 1
      PRINT *, '*** FORWARD ELIMINATION ***'
      DO 2100 K = 1, N1
      NK = N - K + 1
      IF (NK .GT. NBW) NK = NBW
      DO 2100 I = 2, NK
      C1 = A(K, I) / A(K, 1)
      I1 = K + I - 1
      DO 2000 J = I, NK
      J1 = J - I + 1
 2000 A(I1, J1) = A(I1, J1) - C1 * A(K, J)
 2100 B(I1) = B(I1) - C1 * B(K)
      PRINT *, '*** BACK SUBSTITUTION ***'
      B(N) = B(N) / A(N, 1)
      DO 2300 KK = 1, N1
      K = N - KK
      C1 = 1 / A(K, 1)
      B(K) = C1 * B(K)
      NK = N - K + 1
      IF (NK .GT. NBW) NK = NBW
      DO 2200 J = 2, NK
      B(K) = B(K) - C1 * A(K, J) * B(K + J - 1)
 2200 CONTINUE
 2300 CONTINUE
      RETURN
      END
