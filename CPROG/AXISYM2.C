/***************************************/
/*         program  axisym2            */
/*    axisymmetric stress analysis     */
/* t.r.chandrupatla and a.d.belegundu  */
/***************************************/
#include <stdio.h>
#include <math.h>
#define PI 3.141593
main()
{
   FILE *fptr1, *fptr2;
   int n,i,j,k,m,i1,i2,i3,ii,jj,m1,nmin,nmax,nrt,nct,it,jt;
   int nr,nc;
   char dummy[81], title[81], file1[81], file2[81];
   int ne,nn,nq,nm,nd,nl,nen,ndn,ndim,npr,nbw,nch,nmpc;
   int *noc, *nu, *mat, *mpc;
   float *x, *pm, *u, *tempr, *s, *f, *beta;
   float c,dj,al,pnu,tld,cnst,reaction,s1,s2,s3,ang,r,rbar;
   float b[4][6], d[4][4], db[4][6], se[6][6], str[4], tl[6];
/*-------------------------------------------------------*/
   printf("\n");
   puts("Input file name < dr:fn.ext >: ");
   gets(file1);
   puts("Output file name < dr:fn.ext >: ");
   gets(file2);
   fptr1 = fopen(file1, "r");
   fgets(dummy,80,fptr1);
   fgets(title,80,fptr1);
   fgets(dummy,80,fptr1);
   fscanf(fptr1,"%d %d %d %d %d %d\n", &nn, &ne, &nm, &ndim, &nen, &ndn);
   fgets(dummy, 80, fptr1);
   fscanf(fptr1,"%d %d %d %d %d\n", &nd, &nl, &nch, &npr, &nmpc);
   if (npr < 3)
      npr = 3;       /*--- dimensioned for minimum three properties ---*/
/* ----- memory allocation ----- */
   x = (float *) calloc(nn*ndim, sizeof(float));
   noc = (int *) calloc(ne*nen, sizeof(int));
   u = (float *) calloc(nd, sizeof(float));
   nu = (int *) calloc(nd, sizeof(int));
   mat = (int *) calloc(ne,sizeof(int));
   f = (float *) calloc(nn*ndn, sizeof(float));
   tempr = (float *) calloc(ne, sizeof(float));
   pm = (float *) calloc(nm*npr, sizeof(float));
   mpc = (int *) calloc(2*nmpc, sizeof(int));
   beta = (float *) calloc(3*nmpc, sizeof(float));
/* ----- total dof is  nq ----- */
     nq = ndn * nn;
/* ===============  read data  ==================== */
/* ----- coordinates ----- */
     fgets(dummy,80,fptr1);
     for (i = 0; i < nn; i++){
        fscanf(fptr1, "%d", &n);
        for (j = 0; j < ndim; j++){
		fscanf(fptr1, "%f\n", &c);
                x[ndim*(n-1)+j] = c;
            }
         }
/* ----- connectivity, material, temp-change ----- */
   fgets(dummy,80,fptr1);
   for (i = 0; i < ne; i++) {
       fscanf(fptr1,"%d", &n);
       for (j = 0; j < nen; j++) {
           fscanf(fptr1,"%d", &k);
           noc[(n-1)*nen+j]=k;
       }
       fscanf(fptr1,"%d", &k);
       mat[n-1] = k;
       fscanf(fptr1,"%f\n",&c);
       tempr[n-1] = c;
   }
/* ----- displacement bc  ----- */
   fgets(dummy,80,fptr1);
   for (i = 0; i < nd; i++) {
      fscanf(fptr1, "%d %f\n", &k, &c);
      nu[i] = k;
      u[i] = c;
   }
/* ----- component loads ----- */
   fgets(dummy,80,fptr1);
   for (i = 0; i < nl; i++) {
      fscanf(fptr1, "%d %f\n", &k, &c);
      f[k-1] = c;
   }
/* ----- material properties ----- */
   fgets(dummy,80,fptr1);
   for (i = 0; i < nm; i++){
      fscanf(fptr1, "%d", &k);
      for (j = 0; j < npr; j++) {
         fscanf(fptr1, "%f\n", &c);
    	pm[(k-1)*npr+j] = c;
      }
   }
/* ----- multipoint constraints ----- */
   if (nmpc > 0) 
      { fgets(dummy,80,fptr1);
	for(j=0;j<nmpc;j++){
	   fscanf(fptr1,"%f",&c);
	   beta[3*j]=c;
	   fscanf(fptr1,"%d",&k);
	   mpc[2*j]=k;
           fscanf(fptr1,"%f",&c);
	   beta[3*j+1]=c;
           fscanf(fptr1,"%d",&k);
           mpc[2*j+1]=k;
           fscanf(fptr1,"%f",&c);
	   beta[3*j+2]=c;
	   }
	}
   fclose (fptr1);
/* ----- bandwidth nbw from connectivity noc() and mpc ----- */
   nbw = 0;
   for (i = 0; i < ne; i++) {
        nmin = noc[nen*i];
        nmax = nmin;
        for (j = 1; j < 3;j++) {
            n =noc[nen*i+j];
            if (nmin > n)
               nmin = n;
            if (nmax < n)
               nmax = n;
            }  
        n= ndn * (nmax - nmin + 1);
        if (nbw < n)
           nbw = n;
   }    
   for (i = 0; i < nmpc; i++) {
        n = abs(mpc[2*i] - mpc[2*i+1]) + 1;
        if (nbw < n)
           nbw = n;
        }
     printf ("the bandwidth is %d\n", nbw);
/* ----- allocate memory for stiffness ----- */
   s = (float *) calloc(nq*nbw, sizeof(float));
/* ----- global stiffness matrix -----*/
     for (n = 0; n < ne; n++) {
	printf("forming stiffness matrix of element %d\n", n+1);
	dbmat(n,b,d,db,mat,pm,npr,x,noc,&dj,&al,&pnu,&rbar,&i1,&i2,&i3);
     /* --- element stiffness --- */
        for (i = 0; i < 6; i++) {
            for (j = 0; j < 6; j++) {
                c = 0.;
                for (k = 0; k < 4; k++) {
                    c = c + fabs(dj) * b[k][i] * db[k][j] * PI * rbar;
                    }
		se[i][j] = c;
		}
	    }
     /* --- temperature load vector --- */
	c = al * tempr[n] * PI * rbar * fabs(dj);
	for (i = 0; i < 6; i++) {
	    tl[i] = c * (db[0][i] + db[1][i] + db[3][i]);
	    }
	printf (".... placing in global locations\n");
	for (ii = 0; ii < nen; ii++) {
	   nrt = ndn * (noc[nen*n + ii] - 1);
	   for (it = 0; it < ndn; it++) {
	      nr = nrt + it;
	      i = ndn * ii + it;
	      for (jj = 0; jj < nen; jj++) {
		 nct = ndn * (noc[nen*n+jj] - 1);
		 for (jt = 0; jt < ndn; jt++) {
		    j = ndn * jj + jt;
		    nc = nct + jt - nr;
		    if (nc >= 0)
		       s[nbw*nr+nc] = s[nbw*nr+nc] + se[i][j];
		    }
		 }
	      f[nr] = f[nr] + tl[i];
	      }
	   }
     }
/* ----- decide penalty parameter cnst ----- */
     cnst = 0.;
     for (i = 0; i < nq; i++) {
	 if (cnst < s[i*nbw])
	    cnst = s[i*nbw];
	 }
     cnst = cnst * 10000.;
/* ----- modify for displacement boundary conditions ----- */
   for (i = 0; i < nd; i++) {
      k = nu[i];
      s[(k-1)*nbw] = s[(k-1)*nbw] + cnst;
      f[k-1] = f[k-1] + cnst * u[i];
   }
/* ----- modify for multipoint constraints ----- */
   for (i = 0; i < nmpc; i++){
       i1 = mpc[2*i]-1;
       i2 = mpc[2*i+1]-1;
       s[i1*nbw] = s[i1*nbw] + cnst*beta[3*i]*beta[3*i];
       s[i2*nbw] = s[i2*nbw] + cnst*beta[3*i+1]*beta[3*i+1];
       n=i1;
       if (n > i2)
       n = i2;
       m = abs(i2-i1);
       s[n*nbw+m] = s[n*nbw+m]+cnst*beta[3*i]*beta[3*i+1];
       f[i1] = f[i1] + cnst*beta[3*i]*beta[3*i+2];
       f[i2] = f[i2] + cnst*beta[3*i+1]*beta[3*i+2];
       }
/* ----- solution of equations using band solver ----- */
       bansol(s,f,nq,nbw);
/* ----- printing displacements ----- */
   fptr1 = fopen(file2, "w");
   fprintf(fptr1, "\nOutput for input data in file  %s\n", file1);
   printf("%s\n", title);
   fprintf(fptr1, "\n%s\n", title);
   fprintf(fptr1, "bandwidth = %d\n",nbw);
   fprintf(fptr1, "axisymmetric stress analysis\n");
   fprintf(fptr1, "node#     r-displ      z-displ\n");
     printf ("node#     r-displ      z-displ\n");
     for (i = 0; i < nn; i++) {
	 printf(" %4d  %11.4e  %11.4e\n",i+1,f[2*i],f[2*i+1]);
	 fprintf(fptr1," %4d  %11.4e  %11.4e\n",i+1,f[2*i],f[2*i+1]);
	 }
/* ----- reaction calculation ----- */
   printf("node#    reaction\n");
   fprintf(fptr1, "node#     reaction\n");
   for (i = 0; i < nd; i++) {
      k = nu[i];
      reaction = cnst * (u[i] - f[k-1]);
      printf(" %4d  %11.4e\n", k, reaction);
      fprintf(fptr1, " %4d  %11.4e\n", k, reaction);
   }
/* -----  stress calculations ----- */
     fprintf (fptr1, "elem#   sr        sz        trz");
     fprintf (fptr1, "       hoop      s1       s2    angle sr->s1\n");
  for (n = 0; n < ne; n++) {
	dbmat(n,b,d,db,mat,pm,npr,x,noc,&dj,&al,&pnu,&rbar,&i1,&i2,&i3);
	stress(f,i1,i2,i3,al,tempr,n,d,db,str);
/* ----- principal stress calculations ----- */
        if (str[2] == 0.) {
              s1 = str[0];
              s2 = str[1];
	      ang = 0.;
	   if (s2 > s1) {
              s1 = str[1];
              s2 = str[0];
              ang = 90.;
           }
        }
        else {
           c = .5 * (str[0] + str[1]);
           r = .25 * (str[0] - str[1]) *  (str[0] - str[1]);
           r = r + str[2] * str[2];
           r = sqrt ((double) r);
           s1 = c + r;
           s2 = c - r;
	   if (c > str[0]) {
	      ang = 57.2957795 * atan(str[3] / (s1 - str[0]));
	      if (str[2] > 0)
		 ang = 90. - ang;
	      if (str[2] < 0)
		 ang = -90 - ang;
	   }
	   else {                             
	      ang = 57.29577951 * atan( str[2] / (str[0] - s2));
	      }
           }
	fprintf (fptr1, "%3d %9.2e %9.2e %9.2e",n+1,str[0],str[1],str[2]);
	fprintf (fptr1, " %9.2e %9.2e %9.2e %9.2e\n",str[3],s1,s2,ang);
    }
     fclose(fptr1);
     printf( "complete results are in file %s\n", file2);
     return(0);
}
/* ----- db[] matrix ----- */
dbmat(n,b,d,db,mat,pm,npr,x,noc,dj1,al1,pnu1,rbar1,ia,ib,ic)
    int n,npr,*ia,*ib,*ic;
    float *dj1,*pnu1,*al1,*rbar1;
    int *mat,*noc;
    float *x,*pm;
    float b[][6],db[][6],d[][4];
{
    int i,j,k,m,i1,i2,i3;
    float c,e,c1,c2,dj,pnu,al,rbar;
    float r1,r2,r3,z1,z2,z3,r21,r32,r13,z12,z23,z31;
/* ----- d(), b() and db() matrices ----- */
     /* --- first the d-matrix --- */
     m = mat[n]-1;
     e = pm[npr*m];
     pnu = pm[npr*m+1];
     *pnu1 = pnu;
     al = pm[npr*m+2];
     *al1 = al;
     /* --- d() matrix --- */
     c1 = e * (1 - pnu)/((1 + pnu) * (1 -2*pnu));
     c2 = pnu/(1 - pnu); 
     for (i = 0; i < 4; i++){
         for (j = 0; j < 4; j++){
             d[i][j] = 0;
             }
         }
     d[0][0] = c1;
     d[0][1] = c1*c2;
     d[0][3] = c1*c2;
     d[1][0] = d[0][1];
     d[1][1] = c1;
     d[1][3] = c1*c2;
     d[2][2] = .5 * e / (1 + pnu);
     d[3][0] = d[0][3];
     d[3][1] = d[1][3];
     d[3][3] = c1;
/* --- strain-displacement matrix b() --- */
     i1 = noc[3*n]-1;
     i2 = noc[3*n+1]-1;
     i3 = noc[3*n+2]-1;
     *ia = i1;
     *ib = i2;
     *ic = i3;
     r1 = x[2*i1];
     z1 = x[2*i1+1];
     r2 = x[2*i2];
     z2 = x[2*i2+1];
     r3 = x[2*i3];
     z3 = x[2*i3+1];
     r21 = r2 - r1;
     r32 = r3 - r2;
     r13 = r1 - r3;
     z12 = z1 - z2;
     z23 = z2 - z3;
     z31 = z3 - z1;
     dj = r13 * z23 - r32 * z31; /* dj is determinant of jacobian */
     rbar = (r1 + r2 + r3)/3.;
     *dj1 = dj;
     *rbar1 = rbar;
/* --- definition of b() matrix --- */
     b[0][0] = z23 / dj;
     b[1][0] = 0.;
     b[2][0] = r32 / dj;
     b[0][1] = 0.;
     b[1][1] = r32 / dj;
     b[2][1] = z23 / dj;
     b[0][2] = z31 / dj;
     b[1][2] = 0.;
     b[2][2] = r13 / dj;
     b[0][3] = 0;
     b[1][3] = r13 / dj;
     b[2][3] = z31 / dj;
     b[0][4] = z12 / dj;
     b[1][4] = 0.;
     b[2][4] = r21 / dj;
     b[0][5] = 0.;
     b[1][5] = r21 / dj;
     b[2][5] = z12 / dj;
     b[3][0] = 1./(3*rbar);
     b[3][1] = 0.;
     b[3][2] = 1./(3*rbar);
     b[3][3] = 0.;
     b[3][4] = 1./(3*rbar);
     b[3][5] = 0.;
/* --- db matrix db = d*b ---*/
     for (i = 0; i < 4; i++) {
         for (j = 0; j < 6; j++) {
             c = 0.;
             for (k = 0; k < 4; k++) {
		 c = c + d[i][k] * b[k][j];
		 }
	     db[i][j] = c;
	     }
	 }
    return(0);

}
/* ----- stress evaluation ----- */
stress(f,i1,i2,i3,al,tempr,n,d,db,str)
  int i1,i2,i3,n;
  float *f,*tempr,d[][4],db[][6],str[];
  float al;
{  int i,k;
   float c,c1,q[6];
     q[0] = f[2*i1];
     q[1] = f[2*i1+1];
     q[2] = f[2*i2];
     q[3] = f[2*i2+1];
     q[4] = f[2*i3];
     q[5] = f[2*i3+1];
     c1 = al * tempr[n-1];
     for (i = 0; i < 4; i++) {
	 c = 0.;
	for (k = 0; k < 6; k++) {
	    c = c + db[i][k] * q[k];
            }
	 str[i] = c - c1 * (d[i][0] + d[i][1] + d[i][3]);
         }
     return(0);
     }
/* ----- band solver ----- */
bansol(s,f,nq,nbw)
  int nq, nbw;
  float *s, *f;
{
 int n1,k,nk,i,i1,j,j1,kk;
  float c1;
  /* ----- band solver ----- */
  n1 = nq - 1;
  /* --- forward elimination --- */
  for (k = 1; k <= n1; k++) {
     nk = nq - k + 1;
     if (nk > nbw)
	nk = nbw;
     for (i = 2; i <= nk; i++) {
       c1 = s[nbw*(k-1)+i-1] / s[nbw*(k-1)];
       i1 = k + i - 1;
       for (j = i; j <= nk; j++) {
	j1 = j - i + 1;
	s[nbw*(i1-1)+j1-1] = s[nbw*(i1-1)+j1-1] - c1 * s[nbw*(k-1)+j-1];
	}
       f[i1-1] = f[i1-1] - c1 * f[k-1];
       }
     }
  /* --- back-substitution --- */
  f[nq-1] = f[nq-1] / s[nbw*(nq-1)];
  for (kk = 1; kk <= n1;kk++) {
     k = nq - kk;
     c1 = 1 / s[nbw*(k-1)];
     f[k-1] = c1 * f[k-1];
     nk = nq - k + 1;
     if (nk > nbw)
       nk = nbw;
       for (j = 2; j <= nk; j++) {
	 f[k-1] = f[k-1] - c1 * s[nbw*(k-1)+j-1] * f[k + j - 2];
	}
     }
    return(0);
}