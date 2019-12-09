     /*-------  program gauss_r2  ----------/
     /*      gauss elimination method      */
     /*          general matrix            */
     /* t.r.chandrupatla and a.d.belegundu */
     /*------------------------------------*/
     #include <stdio.h>
     #include <math.h>
     main()
     {
     /* ----- Gauss Elimination Method to Solve ax = b ----- */
       int n = 8;
       float a[8][8] = {
	    { 6,0,1,2,0,0,2,1 },
	    { 0,5,1,1,0,0,3,0 },
	    { 1,1,6,1,2,0,1,2 },
	    { 2,1,1,7,1,2,1,1 },
	    { 0,0,2,1,6,0,2,1 },
	    { 0,0,0,2,0,4,1,0 },
	    { 2,3,1,1,2,1,5,1 },
	    { 1,0,2,1,1,0,1,3 } };
	float b[8] = { 1,1,1,1,1,1,1,1};
	int i,j,k;
	float c;
    /* ----- forward elimination ----- */
     for (k = 0; k < n - 1; k++) {
	 for (i = k + 1; i < n; i++) {
	     c = a[i][k] / a[k][k];
	     for (j = k + 1; j < n; j++) {
		 a[i][j] = a[i][j] - c * a[k][j];
		 }
	     b[i] = b[i] - c * b[k];
	     }
	  }
     /* ----- back-substitution ----- */
     b[n-1] = b[n-1] / a[n-1][n-1];
     for (i = n - 2; i >= 0; i--) {
	 c = 1 / a[i][i];
	 b[i] = c * b[i];
	 for (k = i + 1; k < n; k++) {
	     b[i] = b[i] - c * a[i][k] * b[k];
	     }
	 }
     printf("\nx() =\n");
     for (i = 0; i < n; i++) {
	 printf("%7.4f ", b[i]);
	 }
     printf("\n");
     return(0);
  }
