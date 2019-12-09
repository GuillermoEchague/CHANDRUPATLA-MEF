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
     float x[8];
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
	 int i,j,k,iter;
	 float c;
	cgsolve(a, b, x, n, &iter);
	printf("\n");
	printf("iterations = %d\n", iter);
	printf("x() = \n");
	for (i = 0; i < n; i++) {
	    printf("%9.4e ", x[i]);
	    }
       printf("\n");
    return(0);
  }
    cgsolve (a, b, x, n, iter1)
      float *a, *b, *x;
      int n, *iter1;
    {
      float *g, *d, *ad, gg1, gg2;
      int iter, i, j;
      float c, al, bt, dad;
      g = (float *)calloc(n, sizeof(float));
      d = (float *)calloc(n, sizeof(float));
      ad = (float *)calloc(n, sizeof(float));      
      for (i = 0; i < n; i++) {
           x[i] = 0.;
           g[i] = -b[i];
           d[i] = b[i];
          }
          gg1 = 0;
        for (i = 0; i < n; i++) {
            gg1 = gg1 + g[i] * g[i];
            }
        iter = 0;
while(gg1 > .000001) {
        iter = iter + 1;
        dad = 0.;
        for (i = 0; i < n; i++) {
	   c = 0;
	   for (j = 0; j < n; j++) {
              c = c + a[n*i + j] * d[j];
              }
           ad[i] = c;
           dad = dad + c * d[i];
           }
        al = gg1 / dad;
        gg2 = 0.;
        for (i = 0; i < n; i++) {
           x[i] = x[i] + al * d[i];
           g[i] = g[i] + al * ad[i];
           gg2 = gg2 + g[i] * g[i];
           }
        bt = gg2 / gg1;
        for (i = 0; i < n; i++) {
           d[i] = -g[i] + bt * d[i];
           }
	gg1 = gg2;
      }
      *iter1 = iter;
      return(0);
  }

