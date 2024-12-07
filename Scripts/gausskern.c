/* Problem 2 ===================================================================
  kernel density estimation */
  
  /* import R math for the dnorm() function, where arguments are
* x = (grid pt minus data to evaluate), mean = 0, sd = bw, log = F */
  #include <Rmath.h>
  
  /* inputs: gridpoints vector g of length m
*         observed data vector x of length n
*         bandwidth for gaussian density
* outputs: y = gaussian density estimate of length m */
  
  void gausskern(int *m, int *n, double *g, double *x, double *bw, double *y)
{
  int i, j;
  for (i = 0; i < *m; i++) { /* <-- for i, ..., m points in grid */
    y[i] = 0.0;
    for (j = 0; j < *n; j++) {  /* <-- pile up gauss dens from each data pt j */
      y[i] += dnorm(x[j] - g[i], 0, *bw, FALSE) / *n;
    }
  }
  }
