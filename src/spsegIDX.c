#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>

/* -----------------------------------------------------------------------------
   spsegIDX()

   Last updated: 
   Updated: Thu 28 Apr 2011 22:21:50 NZST
            Mon 24 Sept 2018 20:36 
            // Filename changed from spseg() to spsegIDX()

   R objects   : Rx, Ry, Rm, Ridx   
     'Rx'   - a numeric vector containing the population of each group at each
              point. 
     'Ry'   - a numeric vector containing the population of each group at each
              local environment.
     'Rm'   - an integer indicating the number of population groups.
     'Ridx' - a numeric vector indicating indices to calculate.
----------------------------------------------------------------------------- */
SEXP spsegIDX(SEXP Rx, SEXP Ry, SEXP Rm, SEXP Ridx)
{
  int i, j, k, size = length(Rx), nrow, ncol = INTEGER(Rm)[0], *idx, INDEX;
  double *x, *y, *resultsP, *xRowSum, *yRowSum, *xColSum, *yColSum, 
         *xProp, *yProp, xSum, xTmp, yTmp, logM, E, I = -0.1, denominator;
  SEXP results;
  
  PROTECT(Rx = coerceVector(Rx, REALSXP));
  PROTECT(Ry = coerceVector(Ry, REALSXP));
  PROTECT(Ridx = coerceVector(Ridx, INTSXP));
  PROTECT(results = allocVector(REALSXP, ncol * ncol + 3));
  
  x = REAL(Rx); y = REAL(Ry); idx = INTEGER(Ridx);
  resultsP = REAL(results);

  nrow = size / ncol;
  xRowSum = (double*) malloc (nrow * sizeof(double));
  yRowSum = (double*) malloc (nrow * sizeof(double));
  xColSum = (double*) malloc (ncol * sizeof(double));
  yColSum = (double*) malloc (ncol * sizeof(double));
  xProp = (double*) malloc (ncol * sizeof(double));
  yProp = (double*) malloc (size * sizeof(double));

  /* ---------------------------------------------------------------------------
     Below shows how to index a 2 by 2 matrix as an one dimensional array.

     > matrix(0:29, ncol = 3)
           [,1] [,2] [,3]
      [1,]    0   10   20
      [2,]    1   11   21
      [3,]    2   12   22
      [4,]    3   13   23
      [5,]    4   14   24
      [6,]    5   15   25
      [7,]    6   16   26
      [8,]    7   17   27
      [9,]    8   18   28
     [10,]    9   19   29
     
     > as.vector(matrix(0:29, ncol = 3))
      [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
     [23] 23 24 25 26 27 28 29
  --------------------------------------------------------------------------- */
  xSum = 0.0;
  for (i = 0; i < nrow; i++) {
    xTmp = 0.0; yTmp = 0.0;
    for (j = 0; j < ncol; j++) {
      INDEX = i + j * nrow;
      xTmp += x[INDEX];
      yTmp += y[INDEX];
    }
    xRowSum[i] = xTmp;
    yRowSum[i] = yTmp;
    xSum += xTmp;
  }

  for (i = 0; i < ncol; i++) {
    xTmp = 0.0; yTmp = 0.0;
    for (j = 0; j < nrow; j++) {
      INDEX = j + i * nrow;
      xTmp += x[INDEX];
      yTmp += y[INDEX];
      yProp[INDEX] = y[INDEX] / yRowSum[j];
    }
    xColSum[i] = xTmp;
    yColSum[i] = yTmp;
    xProp[i] = xTmp / xSum;
  }
  
  /* ---------------------------------------------------------------------------
     The spatial exposure segregation index (P) is defined as below:
     
         mPn = SUM(x_{pm} / xColSum_{m} * yProp_{pn})
     
     where x_{pm}      = the population of group 'm' at point 'p', 
           xColSum_{m} = the total population of group 'm', and
           yProp_{pn}  = the population proportion of group 'n' at the local 
                         environment 'p'.
  --------------------------------------------------------------------------- */
  if (idx[0] == 1) {
    for (i = 0; i < ncol; i++) {       // Group 'm'
      for (j = 0; j < ncol; j++) {     // Group 'n'
        xTmp = 0.0;
        for (k = 0; k < nrow; k++) {   // Point 'p'
          xTmp += x[k + i * nrow] / xColSum[i] * yProp[k + j * nrow];
        }
        resultsP[j + i * ncol] = xTmp;
      } 
    }
  } else {
    for (i = 0; i < (ncol * ncol); i++)
      resultsP[i] = NA_REAL;
  }

  /* ---------------------------------------------------------------------------
     The spatial information theory index (H) is defined as below:
     
         H = 1 - (1/(xSum * E)) * 
               SUM(xRowSum_{p} * SUM(yProp_{pm} * log_{M}(yProp_{pm})) * -1)
     														 -------------------------------------
                                         Sum (1) - sum over {m}
               -------------------------------------------------------
                              Sum (2) - sum over {p}
     
     where xRowSum_{p} = the total population of all groups at point 'p', 
           yProp_{pn}  = the population proportion of group 'n' at the local 
                         environment 'p',
           M           = the number of population groups, and
           xSum        = the total population in the study region.
     
     E is defined as:
     
         E = (-1) * SUM(xProp_{m} * log_{M}(xProp_{m}))

     where xProp_{m}   = xColSum_{m} / xSum.
  --------------------------------------------------------------------------- */
  if (idx[1] == 1) {
    yTmp = 0.0; logM = log10(ncol); E = 0.0;

    for (i = 0; i < nrow; i++) {						// Sum (2) for points, p
      xTmp = 0.0;
      for (j = 0; j < ncol; j++) {          // Sum (1) for population groups, m
        INDEX = i + j * nrow;
        if (yProp[INDEX] != 0)
          xTmp -= yProp[INDEX] * log10(yProp[INDEX]) / logM;  // Sum (1)
        // Overall regional entropy E
        if (i == 0)
          E -= xProp[j] * (log10(xProp[j]) / log10(ncol));
      }
      yTmp += xRowSum[i] * xTmp;                              // Sum (2)
    }
    resultsP[ncol * ncol] = 1 - (yTmp / (xSum * E));
  } else {
    resultsP[ncol * ncol] = NA_REAL;
  }

  /* ---------------------------------------------------------------------------
     The spatial relative diversity index (R) is defined as below:
     
         R = 1 - 
             SUM(xRowSum_{p} * SUM(yProp_{pm} * (1 - yProp_{pm})) / (xSum * I))
                               ----------------------------------
                                     Sum (1) - sum over {m}
             ------------------------------------------------------------------
                                     Sum (2) - sum over {p}

     where I = SUM((xColSum_{m} / xSum) * (1 - (xColSum_{m} / xSum))).
  --------------------------------------------------------------------------- */
  if (idx[2] == 1) {
    yTmp = 0.0; I = 0.0;

    for (i = 0; i < nrow; i++) {						// Sum (2) for points, p
      xTmp = 0.0;
      for (j = 0; j < ncol; j++) {				  // Sum (1) for population groups, m
        INDEX = i + j * nrow;
        xTmp += yProp[INDEX] * (1 - yProp[INDEX]);
        
        // Interaction index I
        if (i == 0)
          I += xProp[j] * (1 - xProp[j]);
      }
      if (i == 0)
        denominator = xSum * I;
      yTmp += xRowSum[i] * xTmp;
      xTmp = 0.0;
    }
    resultsP[ncol * ncol + 1] = 1 - yTmp / denominator;
  } else {
    resultsP[ncol * ncol + 1] = NA_REAL;
  }

  /* ---------------------------------------------------------------------------
     The spatial dissimilarity index (D) is defined as below:
     
         D = SUM( 
               SUM(
                 (xRowSum_{p} / (2 * xSum * I)) * ABS(yProp_{pm} - xProp_{m})))
               ----------------------------------------------------------------
                                     Sum (1) - sum over {p}
             ------------------------------------------------------------------  
                                     Sum (2) - sum over {m}
                 
               SUM(xRowSum_{p} * SUM(yProp_{pm} * log_{M}(yProp_{pm})) * -1)
     
     where I is defined as in the spatial relative diversity index, R.
  --------------------------------------------------------------------------- */
  if (idx[3] == 1) {
    // If the interaction index was not calculated from the above:
    if (I < 0) {
      I = 0.0;
      for (i = 0; i < ncol; i++)
          I += xProp[i] * (1 - xProp[i]);
    }
    
    xTmp = 0.0; denominator = 2 * xSum * I;
    
    for (i = 0; i < ncol; i++) {				    // Sum (2) for population groups, m
      for (j = 0; j < nrow; j++) {          // Sum (1) for points, p
        INDEX = j + i * nrow;
        xTmp += (xRowSum[j] / denominator) * fabs(yProp[INDEX] - xProp[i]);
      }
    }
    resultsP[ncol * ncol + 2] = xTmp;
  } else {
    resultsP[ncol * ncol + 2] = NA_REAL;
  }

  free(xRowSum); free(xColSum); free(xProp);
  free(yRowSum); free(yColSum); free(yProp);
  
  UNPROTECT(4);
  return(results);
}
