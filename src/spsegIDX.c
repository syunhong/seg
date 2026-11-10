#include <math.h>
#include <R.h>
#include <Rinternals.h>

/* -----------------------------------------------------------------------------
   spsegIDX()

   Reardon and O'Sullivan's (2004) spatial segregation indices.

   Rx   : population of each group at each point (n * m, column-major)
   Ry   : population of each group in each local environment (n * m)
   Rm   : number of population groups, m
   Ridx : integer flags for (exposure, information, diversity, dissimilarity)

   Returns a vector of length m * m + 3: the exposure matrix (row-major),
   followed by H, R and D. Indices that were not requested are NA.
----------------------------------------------------------------------------- */
SEXP spsegIDX(SEXP Rx, SEXP Ry, SEXP Rm, SEXP Ridx)
{
  int i, j, k, size, n, m = asInteger(Rm), *idx, INDEX;
  double *x, *y, *res, *xRowSum, *yRowSum, *xColSum, *xProp, *yProp,
         xSum, xTmp, yTmp, logM, E, I, denominator;
  SEXP results;

  PROTECT(Rx = coerceVector(Rx, REALSXP));
  PROTECT(Ry = coerceVector(Ry, REALSXP));
  PROTECT(Ridx = coerceVector(Ridx, INTSXP));
  PROTECT(results = allocVector(REALSXP, m * m + 3));

  x = REAL(Rx); y = REAL(Ry); idx = INTEGER(Ridx); res = REAL(results);
  size = length(Rx);
  n = size / m;

  xRowSum = (double *) R_alloc(n, sizeof(double));
  yRowSum = (double *) R_alloc(n, sizeof(double));
  xColSum = (double *) R_alloc(m, sizeof(double));
  xProp   = (double *) R_alloc(m, sizeof(double));
  yProp   = (double *) R_alloc(size, sizeof(double));

  /* row sums and total */
  xSum = 0.0;
  for (i = 0; i < n; i++) {
    xTmp = 0.0; yTmp = 0.0;
    for (j = 0; j < m; j++) {
      INDEX = i + j * n;
      xTmp += x[INDEX];
      yTmp += y[INDEX];
    }
    xRowSum[i] = xTmp;
    yRowSum[i] = yTmp;
    xSum += xTmp;
  }

  /* column sums, overall proportions, local proportions */
  for (i = 0; i < m; i++) {
    xTmp = 0.0;
    for (j = 0; j < n; j++) {
      INDEX = j + i * n;
      xTmp += x[INDEX];
      yProp[INDEX] = y[INDEX] / yRowSum[j];
    }
    xColSum[i] = xTmp;
    xProp[i] = xTmp / xSum;
  }

  /* interaction index I, used by both R and D */
  I = 0.0;
  for (i = 0; i < m; i++)
    I += xProp[i] * (1 - xProp[i]);

  /* exposure/isolation, P */
  if (idx[0] == 1) {
    for (i = 0; i < m; i++) {
      for (j = 0; j < m; j++) {
        xTmp = 0.0;
        for (k = 0; k < n; k++)
          xTmp += x[k + i * n] / xColSum[i] * yProp[k + j * n];
        res[j + i * m] = xTmp;
      }
    }
  } else {
    for (i = 0; i < (m * m); i++)
      res[i] = NA_REAL;
  }

  /* information theory, H */
  if (idx[1] == 1) {
    yTmp = 0.0; logM = log10((double) m); E = 0.0;
    for (j = 0; j < m; j++)
      E -= xProp[j] * (log10(xProp[j]) / logM);
    for (i = 0; i < n; i++) {
      xTmp = 0.0;
      for (j = 0; j < m; j++) {
        INDEX = i + j * n;
        if (yProp[INDEX] != 0)
          xTmp -= yProp[INDEX] * log10(yProp[INDEX]) / logM;
      }
      yTmp += xRowSum[i] * xTmp;
    }
    res[m * m] = 1 - (yTmp / (xSum * E));
  } else {
    res[m * m] = NA_REAL;
  }

  /* relative diversity, R */
  if (idx[2] == 1) {
    yTmp = 0.0; denominator = xSum * I;
    for (i = 0; i < n; i++) {
      xTmp = 0.0;
      for (j = 0; j < m; j++) {
        INDEX = i + j * n;
        xTmp += yProp[INDEX] * (1 - yProp[INDEX]);
      }
      yTmp += xRowSum[i] * xTmp;
    }
    res[m * m + 1] = 1 - yTmp / denominator;
  } else {
    res[m * m + 1] = NA_REAL;
  }

  /* dissimilarity, D */
  if (idx[3] == 1) {
    xTmp = 0.0; denominator = 2 * xSum * I;
    for (i = 0; i < m; i++) {
      for (j = 0; j < n; j++) {
        INDEX = j + i * n;
        xTmp += (xRowSum[j] / denominator) * fabs(yProp[INDEX] - xProp[i]);
      }
    }
    res[m * m + 2] = xTmp;
  } else {
    res[m * m + 2] = NA_REAL;
  }

  UNPROTECT(4);
  return results;
}
