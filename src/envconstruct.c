#include <math.h>
#include <R.h>
#include <Rinternals.h>

/* -----------------------------------------------------------------------------
   envconstruct()

   Start    : 19 April 2011
   Updated  : 23 September 2018
   R objects: xval, yval, data, as.integer(dim), power, as.integer(useExp), 
              as.integer(scale), threshold, error
----------------------------------------------------------------------------- */
SEXP envconstruct(SEXP x, SEXP y, SEXP v, SEXP dim, SEXP p, SEXP useExp,
                  SEXP scale, SEXP d, SEXP e)
{
  int i, j, k, nrow, ncol = INTEGER(dim)[0], expF = INTEGER(useExp)[0],
      scaled = INTEGER(scale)[0];
  double *xP, *yP, *vP, *envP, weight, weightSum, dx, dy, dxy, 
         dist = REAL(d)[0], power = REAL(p)[0], error = REAL(e)[0];
  SEXP env;
  
  PROTECT(x = coerceVector(x, REALSXP));
  PROTECT(y = coerceVector(y, REALSXP));
  PROTECT(v = coerceVector(v, REALSXP));
  
  nrow = length(x);  
  PROTECT(env = allocMatrix(REALSXP, nrow, ncol));

  xP = REAL(x); yP = REAL(y); vP = REAL(v);
  envP = REAL(env);
  
  // Calculate the local environment values of each point in turn
  for(i = 0; i < nrow; i++) {
    weightSum = 0;
    // Calculate the euclidean distance between the point "i" and all other
    // points (including itself) i.e., j = {1, 2, 3, ..., nrow}
    for (j = 0; j < nrow; j++) {
      dx = fabs(xP[i] - xP[j]); dy = fabs(yP[i] - yP[j]);
      // Maximum search radius is given. Any points that are further than the
      // specified distance from the current ont (point "i") will be ignored.
      if (dist >= 0) {
        if (dx > dist || dy > dist)
          continue;      
        dxy = sqrt(dx*dx + dy*dy);
        if (dxy > dist)
          continue;
      } 
      // Maximum search radius is not given. Consider all points.
      else {
        dxy = sqrt(dx*dx + dy*dy);
      }
      
      if (scaled == 0) {
        if (expF == 0) // Inverse distance weight
          weight = 1/pow(dxy + error, power);
        else           // Inverse distance weight using an exponential function
          weight = exp(dxy * power * -1);            
      } 
      
      else {
        /* ---------------------------------------------------------------------
         Code contribution from Benjamin Jarvis, 2 August 2018
         --------------------------------------------------------------------- */         
         if (expF == 0)  // normal-like biweight kernel
           weight = pow(1 - pow(dxy/dist, power), power);
         else           // scaled and normalized exponential decay
           weight = (exp(dxy/dist * power * -1) - 
             exp(power * -1))/(1 - exp(power * -1));
      }

      // Get weighted total
      for (k = 0; k < ncol; k++) {
        if (weightSum == 0)
          envP[i+nrow*k] = weight * vP[j+nrow*k];
        else
          envP[i+nrow*k] += weight * vP[j+nrow*k];
      }
      weightSum += weight;
    }
    
    // Get weighted average
    for (k = 0; k < ncol; k++)
      envP[i+nrow*k] = envP[i+nrow*k] / weightSum;
  }

  UNPROTECT(4);
  return(env);
}
