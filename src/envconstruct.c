#include <math.h>
#include <R.h>
#include <Rinternals.h>

/* -----------------------------------------------------------------------------
   envconstruct()

   Computes the population composition of the local environment of each point
   as a distance-weighted average of the population counts at all points.

   x, y   : coordinates (length n)
   v      : population counts (n * m, column-major)
   dim    : number of population groups, m
   p      : power (distance-decay parameter)
   useExp : 1 = negative exponential, 0 = inverse distance
   scale  : 1 = scaled kernels (requires d >= 0), see ?localenv
   d      : maximum search radius; negative means no limit
   e      : small value added to distances for the inverse distance weight

   Scaled kernels contributed by Benjamin Jarvis (2 August 2018).
----------------------------------------------------------------------------- */
SEXP envconstruct(SEXP x, SEXP y, SEXP v, SEXP dim, SEXP p, SEXP useExp,
                  SEXP scale, SEXP d, SEXP e)
{
  int i, j, k, n, m = asInteger(dim), expF = asInteger(useExp),
      scaled = asInteger(scale);
  double *xP, *yP, *vP, *envP, weight, weightSum, dx, dy, dxy,
         dist = asReal(d), power = asReal(p), error = asReal(e);
  SEXP env;

  PROTECT(x = coerceVector(x, REALSXP));
  PROTECT(y = coerceVector(y, REALSXP));
  PROTECT(v = coerceVector(v, REALSXP));

  n = length(x);
  PROTECT(env = allocMatrix(REALSXP, n, m));

  xP = REAL(x); yP = REAL(y); vP = REAL(v); envP = REAL(env);

  for (i = 0; i < n; i++) {
    weightSum = 0.0;
    for (k = 0; k < m; k++)
      envP[i + n * k] = 0.0;

    for (j = 0; j < n; j++) {
      dx = fabs(xP[i] - xP[j]); dy = fabs(yP[i] - yP[j]);
      if (dist >= 0) {
        if (dx > dist || dy > dist)
          continue;
        dxy = sqrt(dx * dx + dy * dy);
        if (dxy > dist)
          continue;
      } else {
        dxy = sqrt(dx * dx + dy * dy);
      }

      if (scaled == 0) {
        if (expF == 0)        /* inverse distance */
          weight = 1 / pow(dxy + error, power);
        else                  /* negative exponential */
          weight = exp(dxy * power * -1);
      } else {
        if (expF == 0)        /* normal-like biweight kernel */
          weight = pow(1 - pow(dxy / dist, power), power);
        else                  /* scaled and normalised exponential decay */
          weight = (exp(dxy / dist * power * -1) - exp(power * -1)) /
                   (1 - exp(power * -1));
      }

      for (k = 0; k < m; k++)
        envP[i + n * k] += weight * vP[j + n * k];
      weightSum += weight;
    }

    for (k = 0; k < m; k++)
      envP[i + n * k] = envP[i + n * k] / weightSum;
  }

  UNPROTECT(4);
  return env;
}
