#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
  Check these declarations against the C/Fortran source code.
*/
  
  /* .Call calls */
extern SEXP envconstruct(SEXP x, SEXP y, SEXP v, SEXP dim, SEXP p, SEXP useExp, 
                         SEXP normalise, SEXP d, SEXP e);
extern SEXP spsegIDX(SEXP Rx, SEXP Ry, SEXP Rm, SEXP Ridx);

static const R_CallMethodDef CallEntries[] = {
  {"envconstruct", (DL_FUNC) &envconstruct, 9},
  {"spsegIDX",     (DL_FUNC) &spsegIDX,     4},
  {NULL, NULL, 0}
};

void R_init_seg(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
