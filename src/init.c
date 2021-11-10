#include "compile.h"
#include "mtime.h"
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[]  = {
  {"C_compile_file", (DL_FUNC) &compile_file, 2},
  {"C_compile_data", (DL_FUNC) &compile_data, 2},
  {"C_file_mtime", (DL_FUNC) &file_mtime, 1},
  {NULL, NULL, 0}
};

void R_init_sass(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
