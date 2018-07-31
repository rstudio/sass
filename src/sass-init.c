#include "compile.h"
#include <R_ext/Rdynload.h>

void R_init_sass(DllInfo *info) {
  R_RegisterCCallable("sass", "compile",  (DL_FUNC) &compile_);
}
