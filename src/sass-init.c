#include "compile.h"
#include <R_ext/Rdynload.h>

void R_init_sass(DllInfo *info) {
  R_RegisterCCallable("sass", "compile_file",  (DL_FUNC) &compile_file_);
  R_RegisterCCallable("sass", "compile_data",  (DL_FUNC) &compile_data_);
}
