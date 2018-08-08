#ifndef COMPILE_H
#define COMPILE_H

#include <Rinternals.h>

SEXP compile_file(SEXP file, SEXP options);
SEXP compile_data(SEXP data, SEXP options);

#endif
