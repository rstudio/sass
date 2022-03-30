#ifndef COMPILE_H
#define COMPILE_H

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP compile_file(SEXP file, SEXP options);
SEXP compile_data(SEXP data, SEXP options);

#ifdef __cplusplus
}
#endif

#endif
