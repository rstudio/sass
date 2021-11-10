#ifndef MTIME_H
#define MTIME_H

#include <Rinternals.h>

SEXP file_mtime(SEXP filename);
SEXP get_path_mtimes(SEXP path);

#endif
