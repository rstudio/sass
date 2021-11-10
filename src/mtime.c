#include <R.h>
#include "mtime.h"
#include <time.h>
#include <sys/stat.h>

SEXP file_mtime(SEXP filename) {
    const char* path = CHAR(asChar(filename));
    struct stat attr;
    stat(path, &attr);
    return Rf_ScalarReal(attr.st_mtime);
}
