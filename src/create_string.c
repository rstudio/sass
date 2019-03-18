#include <stdlib.h>
#include <string.h>
#include "create_string.h"

char* create_string(const char* value, int n) {
  // We would call strlen(value) in this function, but gcc 8.3.0 gives a warning
  // about it, so we'll have the caller compute the length and pass it in.
  // https://github.com/rstudio/sass/issues/15
  char* str = (char *) malloc(n + 1);
  strncpy(str, value, n + 1);
  return str;
}
