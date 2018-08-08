#include <stdlib.h>
#include <string.h>
#include "create_string.h"

char* create_string(const char* value) {
  // strlen doesn't count null terminator
  int n = strlen(value) + 1;
  char* str = (char *) malloc(n);
  strncpy(str, value, n);
  return str;
}
