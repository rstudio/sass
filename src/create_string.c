#include <stdlib.h>
#include <string.h>
#include "create_string.h"

char* create_string(const char* value) {
  int n = strlen(value) + 1;
  char* str = (char *) malloc(n);
  strcpy(str, value);
  return str;
}
