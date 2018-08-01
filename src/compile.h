#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

#include "sass/context.h"

char* create_string(const char* value) {
  int n = strlen(value) + 1;
  char* str = (char *) malloc(n);
  strcpy(str, value);
  return str;
}

SEXP compile_file_(SEXP file, SEXP opts) {

  const char* input = CHAR(asChar(file));

  // create the file context and get all related structs
  struct Sass_File_Context* file_ctx = sass_make_file_context(input);
  struct Sass_Context* ctx = sass_file_context_get_context(file_ctx);
  struct Sass_Options* ctx_opt = sass_context_get_options(ctx);

  // configure some options ...
  sass_option_set_precision(ctx_opt, 10);

  // context is set up, call the compile step now
  int status = sass_compile_file_context(file_ctx);

  if (status != 0)
    error(sass_context_get_error_message(ctx));

  // TODO: do I need to protect?
  SEXP ret = mkString(sass_context_get_output_string(ctx));

  sass_delete_file_context(file_ctx);

  return ret;
}

SEXP compile_data_(SEXP data, SEXP opts) {

  const char* data_string = CHAR(asChar(data));
  // necessary because sass_compile_data_context tries to free
  // string, so we need to allocate it
  char* input = create_string(data_string);

  struct Sass_Data_Context* data_ctx = sass_make_data_context(input);
  struct Sass_Context* ctx = sass_data_context_get_context(data_ctx);
  struct Sass_Options* ctx_opt = sass_context_get_options(ctx);

  // configure some options ...
  sass_option_set_precision(ctx_opt, 10);

  // context is set up, call the compile step now
  int status = sass_compile_data_context(data_ctx);

  if (status != 0)
    error(sass_context_get_error_message(ctx));

  // TODO: do I need to protect?
  SEXP ret = mkString(sass_context_get_output_string(ctx));

  sass_delete_data_context(data_ctx);

  return ret;
}







