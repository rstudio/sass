#include <R.h>
#include <Rinternals.h>
#include "sass/context.h"

SEXP compile_(SEXP file_path) {

  // note that file path is a file or path string
  const char* input = CHAR(asChar(file_path));

  // create the file context and get all related structs
  struct Sass_File_Context* file_ctx = sass_make_file_context(input);
  struct Sass_Context* ctx = sass_file_context_get_context(file_ctx);
  struct Sass_Options* ctx_opt = sass_context_get_options(ctx);

  // configure some options ...
  sass_option_set_precision(ctx_opt, 10);

  // context is set up, call the compile step now
  int status = sass_compile_file_context(file_ctx);

  // print the result or the error to the stdout
  if (status != 0)
    error(sass_context_get_error_message(ctx));

  const char* ret = sass_context_get_output_string(ctx);

  // release allocated memory
  sass_delete_file_context(file_ctx);

  // exit status
  return mkString(ret);
}
