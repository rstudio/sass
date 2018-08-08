#include <R.h>
#include <Rinternals.h>

#include "sass/context.h"
#include "optiondefs.h"
#include "create_string.h"

// TODO: add implements in compile.c and defintions here

const char* get_char_element(SEXP list, int index) {
  return CHAR(asChar(VECTOR_ELT(list, index)));
}

int get_bool_element(SEXP list, int index) {
  return asLogical(VECTOR_ELT(list, index));
}

// TODO: check types here
// TODO: check that the index is within the length of list
//       else error
int get_int_element(SEXP list, int index) {
  return asInteger(VECTOR_ELT(list, index));
}

// TODO: potentially add more type checking to prevent segfault
// look implementation on wrong types
// TODO: check length of SEXP options
void set_options(struct Sass_Options* sass_options, SEXP options) {
  sass_option_set_output_path(sass_options, get_char_element(options, RSASS_OUTPUT_PATH));
  sass_option_set_output_style(sass_options, get_int_element(options, RSASS_OUTPUT_STYLE));
  sass_option_set_is_indented_syntax_src(sass_options, get_bool_element(options, RSASS_INDENTED_SYNTAX));
  sass_option_set_source_comments(sass_options, get_bool_element(options, RSASS_SOURCE_COMMENTS));
  sass_option_set_omit_source_map_url(sass_options, get_bool_element(options, RSASS_OMIT_SOURCE_MAP_URL));
  sass_option_set_source_map_embed(sass_options, get_bool_element(options, RSASS_SOURCE_MAP_EMBED));
  sass_option_set_source_map_contents(sass_options, get_bool_element(options, RSASS_SOURCE_MAP_CONTENTS));
  sass_option_set_source_map_file(sass_options, get_char_element(options, RSASS_SOURCE_MAP_FILE));
  sass_option_set_source_map_root(sass_options, get_char_element(options, RSASS_SOURCE_MAP_ROOT));
  sass_option_set_include_path(sass_options, get_char_element(options, RSASS_INCLUDE_PATH));
  sass_option_set_precision(sass_options, get_int_element(options, RSASS_PRECISION));
  sass_option_set_indent(sass_options, get_char_element(options, RSASS_INDENT));
  sass_option_set_linefeed(sass_options, get_char_element(options, RSASS_LINEFEED));
}

SEXP compile_file(SEXP file, SEXP options) {

  const char* input = CHAR(asChar(file));

  // TODO: do these structs get freeed?

  // create the file context and get all related structs
  struct Sass_File_Context* file_context = sass_make_file_context(input);
  struct Sass_Context* context = sass_file_context_get_context(file_context);
  struct Sass_Options* sass_options = sass_context_get_options(context);

  set_options(sass_options, options);

  int status = sass_compile_file_context(file_context);

  if (status != 0)
    error(sass_context_get_error_message(context));

  SEXP ret = PROTECT(mkString(sass_context_get_output_string(context)));

  sass_delete_file_context(file_context);

  UNPROTECT(1);
  return ret;
}

SEXP compile_data(SEXP data, SEXP options) {

  const char* data_string = CHAR(asChar(data));
  char* input = create_string(data_string);

  // TODO: do these structs get freeed?

  struct Sass_Data_Context* data_context = sass_make_data_context(input);
  struct Sass_Context* context = sass_data_context_get_context(data_context);
  struct Sass_Options* sass_options = sass_context_get_options(context);

  set_options(sass_options, options);

  int status = sass_compile_data_context(data_context);

  if (status != 0)
    error(sass_context_get_error_message(context));

  SEXP ret = PROTECT(mkString(sass_context_get_output_string(context)));

  sass_delete_data_context(data_context);

  UNPROTECT(1);
  return ret;
}
