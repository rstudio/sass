#include <R.h>
#include <Rinternals.h>

#include "sass/context.h"
#include "optiondefs.h"
#include "create_string.h"

const char* get_char_element(SEXP list, int index) {
  return CHAR(asChar(VECTOR_ELT(list, index)));
}

int get_bool_element(SEXP list, int index) {
  return asLogical(VECTOR_ELT(list, index));
}

int get_int_element(SEXP list, int index) {
  return asInteger(VECTOR_ELT(list, index));
}

void set_options(struct Sass_Options* sass_options, SEXP options) {
  char* indent = create_string(get_char_element(options, RSASS_INDENT));
  char* linefeed = create_string(get_char_element(options, RSASS_LINEFEED));
  char* include_path = create_string(get_char_element(options, RSASS_INCLUDE_PATH));
  char* output_path = create_string(get_char_element(options, RSASS_OUTPUT_PATH));
  char* source_map_file = create_string(get_char_element(options, RSASS_SOURCE_MAP_FILE));
  char* source_map_root = create_string(get_char_element(options, RSASS_SOURCE_MAP_ROOT));

  sass_option_set_output_path(sass_options, output_path);
  sass_option_set_output_style(sass_options, get_int_element(options, RSASS_OUTPUT_STYLE));
  sass_option_set_is_indented_syntax_src(sass_options, get_bool_element(options, RSASS_INDENTED_SYNTAX));
  sass_option_set_source_comments(sass_options, get_bool_element(options, RSASS_SOURCE_COMMENTS));
  sass_option_set_omit_source_map_url(sass_options, get_bool_element(options, RSASS_OMIT_SOURCE_MAP_URL));
  sass_option_set_source_map_embed(sass_options, get_bool_element(options, RSASS_SOURCE_MAP_EMBED));
  sass_option_set_source_map_contents(sass_options, get_bool_element(options, RSASS_SOURCE_MAP_CONTENTS));
  sass_option_set_source_map_file(sass_options, source_map_file);
  sass_option_set_source_map_root(sass_options, source_map_root);
  sass_option_set_include_path(sass_options, include_path);
  sass_option_set_precision(sass_options, get_int_element(options, RSASS_PRECISION));
  sass_option_set_indent(sass_options, indent);
  sass_option_set_linefeed(sass_options, linefeed);
}

SEXP compile_file_(SEXP file, SEXP options) {

  const char* input = CHAR(asChar(file));

  // create the file context and get all related structs
  struct Sass_File_Context* file_context = sass_make_file_context(input);
  struct Sass_Context* context = sass_file_context_get_context(file_context);
  struct Sass_Options* sass_options = sass_context_get_options(context);

  set_options(sass_options, options);

  // context is set up, call the compile step now
  int status = sass_compile_file_context(file_context);

  if (status != 0)
    error(sass_context_get_error_message(context));

  // TODO: do I need to protect?
  SEXP ret = mkString(sass_context_get_output_string(context));

  sass_delete_file_context(file_context);

  return ret;
}

SEXP compile_data_(SEXP data, SEXP options) {

  const char* data_string = CHAR(asChar(data));
  // necessary because sass_compile_data_context tries to free
  // string, so we need to allocate it
  char* input = create_string(data_string);

  struct Sass_Data_Context* data_context = sass_make_data_context(input);
  struct Sass_Context* context = sass_data_context_get_context(data_context);
  struct Sass_Options* sass_options = sass_context_get_options(context);

  set_options(sass_options, options);

  // context is set up, call the compile step now
  int status = sass_compile_data_context(data_context);

  if (status != 0)
    error(sass_context_get_error_message(context));

  // TODO: do I need to protect?
  SEXP ret = mkString(sass_context_get_output_string(context));

  sass_delete_data_context(data_context);

  return ret;
}
