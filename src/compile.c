#include <R.h>
#include "compile.h"
#include "sass/context.h"

int get_index(SEXP list, const char* name) {
  SEXP names = PROTECT(Rf_getAttrib(list, R_NamesSymbol));
  if (isNull(names)) {
    UNPROTECT(1);
    error("No named options in options list.");
  }

  int n = length(list);
  for (int i = 0; i < n; i++) {
    const char* element_name = CHAR(STRING_ELT(names, i));
    if (strcmp(name, element_name) == 0) {
      UNPROTECT(1);
      return i;
    }
  }
  UNPROTECT(1);
  error("Option %s not found in option list.", name);
}

const char* get_char_element(SEXP list, const char* name) {
  int index = get_index(list, name);
  SEXP value = PROTECT(VECTOR_ELT(list, index));
  if (TYPEOF(value) != STRSXP) {
    UNPROTECT(1);
    error("Invalid type for %s option. Expected string.", name);
  }
  UNPROTECT(1);
  return CHAR(asChar(value));
}

int get_bool_element(SEXP list, const char* name) {
  int index = get_index(list, name);
  SEXP value = PROTECT(VECTOR_ELT(list, index));
  if (TYPEOF(value) != LGLSXP) {
    UNPROTECT(1);
    error("Invalid type for %s option. Expected logical.", name);
  }
  UNPROTECT(1);
  return asLogical(value);
}

int get_int_element(SEXP list, const char* name) {
  int index = get_index(list, name);
  SEXP value = PROTECT(VECTOR_ELT(list, index));
  if ((TYPEOF(value) != INTSXP) && (TYPEOF(value) != REALSXP)) {
    UNPROTECT(1);
    error("Invalid type for %s option. Expected integer.", name);
  }
  int i = asInteger(value);
  if ((i < 0) || (i > 10)) {
    UNPROTECT(1);
    error("Invalid option. Integer value is out of range.");
  }
  UNPROTECT(1);
  return i;
}

void set_options(struct Sass_Options* sass_options, SEXP options) {
  int maximum_options = 13;
  if (length(options) > maximum_options) {
    error("Option list contains unsupported options.");
  } else if (length(options) < maximum_options) {
    error("Option list missing options.");
  }

  sass_option_set_output_path(sass_options, get_char_element(options, "output_path"));
  sass_option_set_output_style(sass_options, get_int_element(options, "output_style"));
  sass_option_set_is_indented_syntax_src(sass_options, get_bool_element(options, "indented_syntax"));
  sass_option_set_source_comments(sass_options, get_bool_element(options, "source_comments"));
  sass_option_set_omit_source_map_url(sass_options, get_bool_element(options, "omit_source_map_url"));
  sass_option_set_source_map_embed(sass_options, get_bool_element(options, "source_map_embed"));
  sass_option_set_source_map_contents(sass_options, get_bool_element(options, "source_map_contents"));
  sass_option_set_source_map_file(sass_options, get_char_element(options, "source_map_file"));
  sass_option_set_source_map_root(sass_options, get_char_element(options, "source_map_root"));
  sass_option_set_include_path(sass_options, get_char_element(options, "include_path"));
  sass_option_set_precision(sass_options, get_int_element(options, "precision"));
  sass_option_set_indent(sass_options, get_char_element(options, "indent"));
  sass_option_set_linefeed(sass_options, get_char_element(options, "linefeed"));
}

SEXP compile_file(SEXP file, SEXP options) {

  const char* input = CHAR(asChar(file));

  // freed by sass_delete_file_context
  struct Sass_File_Context* file_context = sass_make_file_context(input);

  // Both context and options return the same file_context pointer
  // which is freed by sass_delete_file_context
  // https://github.com/sass/libsass/blob/ea0e39767600e879a2774509569a432b56cf4750/src/sass_context.cpp#L646
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
  // sass_delete_data_context will free input
  char* input = sass_copy_c_string(data_string);

  // freed by sass_delete_data_context
  struct Sass_Data_Context* data_context = sass_make_data_context(input);
  // Both context and options return the same data_context
  // pointer which is freed by sass_delete_data_context
  // https://github.com/sass/libsass/blob/ea0e39767600e879a2774509569a432b56cf4750/src/sass_context.cpp#L647
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
