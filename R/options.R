#' Compiler Options for Sass
#'
#' Set compiler `options` for [sass()]. To customize options, either provide
#' `sass_options()` directly to a [sass()] call or set options globally via
#' `sass_options_set()`. When `shiny::devmode()` is enabled,
#' `sass_options_get()` defaults `source_map_embed` and `source_map_contents` to
#' `TRUE`.
#'
#' @param precision Number of decimal places.
#' @param output_style Bracketing and formatting style of the CSS output.
#'   Possible styles: `"nested"`, `"expanded"`, `"compact"`, and
#'   `"compressed"`.
#' @param indented_syntax Enables the compiler to parse Sass Indented Syntax in
#'   strings. Note that the compiler automatically overrides this option to
#'   `TRUE` or `FALSE` for files with .sass and .scss file extensions
#'   respectively.
#' @param indent_type Specifies the indent type as `"space"` or
#'   `"tab"`.
#' @param indent_width Number of tabs or spaces used for indentation. Maximum
#'   10.
#' @param include_path Vector of paths used to resolve `@import`. Multiple
#'   paths are possible using a character vector of paths.
#' @param source_comments Annotates CSS output with line and file comments from
#'   Sass file for debugging.
#' @param linefeed Specifies how new lines should be delimited. Possible values:
#'   `"lf"`, `"cr"`, `"lfcr"`, and `"crlf"`.
#' @param output_path Specifies the location of the output file. Note: this
#'   option will not write the file on disk. It is only for internal reference
#'   with the source map.
#' @param source_map_file Specifies the location for Sass to write the source
#'   map.
#' @param source_map_root Value will be included as source root in the source
#'   map information.
#' @param source_map_embed Embeds the source map as a data URI.
#' @param source_map_contents Includes the contents in the source map
#'   information.
#' @param omit_source_map_url Disable the inclusion of source map information in
#'   the output file. Note: must specify `output_path` when `TRUE`.
#'
#' @return List of Sass compiler options to be used with
#'   [sass()].
#'
#' @examples
#' sass(
#'   "foo { margin: 122px * .3; }",
#'   options = sass_options(output_style = "compact")
#' )
#'
#' @export
sass_options <- function(
  precision = 5,
  output_style = "expanded",
  indented_syntax = FALSE,
  include_path = "",
  source_comments = FALSE,
  indent_type = "space",
  indent_width = 2,
  linefeed = "lf",
  output_path = "",
  source_map_file = "",
  source_map_root = "",
  source_map_embed = FALSE,
  source_map_contents = FALSE,
  omit_source_map_url = FALSE
) {
  if (indent_width > 10) {
    warning("Maximum indent width is 10. Setting to 10...")
    indent_width <- 10
  } else if (indent_width < 0) {
    warning("Minimum indent width is 0. Setting to 0...")
    indent_width <- 0
  }

  indent_type <- switch(
    indent_type,
    tab = "\t",
    space = " ",
    stop("invalid indent type. Please specify \"space\" or \"tab\".")
  )

  indent <- strrep(indent_type, indent_width)

  output_style <- switch(
    output_style,
    nested = 0,
    expanded = 1,
    compact = 2,
    compressed = 3,
    stop("output style not supported.")
  )

  linefeed <- switch(
    linefeed,
    lf = "\n",
    cr = "\r",
    crlf = "\r\n",
    lfcr = "\n\r",
    stop("invalid linefeed.")
  )

  sep <- switch(
    .Platform$OS.type,
    unix = ":",
    windows = ";",
    stop("unknown operating system")
  )

  include_path <- paste(include_path, collapse = sep)

  ret <- list(
    precision = precision,
    output_style = output_style,
    indented_syntax = indented_syntax,
    include_path = include_path,
    source_comments = source_comments,
    indent = indent,
    linefeed = linefeed,
    output_path = output_path,
    source_map_file = source_map_file,
    source_map_root = source_map_root,
    source_map_embed = source_map_embed,
    source_map_contents = source_map_contents,
    omit_source_map_url = omit_source_map_url
  )

  class(ret) <- c("sass_options", class(ret))
  ret
}

#' @rdname sass_options
#' @param ... arguments to [sass_options()]. If specified, these options will take priority over any options set globally (or via `shiny::devmode()`)
#' @importFrom utils modifyList
#' @export
sass_options_get <- function(...) {

  global_opts <- get_shiny_devmode_option(
    "sass.options",
    default = sass_options(),
    devmode_default = sass_options(
      source_map_embed = TRUE,
      source_map_contents = TRUE
    )
  )

  opts <- rlang::list2(...)
  local_opts <- do.call(sass_options, opts)

  utils::modifyList(global_opts, local_opts[names(opts)])
}

#' @rdname sass_options
#' @param x either a `sass_options()` object or `NULL` (to 'clear' the options globally).
#' @export
sass_options_set <- function(x) {
  # allow options to be cleared via sass_options_set(NULL)
  if (!is.null(x) && !inherits(x, "sass_options")) {
    stop("`x` must be a `sass_options()` object.")
  }
  options(sass.options = x)
}
