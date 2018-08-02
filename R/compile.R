#' Compile .scss and .sass files to CSS
#'
#' Compile .scss and .sass files to CSS using libSass.
#'
#'
#' @param input path to .scss or .sass file, or string containing
#' code to compile.
#' @param ... options to send to libSass. For a full list of supported
#' options, see the options help page.
#' @param .list send options as a list instead of function arguments
#'
#' @return One element character vector containing the compiled CSS.
#'
#' @export
compile <- function(input, ..., .list = NULL) {
  if (is.null(input)) {
    stop("input is invalid")
  }

  opts <- list(...)
  if (!is.null(.list)) {
    opts <- .list
  }

  opts <- set_options(opts)

  # in node-sass, there are two seperate inputs:
  # file and data, and data overrides file
  if (file.exists(input)) {
    css <- compile_file(input, opts)
  } else {
    css <- compile_data(input, opts)
  }

  class(css) <- c("css", class(css))
  css
}

#' @export
print.css <- function(x, ...) {
  cat(x)
}

set_options <- function(opts) {
  # order defined in optionsdef.h
  # TODO:
  # include_path needs to allow a vector of paths,
  # and then concatenate them however libSass wants
  default_opts <- list(
    precision = 5,
    output_style = "expanded",
    source_comments = FALSE,
    source_map_embed = FALSE,
    source_map_contents = FALSE,
    omit_source_map_url = FALSE,
    indented_syntax = FALSE,
    linefeed = 'lf',
    output_path = "",
    include_path = "",
    source_map_file = "",
    source_map_root = "",
    indent_type = "space",
    indent_width = 2
  )

  selected_opts <- names(opts) %in% names(default_opts)
  if (!all(selected_opts)) {
    stop(names(opts)[!selected_opts][1], " is not a valid option.")
  }

  default_opts[names(opts)] <- opts
  default_opts$indent_width <- if (default_opts$indent_width > 10) {
    warning("Maximum indent width is 10. Setting to 10...")
    10
  } else {
    default_opts$indent_width
  }
  default_opts$indent_type <- if(default_opts$indent_type == "tab") {
    '\t'
  } else if (default_opts$indent_type == "space") {
    ' '
  } else {
    stop("invlad indent type. Please specify 'space' or 'tab'.")
  }

  default_opts$indent <- strrep(
    default_opts$indent_type,
    default_opts$indent_width
  )

  default_opts$indent_type <- NULL
  default_opts$indent_width <- NULL

  default_opts$output_style <- switch(
    default_opts$output_style,
    nested = 0,
    expanded = 1,
    compact = 2,
    compressed = 3,
    stop('output style not supported.')
  )

  default_opts$linefeed <- switch(
    default_opts$linefeed,
    lf = '\n',
    cr = '\r',
    crlf = '\r\n',
    lfcr = '\n\r',
    stop("invalid linefeed.")
  )

  default_opts
}


#' @useDynLib sass compile_file_
compile_file <- function(file, opts) {
  .Call(compile_file_, file, opts)
}

#' @useDynLib sass compile_data_
compile_data <- function(data, opts) {
  .Call(compile_data_, data, opts)
}

.onUnload <- function (libpath) {
  library.dynam.unload("sass", libpath)
}
