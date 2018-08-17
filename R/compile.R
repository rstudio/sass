#' Compile Sass to CSS
#'
#' Compile Sass to CSS using libSass.
#'
#'
#' @param file Path to .scss or .sass Sass file. Note that the libSass compiler
#'   expects .sass files to use the Sass Indented Syntax.
#' @param options Compiler options for Sass. Please specify options using
#'   \code{\link{sass_options}}.
#' @param output Specifies path to output file for compiled CSS.
#' @param text Character vector of Sass code.
#'
#' @return If \code{output = NULL}, the function returns a one element character
#'   vector of the compiled CSS. If the output path is specified, the compiled
#'   CSS is written to that file and the function has no return value.
#'
#' @note \code{file} and \code{text} cannot be compiled at the same time. Please
#'   pass one or the other.
#'
#' @examples
#' compile_sass(text = "foo { margin: 122px * .3; }")
#'
#' @export
compile_sass <- function(file = NULL, options = sass_options(), output = NULL, text = NULL) {
  if (is.null(file) && is.null(text)) {
    stop("No input detected. Please supply Sass file or text to compile.")
  }

  file_input <- FALSE
  text_input <- FALSE

  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("Input file does not exist.")
    }
    file_input <- TRUE
  }

  if (!is.null(text)) {
    text_input <- TRUE
  }

  if (text_input && file_input) {
    stop("Both file and text were supplied. Please supply only a Sass file ",
         "or text, not both.")
  }

  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using sass::sass_options.")
  }

  if (file_input) {
    css <- compile_file(file, options)
  } else {
    css <- compile_data(text, options)
  }

  if (!is.null(output)) {
    write(css, output)
    class(css) <- c("css", class(css))
    return(invisible(css))
  }

  # TODO: is there something that is already a css?
  class(css) <- c("css", class(css))
  css
}

#' @export
print.css <- function(x, ...) {
  cat(x)
}

#' @useDynLib sassr, .registration = TRUE
compile_file <- function(file, opts) {
  .Call(C_compile_file, file, opts)
}

compile_data <- function(data, opts) {
  .Call(C_compile_data, data, opts)
}

.onUnload <- function (libpath) {
  library.dynam.unload("sassr", libpath)
}
