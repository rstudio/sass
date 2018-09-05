#' Compile Sass to CSS
#'
#' Compile Sass to CSS using libSass.
#'
#'
#' @param input Raw Sass text or a file path to .scss or .sass Sass file.
#'   Note that the libSass compiler expects .sass files to use the Sass Indented Syntax.
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
sass <- function(input = NULL, options = sass_options(), output = NULL) {
  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using sass::sass_options.")
  }
  if (file.exists(input)) {
    css <- compile_file(input, options)
  } else {
    css <- compile_data(input, options)
  }
  class(css) <- "sass"

  if (!is.null(output)) {
    write(css, output)
    return(invisible(css))
  }

  css
}


format.sass <- function(x, ...) {
  as.character(x)
}
#' @export
print.sass <- function(x, ...) {
  cat(format(x), "\n")
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
