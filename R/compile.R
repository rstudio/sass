#' Compile Sass to CSS
#'
#' Compile Sass to CSS using LibSass.
#'
#'
#' @param input Raw Sass text or a file path to a .scss or .sass Sass file.
#'   Note that the LibSass compiler expects .sass files to use the Sass Indented Syntax.
#' @param options Compiler options for Sass. Please specify options using
#'   \code{\link{sass_options}}.
#' @param output Specifies path to output file for compiled CSS.
#' @return If \code{output = NULL}, the function returns a string value
#'   of the compiled CSS. If the output path is specified, the compiled
#'   CSS is written to that file.
#' @seealso \url{http://sass-lang.com/guide}
#' @export
#' @examples
#' sass("foo { margin: 122px * .3; }")
sass <- function(input = NULL, options = sass_options(), output = NULL) {
  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using `sass::sass_options()`.")
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

#' @useDynLib sass, .registration = TRUE
compile_file <- function(file, opts) {
  .Call(C_compile_file, file, opts)
}

compile_data <- function(data, opts) {
  .Call(C_compile_data, data, opts)
}

.onUnload <- function (libpath) {
  library.dynam.unload("sass", libpath)
}
