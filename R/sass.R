#' Compile Sass to CSS
#'
#' Compile Sass to CSS using LibSass.
#'
#'
#' @param input Accepts raw Sass, file names, Sass file paths, and/or a named list of variables.
#'   See \code{\link{as_sass}} for more detail.
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
#' sass(list(
#'   list(width = "122px"),
#'   "foo { margin: $width * .3; }"
#' ))
sass <- function(input = NULL, options = sass_options(), output = NULL) {
  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using `sass_options()`.")
  }

  css <- compile_data(as_sass(input), options)
  css <- as_html(css, "css")

  if (!is.null(output)) {
    write(css, output)
    return(invisible(css))
  }
  css
}
