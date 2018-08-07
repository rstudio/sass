#' Compile Sass to CSS
#'
#' Compile Sass to CSS using libSass.
#'
#'
#' @param input Path to .scss or .sass file, or string containing
#' code to compile.
#' @param options Compiler options for Sass. Please specify options
#' using \code{\link{opts}}.
#'
#' @return One element character vector containing the compiled CSS.
#'
#' @examples
#' compile("foo { margin: 122px * .3; }")
#'
#' @export
compile <- function(input, options = sass::opts()) {
  if (is.null(input)) {
    stop("input is invalid")
  }

  if (!inherits(options, "sass_options")) {
    warning("options not constructed using sass::opts.",
            " compile may not work as expected.")
  }

  # in node-sass, there are two seperate inputs:
  # file and data, and data overrides file
  if (file.exists(input)) {
    css <- compile_file(input, options)
  } else {
    css <- compile_data(input, options)
  }

  class(css) <- c("css", class(css))
  css
}

#' @export
print.css <- function(x, ...) {
  cat(x)
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
