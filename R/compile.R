#' Compile .scss and .sass files to CSS
#'
#' Compile .scss and .sass files to CSS using libSass.
#'
#'
#' @param input path to .scss or .sass file, or string containing
#' code to compile.
#' @param ... options to send to libSass. For a full list of supported
#' options, see the options help page.
#'
#' @return One element character vector containing the compiled CSS.
#'
#' @export
compile <- function(input, ...) {
  if (is_null_or_empty(input)) {
    stop("input is invalid")
  }

  # flatten list
  opts <- dropNulls(as.list(c(list(...), recursive = TRUE)))


  # in node-sass, there are two seperate inputs:
  # file and data, and data overrides file
  if (file.exists(input)) {
    compile_file(input, opts)
  } else {
    compile_data(input, opts)
  }
}

# TODO:
# custom print method so the output looks pretty


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


