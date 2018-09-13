#' List to Sass converter
#'
#' Converts multiple types of inputs to a single Sass input string for \code{\link{sass}}.
#'
#'   Note that the LibSass compiler expects .sass files to use the Sass Indented Syntax.
#'
#' @param input Either a
#' \itemize{
#'   \item raw Sass string
#'   \item named list containing variable names and values
#'   \item Sass-like file name.
#'
#' }
#' @seealso Visit \url{https://sass-lang.com/documentation/file.SASS_REFERENCE.html#import} for more details.
#' @return a single character value to be supplied to \code{\link{sass}}
#' @export
#' @examples
#' # regular Sass input
#' as_sass("body { color: \"blue\"; }")
#'
#' # add variable support
#' as_sass(list(
#'   list(color = "blue"),
#'   "body { color: $color; }"
#' ))
#'
#' # add a file name
#' someFile <- tempfile("variables")
#' ## overwrite color to red
#' write("$color: \"red\";", someFile)
#' input <- as_sass(list(
#'   list(color = "blue"),
#'   sass_file(someFile),
#'   "body { color: $color; }"
#' ))
#' input
#' ## final body color is red
#' sass(input)
as_sass <- function(input) {
  as_html(as_sass_(input), "sass")
}

as_sass_ <- function(input) {
  UseMethod("as_sass_")
}

as_sass_.default <- function(input) {
  stop("as_sass_() not implemented for object of class: '", class(input)[1], "'")
}

as_sass_.NULL <- function(input) {
  "null"
}
as_sass_.numeric <- function(input) {
  input
}
as_sass_.logical <- function(input) {
  if (input) {
    "true"
  } else {
    "false"
  }
}

as_sass_.list <- function(input) {
  inputNames <- names(input)

  # if it is a list of independent items...
  if (length(inputNames) == 0) {
    inputVals <- lapply(input, as_sass_)

    compiled <- paste0(inputVals, collapse = "\n")
    return(compiled)
  }

  # named list of variables
  if (any(inputNames == "")) {
    stop(
      "If providing sass with a named variable list.  All variables must be named. \n", "
      Missing name at index: ", paste0(which(inputNames == ""), collapse = ", ")
    )
  }

  inputValues <- lapply(input, as_sass_)
  paste0("$", inputNames, ": ", inputValues, ";", collapse = "\n")
}

as_sass_.character <- function(input) {
  # treat like sass text input
  return(input)
}




#' Sass Import
#'
#' Create an import statement to be used within your Sass file. See \url{https://sass-lang.com/documentation/file.SASS_REFERENCE.html#import} for more details.
#'
#' \code{sass_file} adds extra checks to make sure an appropriate file path exists given the input value.
#'
#' @param input Character string to be placed in an import statement.
#' @param quote Logical that determines if a double quote is added to the import value.  Defaults to \code{TRUE}.
#' @return Fully defined Sass import string.
#' @rdname sass_import
#' @export
#' @examples
#' sass_import("foo")
#' sass_import("$foo", FALSE)
#' sass_file("foo.scss")
sass_import <- function(input, quote = TRUE) {
  quote_val <- {if (isTRUE(quote)) "\"" else ""}
  paste0("@import ", quote_val, input, quote_val, ";")
}
#' @rdname sass_import
#' @export
sass_file <- function(input) {
  if (file.exists(input)) {
    # is a file. return an @import statement
    return(sass_import(input))
  }

  # given a partial name, like 'folder/colors.ext'.
  ## should work for
  ### folder/colors.ext
  ### folder/colors.ext.scss
  ### folder/colors.ext.sass
  ### folder/_colors.ext
  ### folder/_colors.ext.scss
  ### folder/_colors.ext.sass
  file_possibilities <- expand.grid(
    c("", "_"),
    basename(input),
    c("", ".scss", ".sass")
  )
  file_possibilities <- apply(file_possibilities, 1, paste0, collapse = "")
  if (dirname(input) != "") {
    file_possibilities <- file.path(dirname(input), file_possibilities)
  }
  if (any(file.exists(file_possibilities))) {
    # is a file. return an @import statement. Let Sass figure it out from here
    return(sass_import(input))
  }

  stop("Could not find file: '", input, "'")
}