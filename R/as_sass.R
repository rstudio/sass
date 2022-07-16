#' Convert an R object into Sass code
#'
#' Converts multiple types of inputs to a single Sass input string for
#' [sass()].
#'
#' @param input Any of the following:
#'   * A character vector containing Sass code.
#'   * A named list containing variable names and values.
#'   * A [sass_file()], [sass_layer()], and/or [sass_bundle()].
#'   * A [list()] containing any of the above.
#'
#' @references <https://sass-lang.com/documentation/at-rules/import>
#' @return a single character value to be supplied to [sass()].
#' @export
#' @examples
#' # Example of regular Sass input
#' as_sass("body { color: \"blue\"; }")
#'
#' # There is support for adding variables
#' as_sass(
#'   list(
#'     list(color = "blue"),
#'    "body { color: $color; }"
#'    )
#' )
#'
#' \donttest{
#' # Add a file name
#' someFile <- tempfile("variables")
#'
#' # Overwrite color to red
#' write("$color: \"red\";", someFile)
#'
#' input <-
#'   as_sass(
#'     list(
#'       list(color = "blue"),
#'       sass_file(someFile),
#'       "body { color: $color; }"
#'       )
#'    )
#'
#' input
#'
#' # The final body color is red
#' sass(input)
#' }
as_sass <- function(input) {
  attachDependencies(
    as_html(as_sass_(input), "sass"),
    find_dependencies(input)
  )
}

find_dependencies <- function(x) {

  deps <- if (is_sass_bundle_like(x)) {
    x <- as_sass_layer(x)
    deps <- x$html_deps
    x$html_deps <- NULL
    deps
  } else if (is_font_collection(x)) {
    x$html_deps
  } else {
    htmlDependencies(x)
  }
  childDeps <- NULL
  # only recurse into generic lists
  if (is.list(x)) {
    childDeps <- unlist(
      lapply(x, find_dependencies),
      recursive = FALSE, use.names = FALSE
    )
  }
  c(childDeps, deps)
}

discard_dependencies <- function(x) {
  if (is_sass_bundle_like(x))  {
    x <- as_sass_layer(x)
    x$html_deps <- NULL
  } else if (is_font_collection(x)) {
    x$html_deps <- NULL
  } else if (inherits(x, "html_dependency")) {
    x <- NULL
  } else {
    htmlDependencies(x) <- NULL
  }
  if (is.list(x)) {
    lapply(x, discard_dependencies)
  } else {
    x
  }
}

as_sass_ <- function(input) {
  UseMethod("as_sass_")
}

as_sass_.default <- function(input) {
  stop("as_sass() not implemented for object of class: '", class(input)[1], "'")
}

as_sass_.NULL <- function(input) {
  "null"
}
as_sass_.numeric <- function(input) {
  as.character(input)
}
as_sass_.logical <- function(input) {
  if (input) {
    "true"
  } else {
    "false"
  }
}

as_sass_.list <- function(input) {
  sass_vals <- mapply(
    SIMPLIFY = FALSE,
    rlang::names2(input),
    input,
    FUN = function(name, val) {
      if (nchar(name) > 0) {
        # if a name is provided...
        collapse0("$", name, ": ", as_sass_(val), ";")
      } else {
        # if no name is provided
        as_sass_(val)
      }
    }
  )
  collapse0(sass_vals)
}

as_sass_.font_collection <- function(input) {
  if (isTRUE(input$default_flag)) {
    paste(input$families, "!default")
  } else {
    input$families
  }
}

as_sass_.sass_layer <- function(input) {
  # concatinate all sass layer content in order
  collapse0(
    c(
      # only collect non-null values
      if (!is.null(input$functions)) as_sass_(input$functions),
      if (!is.null(input$defaults)) as_sass_(input$defaults),
      if (!is.null(input$mixins)) as_sass_(input$mixins),
      if (!is.null(input$declarations)) as_sass_(input$declarations),
      if (!is.null(input$rules)) as_sass_(input$rules)
    )
  )
}

as_sass_.sass_bundle <- function(input) {
  if (length(input$layers) == 0) {
    # if there are no layers, return nothing
    return("")
  }
  as_sass_(as_sass_layer(input))
}

as_sass_.character <- function(input) {
  if (has_any_name(input)) {
    warning(
      "Character vector names are ignored. ",
      "Instead of a named character vector, use a named list to define Sass variables.",
      call. = FALSE
    )
  }
  paste(input, collapse = "\n")
}

#' Sass Import
#'
#' Create an import statement to be used within your Sass file. See
#' <https://sass-lang.com/documentation/at-rules/import> for
#' more details.
#'
#' `sass_file()` adds extra checks to make sure an appropriate file path
#' exists given the input value.
#'
#' Note that the LibSass compiler expects .sass files to use the Sass Indented
#' Syntax.
#'
#' @param input Character string to be placed in an import statement.
#' @param quote Logical that determines if a double quote is added to the import
#'   value. Defaults to \code{TRUE}.
#' @return Fully defined Sass import string.
#' @rdname sass_import
#' @export
#' @examples
#' sass_import("foo")
#' sass_import("$foo", FALSE)
#' \donttest{
#' tmp_scss_file <- tempfile(fileext = ".scss")
#' writeLines("$color: red; body{ color: $color; }", tmp_scss_file)
#' sass_file(tmp_scss_file)
#' sass(sass_file(tmp_scss_file))
#' }
sass_import <- function(input, quote = TRUE) {
  quote_val <- (if (isTRUE(quote)) "\"" else "")
  if (isTRUE(quote)) {
    input <- gsub("\\", "\\\\", input, fixed = TRUE)
    input <- gsub('"', '\\"', input, fixed = TRUE)
    input <- gsub("'", "\\'", input, fixed = TRUE)
  }
  paste0("@import ", quote_val, input, quote_val, ";")
}

#' @rdname sass_import
#' @export
sass_file <- function(input) {

  if (!file.exists(input)) {
    stop("Could not find file: '", input, "' in dir: ", getwd())
  }

  input <- normalizePath(input, mustWork = TRUE)
  # is a file. return an @import statement
  ret <- sass_import(input)

  # Add metadata so we can stir the file mtime into a cache key on demand
  class(ret) <- c("sass_file", class(ret))
  attr(ret, "sass_file_path") <- input

  ret
}
