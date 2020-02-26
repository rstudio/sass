
cat0 <- function(...) {
  cat(..., sep = "")
}

#' @export
#' @noRd
format.css <- function(x, ...) {
  as.character(x)
}
#' @export
#' @noRd
format.sass <- function(x, ...) {
  as.character(x)
}

#' @export
#' @noRd
print.css <- function(x, ...) {
  cat0("/* CSS */\n", format(x), "\n") # nolint
}
#' @export
#' @noRd
print.sass <- function(x, ...) {
  cat0("/* Sass */\n", format(x), "\n") # nolint
}


knit_print.css <- function(x, options, ...) {
  # Display the CSS as a string
  if (identical(options$class.output, 'css')) {
    return(as.character(x))
  }
  # Embed the CSS as HTML
  knitr::knit_print(htmltools::tags$style(x))
}

knit_print.sass <- function(x, options, ...) {
  if (isTRUE(options$class.output %in% c('scss', 'sass'))) {
    return(as.character(x))
  }
  knit_print.css(sass(x), options, ...)
}

# Reusable function for registering a set of methods with S3 manually. The
# methods argument is a list of character vectors, each of which has the form
# c(package, genname, class).
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep="."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

.onLoad <- function(...) {
  # htmltools provides methods for knitr::knit_print, but knitr isn't a Depends or
  # Imports of htmltools, only an Enhances. Therefore, the NAMESPACE file has to
  # declare it as an export, not an S3method. That means that R will only know to
  # use our methods if htmltools is actually attached, i.e., you have to use
  # library(htmltools) in a knitr document or else you'll get escaped HTML in your
  # document. This code snippet manually registers our methods with S3 once both
  # htmltools and knitr are loaded.
  registerMethods(list(
    # c(package, genname, class)
    c("knitr", "knit_print", "css"),
    c("knitr", "knit_print", "sass")
  ))
}
