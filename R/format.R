
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
format.sass_layer <- function(x, ...) {
  format(as_sass(x))
}
#' @export
#' @noRd
format.sass_bundle <- function(x, ...) {
  format(as_sass(x))
}

#' @export
#' @noRd
print.css <- function(x, ...) {
  cat0("/* CSS */\n", format(x), "\n") # nolint
  invisible(x)
}
#' @export
#' @noRd
print.sass <- function(x, ...) {
  cat0("/* Sass */\n", format(x), "\n") # nolint
  invisible(x)
}
#' @export
#' @noRd
print.sass_layer <- function(
  x, ...,
  # currently only used by `print.sass_bundle`
  header_name = "Sass Layer", info_name = "Sass Layer"
) {
  x_fmt <- format(x)
  if (nchar(x_fmt) == 0) x_fmt <- "/* (empty) */"
  cat0(
    "/* ", header_name, " */\n",
    x_fmt, "\n",
    "/* *** */\n"
  )


  x_other <- x[setdiff(names(x), c("defaults", "declarations", "rules"))]
  if (length(unlist(x_other)) > 0) {
    cat0("\nOther ", info_name, " information:\n")
    utils::str(x_other)
  }
  invisible(x)
}
#' @export
#' @noRd
print.sass_bundle <- function(x, ..., name = NULL) {
  if (length(x$layers) == 0) {
    cat0("/* Sass Bundle: (empty) *** */\n")
    return(invisible(x))
  }

  named_layers <- setdiff(unique(names2(x$layers)), "")
  named_layer_txt <-
    if (length(named_layers) > 0) {
      paste0(": ", paste0(named_layers, collapse = ", "))
    } else {
      NULL
    }
  print(
    as_sass_layer(x),
    header_name = paste0("Sass Bundle", named_layer_txt),
    info_name = "Sass Bundle"
  )
  invisible(x)
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
