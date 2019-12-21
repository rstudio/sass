#' Place SASS before and after other SASS
#'
#' [sass_layer()] defines [sass::sass()] input(s) to place before and after
#' existing SASS object(s). To actually surround existing `sass` with a
#' [sass_layer()], use `sass_layer_merge(sass, sass_layer())`.
#'
#' @md
#' @param ... A collection of [sass_layer()]s and/or objects that [as_sass()] understands.
#' @param before A suitable [sass::as_sass()] `input`.
#' @param after A suitable [sass::as_sass()] `input`.
#' @param html_deps An HTML dependency (or a list of them).
#' @export
#' @examples
#' blue <- list(color = "blue !default")
#' red <- list(color = "red !default")
#' green <- list(color = "green !default")
#'
#' # a sass_layer() by itself is not very useful, it just defines some
#' # SASS to place before (before) and after (after)
#' core <- sass_layer(before = blue, after = "body { color: $color; }")
#' core
#' sass(core)
#'
#' # However, by stacking sass_layer()s, we have ability to place
#' # SASS both before and after some other sass (e.g., core)
#' # Here we place a red default _before_ the blue default and export the
#' # color SASS variable as a CSS variable _after_ the core
#' red_layer <- sass_layer(red, after = ":root{ --color: #{$color}; }")
#' sass(sass_layer_merge(core, red_layer))
#' sass(sass_layer_merge(core, red_layer, sass_layer(green)))
#'
sass_layer_merge <- function(...) {
  layers <- dropNulls(rlang::list2(...))
  is_layer <- vapply(layers, is_sass_layer, logical(1))
  layers[!is_layer] <- lapply(layers[!is_layer], function(x) {
    sass_layer(after = x)
  })
  Reduce(sass_layers_join, layers)
}

#' @rdname sass_layer_merge
#' @export
sass_layer <- function(before = "", after = "", html_deps = NULL) {
  if (inherits(html_deps, "html_dependency")) {
    html_deps <- list(html_deps)
  }
  if (!is.null(html_deps)) {
    is_dependency <- vapply(html_deps, inherits, logical(1), "html_dependency")
    if (any(!is_dependency)) stop("html_deps must be a collection of htmltools::htmlDependency() objects", call. = FALSE)
  }

  layer <- list(
    before = as_sass(before),
    after = as_sass(after),
    html_deps = html_deps
  )
  structure(layer, class = "sass_layer")
}

sass_layers_join <- function(layer1, layer2) {
  sass_layer(
    before = as_sass(list(layer2$before, layer1$before)),
    after = as_sass(list(layer1$after, layer2$after)),
    html_deps = c(layer1$html_deps, layer2$html_deps)
  )
}

is_sass_layer <- function(x) {
  inherits(x, "sass_layer")
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

# currently sass_layers are the only thing that
# have a notion of html dependencies
html_dependencies <- function(x) {
  deps <- NULL

  # i.e., htmltools::htmlDependencies()
  html_deps <- attr(x, "html_dependencies", TRUE)
  if (inherits(html_deps, "html_dependency")) {
    html_deps <- list(html_deps)
  }
  deps <- c(deps, html_deps)

  if (is_sass_layer(x)) {
    deps <- c(deps, x$html_deps)
  }

  # recursive case
  if (is.list(x)) {
    new_deps <- unlist(lapply(x, html_dependencies), recursive = FALSE)
    deps <- c(deps, new_deps)
  }

  deps
}
