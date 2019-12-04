#' Place SASS before and after other SASS
#'
#' [sass_layer()] defines [sass::sass()] input(s) to place before and after
#' existing SASS object(s). To actually surround existing `sass` with a
#' [sass_layer()], use `sass_layer_merge(sass, sass_layer())`.
#'
#' @md
#' @param ... A collection of [sass_layer()]s and/or objects that [as_sass()] understands.
#' @param pre A suitable [sass::as_sass()] `input`.
#' @param post A suitable [sass::as_sass()] `input`.
#' @param deps An HTML dependency (or a list of them).
#' @export
#' @examples
#' blue <- list(color = "blue !default")
#' red <- list(color = "red !default")
#' green <- list(color = "green !default")
#'
#' # a sass_layer() by itself is not very useful, it just defines some
#' # SASS to place before (pre) and after (post)
#' core <- sass_layer(pre = blue, post = "body { color: $color; }")
#' core
#' sass(core)
#'
#' # However, by stacking sass_layer()s, we have ability to place
#' # SASS both before (pre) and after (post) some other sass (e.g., core)
#' # Here we place a red default _before_ the blue default and export the
#' # color SASS variable as a CSS variable _after_ the core
#' red_layer <- sass_layer(red, post = ":root{ --color: #{$color}; }")
#' sass(sass_layer_merge(core, red_layer))
#' sass(sass_layer_merge(core, red_layer, sass_layer(green)))
#'
sass_layer_merge <- function(...) {
  layers <- dropNulls(rlang::list2(...))
  is_layer <- vapply(layers, is_sass_layer, logical(1))
  layers[!is_layer] <- lapply(layers[!is_layer], function(x) {
    sass_layer(post = x)
  })
  Reduce(sass_layers_join, layers)
}

#' @rdname sass_layer_merge
#' @export
sass_layer <- function(pre = "", post = "", deps = NULL) {
  if (inherits(deps, "html_dependency")) {
    deps <- list(deps)
  }
  if (!is.null(deps)) {
    is_dependency <- vapply(deps, inherits, logical(1), "html_dependency")
    if (any(!is_dependency)) stop("deps must be a collection of htmltools::htmlDependency() objects", call. = FALSE)
  }

  layer <- list(
    pre = as_sass(pre),
    post = as_sass(post),
    deps = deps
  )
  structure(layer, class = "sass_layer")
}

sass_layers_join <- function(layer1, layer2) {
  sass_layer(
    pre = as_sass(list(layer2$pre, layer1$pre)),
    post = as_sass(list(layer1$post, layer2$post)),
    deps = c(layer1$deps, layer2$deps)
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
html_dependencies <- function(x, deps = NULL) {
  # i.e., htmltools::htmlDependencies()
  html_deps <- attr(x, "html_dependencies", TRUE)
  if (inherits(html_deps, "html_dependency")) {
    html_deps <- list(html_deps)
  }
  deps <- c(deps, html_deps)

  if (is_sass_layer(x)) {
    deps <- c(deps, x$deps)
  }

  # recursive case
  if (is.list(x)) {
    new_deps <- unlist(lapply(x, html_dependencies, deps), recursive = FALSE)
    deps <- c(deps, new_deps)
  }

  deps
}
