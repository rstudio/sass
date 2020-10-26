
#' Deprecated
#'
#' Deprecated Sass functions
#'
#' @rdname sass-deprecated
#' @name sass-deprecated
#' @keywords internal
NULL

#' @describeIn sass-deprecated Please use `sass_bundle(...)`
#' @export
sass_layer_merge <- function(...) {
  .Deprecated(msg = "`sass_layer_merge()` is deprecated. Please use `sass_bundle()` instead. See `?sass_bundle()`")
  sass_bundle(...)
}
