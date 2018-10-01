
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
