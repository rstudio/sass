is_null_or_empty <- function(x) {
  is.null(x) || (x == "")
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}
