collapse0 <- function(x) {
  gsub("\\s+|\\n", "", paste(as.character(x), collapse = ""))
}

expect_css <- function(input, css, output = NULL, ...) {
  res <- sass(input, output = output, ...)
  if (!is.null(output)) {
    res <- read_utf8(res)
  }
  expect_identical(
    collapse0(res),
    collapse0(css)
  )
}
