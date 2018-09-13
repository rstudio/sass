
cat0 <- function(...) {
  cat(..., sep = "")
}

format.css <- function(x, ...) {
  as.character(x)
}
format.sass <- function(x, ...) {
  as.character(x)
}

print.css <- function(x, ...) {
  cat0("/* CSS */\n", format(x), "\n")
}
print.sass <- function(x, ...) {
  cat0("/* Sass */\n", format(x), "\n")
}
