

# add class html and attr('html') <- TRUE to spoof htmltoolts::HTML(css) output
as_html <- function(x, extra_class = NULL) {
  class(x) <- c(extra_class, "html", "character")
  attr(x, "html") <- TRUE
  x
}
