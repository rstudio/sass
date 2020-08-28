# DiskCache object for storing generated .css files.
sass_cache <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- DiskCache$new(dir = file.path(tempdir(), "sass-cache"))
    }
    cache
  }
})

#' Returns a hash of the object, including sass_file mtimes
#'
#' This function returns a hash of the object `x`, intended for use in caching.
#' It recuses into lists, and any [sass_file()] objects will have the file's
#' mtime attached as an attribute. This is useful for detecting if the file has
#' been modified.
#'
#' @param x A list with sass objects.
#'
#' @export
sass_hash <- function(x) {
  digest::digest(
    add_sass_file_mtime(list(x, utils::packageVersion("sass"))),
    algo = "xxhash64"
  )
}

# Given an object, return an object that can be \code{digest::digest}-ed into a
# hash key. This traverses the object and adds file mtimes for files imported
# via `sass_file` directives (but not files that are imported by those files).
add_sass_file_mtime <- function(x) {
  if (inherits(x, "sass_file")) {
    # Add the file's mtime to the cache key. This will cause mtime changes to
    # bust the cache.
    input_path <- attr(x, "sass_file_path", exact = TRUE)
    attr(x, "sass_timestamp") <- file.mtime(input_path)
    x
  } else if (inherits(x, "list")) {
    lapply(x, add_sass_file_mtime)
  } else {
    x
  }
}
