#' Compile Sass to CSS
#'
#' Compile Sass to CSS using LibSass.
#'
#'
#' @param input Accepts raw Sass, a named list of variables, or a list of raw Sass and/or named variables.
#'   See \code{\link{as_sass}} and \code{\link{sass_import}} / \code{\link{sass_file}} for more details.
#' @param options Compiler options for Sass. Please specify options using
#'   \code{\link{sass_options}}.
#' @param output Specifies path to output file for compiled CSS.
#' @param cache_options Caching options for Sass. Please specify options using
#'   \code{\link{sass_cache_options}}. Caching is turned off by default for
#'   interactive R sessions, and turned on for non-interactive ones.
#' @return If \code{output = NULL}, the function returns a string value
#'   of the compiled CSS. If the output path is specified, the compiled
#'   CSS is written to that file and \code{invisible()} is returned.
#' @seealso \url{http://sass-lang.com/guide}
#' @export
#' @examples
#' # raw Sass input
#' sass("foo { margin: 122px * .3; }")
#'
#' # list of inputs, including named variables
#' sass(list(
#'   list(width = "122px"),
#'   "foo { margin: $width * .3; }"
#' ))
#'
#'
#' # import a file
#' tmp_file <- tempfile()
#' writeLines("foo { margin: $width * .3; }", tmp_file)
#' sass(list(
#'   list(width = "122px"),
#'   sass_file(tmp_file)
#' ))
sass <- function(input = NULL, options = sass_options(), output = NULL,
  cache_options = sass_cache_options()) {
  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using `sass_options()`.")
  }
  if (!inherits(cache_options, "sass_cache_options")) {
    stop("Please construct the cache options using `sass_cache_options()`.")
  }

  css <- NULL

  use_cache <- isTRUE(cache_options[["cache"]])
  cache_file <- if (use_cache) {
    cache_file_path(
      normalizePath(cache_options[["cache_dir"]], mustWork = TRUE),
      input,
      options
    )
  }

  if (use_cache) {
    if (file.exists(cache_file)) {
      if (!is.null(output)) {
        tryCatch({
          file.copy(cache_file, output, overwrite = TRUE)
        }, warning = function(w) {
          stop(conditionMessage(w))
        })
        return(invisible())
      } else {
        # TODO: Set encoding to UTF-8?
        css <- paste(readLines(cache_file), collapse = "\n")
      }
    }
  }

  if (is.null(css)) {
    # TODO: Set encoding to UTF-8?
    css <- compile_data(as_sass(input), options)

    # We had a cache miss, so write to disk now
    if (use_cache) {
      # In case this same code is running in two processes pointed at the same
      # cache dir, write to a temp file then rename to the final directory. This
      # is as close as we can get to doing it atomically.
      tmp_cache_file <- paste0(cache_file, ".tmp", Sys.getpid())
      write(css, tmp_cache_file)
      tryCatch({
        file.rename(tmp_cache_file, cache_file)
      }, error = function(e) {
        if (file.exists(cache_file)) {
          # This is fine--we were beaten to the punch by some other process, but
          # the results should be the same.
        } else {
          # Failed for some other reason
          stop(e)
        }
      })
    }
  }

  css <- as_html(css, "css")

  if (!is.null(output)) {
    writeLines(css, output)
    return(invisible())
  } else {
    return(css)
  }
}

cache_file_path <- function(cache_dir, input, options) {
  cache_key <- digest::digest(
    sass_cache_key(list(input, options, utils::packageVersion("sass"))),
    algo = "md5")
  cache_file <- file.path(cache_dir, paste0(cache_key, ".sasscache.css"))

  cache_file
}
