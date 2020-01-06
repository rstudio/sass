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
#' @param write_attachments If the input contains \code{\link{sass_layer}}
#'   objects that have file attachments, and \code{output} is not \code{NULL},
#'   then copy the file attachments to the directory of \code{output}. (Defaults
#'   to \code{NA}, which merely emits a warning if file attachments are present,
#'   but does not write them to disk; the side-effect of writing extra files is
#'   subtle and potentially destructive, as files may be overwritten.)
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
#' # import a file
#' tmp_file <- tempfile()
#' writeLines("foo { margin: $width * .3; }", tmp_file)
#' sass(list(
#'   list(width = "122px"),
#'   sass_file(tmp_file)
#' ))
sass <- function(input = NULL, options = sass_options(), output = NULL,
  cache_options = sass_cache_options(), write_attachments = NA) {

  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using `sass_options()`.")
  }
  if (!inherits(cache_options, "sass_cache_options")) {
    stop("Please construct the cache options using `sass_cache_options()`.")
  }
  if (!is.null(output) && !dir.exists(fs::path_dir(output))) {
    stop("The output directory '", fs::path_dir(output), "' does not exist")
  }
  if (is.null(output) && isTRUE(write_attachments)) {
    stop("sass(write_attachments=TRUE) cannot be used when output=NULL")
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
      # TODO: Set encoding to UTF-8?
      css <- paste(readLines(cache_file), collapse = "\n")
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
  layer <- extract_layer(input)

  if (!is.null(layer)) {
    css <- htmltools::attachDependencies(css, layer$html_deps)
  }

  if (!is.null(output)) {

    if (isTRUE(is.na(write_attachments)) &&
        !is.null(layer) &&
        length(layer$file_attachments) > 0) {

      warning(call. = FALSE,
        "sass() input contains file attachments that are being ignored. Pass ",
        "write_attachments=TRUE to write these files to disk, or FALSE to ",
        "suppress this warning."
      )
    }

    writeLines(css, output)
    if (isTRUE(write_attachments)) {
      if (!is.null(layer)) {
        write_file_attachments(
          layer$file_attachments,
          fs::path_dir(output)
        )
      }
    }
    return(invisible(css))
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
