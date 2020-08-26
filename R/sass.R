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

  use_cache <- isTRUE(cache_options[["cache"]])
  cache_file <- if (use_cache) {
    cache_file_path(
      normalizePath(cache_options[["cache_dir"]], mustWork = TRUE),
      input,
      options
    )
  }

  css <- NULL
  layer <- NULL
  if (use_cache && file.exists(cache_file)) {
    # No need to read cache file if output is specified
    if (!is.null(output)) {
      fs::file_copy(cache_file, output, overwrite = TRUE)
      if (isTRUE(write_attachments == FALSE)) {
        return(invisible())
      }
      layer <- extract_layer(input)
      maybe_write_attachments(layer, output, write_attachments)
      return(invisible())
    }
    # If no output is specified, we need to return a character vector
    # TODO: Set encoding to UTF-8?
    css <- paste(readLines(cache_file), collapse = "\n")
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

  if (!is.null(output)) {
    writeLines(css, output)
    maybe_write_attachments(layer, output, write_attachments)
    return(invisible())
  }

  # Attach HTML dependencies so that placing a sass::sass() call within HTML tags
  # will include the dependencies
  if (is_sass_layer(layer)) {
    css <- htmltools::attachDependencies(css, layer$html_deps)
  }

  css
}

cache_file_path <- function(cache_dir, input, options) {
  cache_key <- digest::digest(
    sass_cache_key(list(input, options, utils::packageVersion("sass"))),
    algo = "xxhash64")
  cache_file <- file.path(cache_dir, paste0(cache_key, ".css"))

  cache_file
}

maybe_write_attachments <- function(layer, output, write_attachments) {
  if (!(is_sass_layer(layer) && length(layer$file_attachments))) {
    return()
  }

  if (isTRUE(write_attachments)) {
    write_file_attachments(
      layer$file_attachments,
      fs::path_dir(output)
    )
    return()
  }

  if (isTRUE(is.na(write_attachments))) {
    warning(
      "sass() input contains file attachments that are being ignored. Pass ",
      "write_attachments=TRUE to write these files to disk, or FALSE to ",
      "suppress this warning.", call. = FALSE
    )
  }
}
