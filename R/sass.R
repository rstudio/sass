#' Compile Sass to CSS
#'
#' Compile Sass to CSS using LibSass.
#'
#'
#' @param input Accepts raw Sass, a named list of variables, or a list of raw
#'   Sass and/or named variables. See \code{\link{as_sass}} and
#'   \code{\link{sass_import}} / \code{\link{sass_file}} for more details.
#' @param options Compiler options for Sass. Please specify options using
#'   \code{\link{sass_options}}.
#' @param output Specifies path to output file for compiled CSS.
#' @param cache A [FileCache] object created by [sass_file_cache()].
#' @param write_attachments If the input contains \code{\link{sass_layer}}
#'   objects that have file attachments, and \code{output} is not \code{NULL},
#'   then copy the file attachments to the directory of \code{output}. (Defaults
#'   to \code{NA}, which merely emits a warning if file attachments are present,
#'   but does not write them to disk; the side-effect of writing extra files is
#'   subtle and potentially destructive, as files may be overwritten.)
#' @return If \code{output = NULL}, the function returns a string value of the
#'   compiled CSS. If the output path is specified, the compiled CSS is written
#'   to that file and \code{invisible()} is returned.
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
  write_attachments = NA, cache = sass_get_default_cache()) {

  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using `sass_options()`.")
  }
  if (!is.null(cache) && !inherits(cache, "FileCache")) {
    stop("Please use NULL or a FileCache object for `cache`.")
  }
  if (!is.null(output) && !dir.exists(fs::path_dir(output))) {
    stop("The output directory '", fs::path_dir(output), "' does not exist")
  }
  if (is.null(output) && isTRUE(write_attachments)) {
    stop("sass(write_attachments=TRUE) cannot be used when output=NULL")
  }

  css <- NULL
  layer <- NULL

  if (!is.null(cache)) {
    cache_key <- sass_hash(list(input, options))
    cache_hit <- FALSE
    if (is.null(output)) {
      # If no output is specified, we need to return a character vector
      css <- cache$get_content(cache_key)
      if (!is.null(css)) {
        cache_hit <- TRUE
      }
    } else {
      cache_hit <- cache$get_file(cache_key, outfile = output)
      if (cache_hit) {
        if (isTRUE(write_attachments == FALSE)) {
          return(invisible())
        }
        layer <- extract_layer(input)
        maybe_write_attachments(layer, output, write_attachments)
        return(invisible())
      }
    }

    if (!cache_hit) {
      # We had a cache miss, so write to disk now
      css <- compile_data(as_sass(input), options)
      Encoding(css) <- "UTF-8"

      # In case this same code is running in two processes pointed at the same
      # cache dir, this could return FALSE (if the file didn't exist when we
      # tried to get it, but does exist when we try to write it here), but
      # that's OK -- it should have the same content.
      cache$set_content(cache_key, css)
    }

  } else {
    # If we got here, we're not using a cache.
    css <- compile_data(as_sass(input), options)
    Encoding(css) <- "UTF-8"
  }

  css <- as_html(css, "css")
  layer <- extract_layer(input)

  if (!is.null(output)) {
    write_utf8(css, output)
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
