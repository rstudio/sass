#' Compile Sass to CSS
#'
#' Compile Sass to CSS using LibSass.
#'
#' @section Caching:
#'
#'   By default, caching is enabled, meaning that `sass()` avoids the possibly
#'   expensive re-compilation of CSS whenever the same `options` and `input` are
#'   requested. Unfortunately, in some cases, `options` and `input` alone aren't
#'   enough to determine whether new CSS output must be generated. For example,
#'   changes in local file
#'   [imports](https://sass-lang.com/documentation/at-rules/import) that aren't
#'   captured through [sass_file()]/[sass_import()], may lead to a
#'   false-positive cache hit. For this reason, developers are encouraged to
#'   capture such information in `cache_key_extra` (possibly with
#'   `packageVersion('myPackage')` if shipping Sass with a package), and users
#'   may want to disable caching altogether during local development (via
#'   `sass_cache_set(NULL)`).
#'
#' @param input Accepts raw Sass, a named list of variables, or a list of raw
#'   Sass and/or named variables. See [as_sass()] and [sass_import()] /
#'   [sass_file()] for more details.
#' @param options Compiler options for Sass. Please specify options using
#'   [sass_options()].
#' @param output Specifies path to output file for compiled CSS. May be a character
#'   string or [output_file()]
#' @param write_attachments If the input contains [sass_layer()] objects that
#'   have file attachments, and `output` is not `NULL`, then copy the file
#'   attachments to the directory of `output`. (Defaults to `NA`, which merely
#'   emits a warning if file attachments are present, but does not write them to
#'   disk; the side-effect of writing extra files is subtle and potentially
#'   destructive, as files may be overwritten.)
#' @param cache A [FileCache] object created by [sass_file_cache()], or `NULL`
#'   for no caching.
#' @param cache_key_extra additional information to include in the [sass_hash()]
#'   call to determine a cache hit. This should include any information that
#'   could possibly influence the resulting CSS that isn't already captured by
#'   `input`. For example, if `input` contains something like `"@import
#'   sass_file.scss"` you may want to include the [file.mtime()] of
#'   `sass_file.scss` (or, perhaps, a [packageVersion()] if `sass_file.scss` is
#'   bundled with an R package).
#'
#' @return If `output = NULL`, the function returns a string value of the
#'   compiled CSS. If `output` is specified, the compiled CSS is written
#'   to a file and the filename is returned.
#'
#' @seealso <http://sass-lang.com/guide>
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
  write_attachments = NA, cache = sass_cache_get(), cache_key_extra = NULL) {

  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using `sass_options()`.")
  }
  if (!is.null(cache) && !inherits(cache, "FileCache")) {
    stop("Please use NULL (no cache) or a FileCache object for `cache`.")
  }
  if (is.null(output) && isTRUE(write_attachments)) {
    stop("sass(write_attachments=TRUE) cannot be used when output=NULL")
  }

  css <- NULL
  layer <- extract_layer(input)

  # If caching is active, compute the hash key
  cache_key <- if (!is.null(cache)) {
    sass_hash(list(
      input, options, cache_key_extra,
      # Detect if any attachments have changed
      if (is.list(layer) && !is.null(layer$file_attachments)) get_file_mtimes(layer$file_attachments)
    ))
  }

  # Resolve output_file(), if need be
  if (is.function(output)) {
    output <- output(options, cache_key)
  }
  if (!is.null(output) && !dir.exists(fs::path_dir(output))) {
    stop("The output directory '", fs::path_dir(output), "' does not exist")
  }

  if (!is.null(cache)) {
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
          return(output)
        }
        maybe_write_attachments(layer, output, write_attachments)
        return(output)
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

  if (!is.null(output)) {
    write_utf8(css, output)
    maybe_write_attachments(layer, output, write_attachments)
    return(output)
  }

  # Attach HTML dependencies so that placing a sass::sass() call within HTML tags
  # will include the dependencies
  if (is_sass_layer(layer)) {
    css <- htmltools::attachDependencies(css, layer$html_deps)
  }

  css
}

#' An intelligent (temporary) output file
#'
#' Intended for use with [sass()]'s `output` argument for temporary file
#' generation that is `cache` and `options` aware. In particular, this ensures
#' that new redundant file(s) aren't generated on a [sass()] cache hit, and that
#' the file's extension is suitable for the [sass_options()]'s `output_style`.
#'
#' @param basename a non-empty character vector giving the outfile name (without
#'   the extension).
#' @param dirname a non-empty character vector giving the initial part of the
#'   directory name.
#' @param fileext the output file extension. The default is `".min.css"` for
#'   compressed and compact output styles; otherwise, its `".css"`.
#'
#' @return A function with two arguments: `options` and `suffix`. When called inside
#' [sass()] with caching enabled, the caching key is supplied to `suffix`.
#'
#' @export
#' @examples
#' sass("body {color: red}", output = output_file())
#'
#' func <- output_file(basename = "foo", dirname = "bar-")
#' func(suffix = "baz")
#'
output_file <- function(basename = "sass", dirname = basename, fileext = NULL) {
  function(options = list(), suffix = NULL) {
    fileext <- fileext %||% if (isTRUE(options$output_style %in% c(2, 3))) ".min.css" else ".css"
    # If caching is enabled, then make sure the out dir is unique to the cache key;
    # otherwise, do the more conservative thing of making sure there is a fresh start everytime
    out_dir <- if (is.null(suffix)) {
      tempfile(pattern = dirname)
    } else {
      file.path(tempdir(), paste0(dirname, suffix))
    }
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    file.path(out_dir, paste0(basename, fileext))
  }
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
