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
#'   may want to disable caching altogether during local development by calling
#'   `options(sass.cache=FALSE)`.
#'
#'   In some cases when developing and modifying .scss files, [sass()] might not
#'   detect changes, and keep using cached .css files instead of rebuilding
#'   them. To be safe, if you are developing a theme with sass, it's best to
#'   turn off caching by calling `options(sass.cache=FALSE)`.
#'
#'   If caching is enabled, [sass()] will attempt to bypass the compilation
#'   process by reusing output from previous [sass()] calls that used equivalent
#'   inputs. This mechanism works by computing a _cache key_ from each [sass()]
#'   call's `input`, `option`, and `cache_key_extra` arguments. If an object
#'   with that hash already exists within the cache directory, its contents are
#'   used instead of performing the compilation. If it does not exist, then
#'   compilation is performed and usual and the results are stored in the cache.
#'
#'   If a file that is included using [sass_file()] changes on disk (i.e. its
#'   last-modified time changes), its previous cache entries will effectively be
#'   invalidated (not removed from disk, but they'll no longer be matched).
#'   However, if a file imported using [sass_file()] itself imports other sass
#'   files using `@import`, changes to those files are invisible to the
#'   cache and you can end up with stale results. To avoid this problem when
#'   developing sass code, it's best to disable caching with
#'   `options(sass.cache=FALSE)`.
#'
#'   By default, the maximum size of the cache is 40 MB. If it grows past that
#'   size, the least-recently-used objects will be evicted from the cache to
#'   keep it under that size. Also by default, the maximum age of objects in the
#'   cache is one week. Older objects will be evicted from the cache.
#'
#'   To clear the default cache, call `sass_cache_get()$reset()`.
#'
#'
#' @inheritParams as_sass
#' @param options Compiler [sass_options()].
#' @param output Specifies path to output file for compiled CSS. May be a
#'   character string or [output_template()]
#' @param write_attachments If the input contains [sass_layer()] objects that
#'   have file attachments, and `output` is not `NULL`, then copy the file
#'   attachments to the directory of `output`. (Defaults to `NA`, which merely
#'   emits a warning if file attachments are present, but does not write them to
#'   disk; the side-effect of writing extra files is subtle and potentially
#'   destructive, as files may be overwritten.)
#' @param cache This can be a directory to use for the cache, a [FileCache]
#'   object created by [sass_file_cache()], or `FALSE` or `NULL` for no caching.
#' @param cache_key_extra additional information to considering when computing
#'   the cache key. This should include any information that could possibly
#'   influence the resulting CSS that isn't already captured by `input`. For
#'   example, if `input` contains something like `"@import sass_file.scss"` you
#'   may want to include the [file.mtime()] of `sass_file.scss` (or, perhaps, a
#'   [packageVersion()] if `sass_file.scss` is bundled with an R package).
#'
#' @return If `output = NULL`, the function returns a string value of the
#'   compiled CSS. If `output` is specified, the compiled CSS is written to a
#'   file and the filename is returned.
#'
#' @seealso <https://sass-lang.com/guide>
#' @export
#' @examples
#' # Raw Sass input
#' sass("foo { margin: 122px * .3; }")
#'
#' # List of inputs, including named variables
#' sass(list(
#'   list(width = "122px"),
#'   "foo { margin: $width * .3; }"
#' ))
#'
#' # Compile a .scss file
#' example_file <- system.file("examples/example-full.scss", package = "sass")
#' sass(sass_file(example_file))
#'
#' # Import a file
#' tmp_file <- tempfile()
#' writeLines("foo { margin: $width * .3; }", tmp_file)
#' sass(list(
#'   list(width = "122px"),
#'   sass_file(tmp_file)
#' ))
#'
#' \dontrun{
#' # ======================
#' # Caching examples
#' # ======================
#' # Very slow to compile
#' fib_sass <- "@function fib($x) {
#'   @if $x <= 1 {
#'     @return $x
#'   }
#'   @return fib($x - 2) + fib($x - 1);
#' }
#'
#' body {
#'   width: fib(27);
#' }"
#'
#' # The first time this runs it will be very slow
#' system.time(sass(fib_sass))
#'
#' # But on subsequent calls, it should be very fast
#' system.time(sass(fib_sass))
#'
#' # sass() can be called with cache=NULL; it will be slow
#' system.time(sass(fib_sass, cache = NULL))
#'
#' # Clear the cache
#' sass_cache_get()$reset()
#' }
#'
#' \dontrun{
#' # Example of disabling cache by setting the default cache to NULL.
#'
#' # Disable the default cache (save the original one first, so we can restore)
#' old_cache <- sass_cache_get()
#' sass_cache_set(NULL)
#' # Will be slow, because no cache
#' system.time(sass(fib_sass))
#'
#' # Restore the original cache
#' sass_cache_set(old_cache)
#' }
sass <- function(
  input = NULL,
  options = sass_options_get(),
  output = NULL,
  write_attachments = NA,
  cache = sass_cache_get(),
  cache_key_extra = NULL)
{

  if (!inherits(options, "sass_options")) {
    stop("Please construct the compile options using `sass_options()`.")
  }
  if (is.null(output) && isTRUE(write_attachments)) {
    stop("sass(write_attachments=TRUE) cannot be used when output=NULL")
  }

  if (identical(cache, FALSE)) {
    cache <- NULL
  } else if (is.character(cache)) {
    # In case it's a directory name
    cache <- sass_cache_get_dir(cache, create = TRUE)
  }

  if (!is.null(cache) && !inherits(cache, "FileCache")) {
    stop("Please use FALSE or NULL (no cache), a string with a directory name, or a FileCache object for `cache`.")
  }

  css <- NULL
  sass_input <- as_sass(input)
  html_deps <- htmlDependencies(sass_input)
  file_attachments <- extract_file_attachments(input)

  # If caching is active, compute the hash key
  cache_key <- if (!is.null(cache)) {
    sass_hash(list(
      # Don't include htmlDependency()s in cache key since they:
      # 1. Are always attached to the return value of sass()
      # 2. Generally won't invalidate Sass->CSS compilation
      # 3. May include a temp directory
      discard_dependencies(input),
      options, cache_key_extra,

      if (isTRUE(write_attachments)) get_file_mtimes(file_attachments)
    ))
  }

  # Resolve output_template(), if need be
  if (is.function(output)) {
    output <- output(options, cache_key)
  }

  # Check the output dir exists (if relevant)
  outdir <- NULL
  if (!is.null(output)) {
    outdir <- fs::path_dir(output)
    if (!dir.exists(outdir)) {
      stop("The output directory '", outdir, "' does not exist")
    }
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
      if (cache$exists(cache_key)) {
        key_file <- file.path(outdir, ".sass_cache_keys")
        cache_hit <- file.exists(output) && file.exists(key_file) &&
          isTRUE(cache_key %in% readLines(key_file, warn = FALSE))
      }
      if (cache_hit) {
        return(attachDependencies(output, html_deps))
      }
    }

    if (!cache_hit) {
      # We had a cache miss, so write to disk now
      css <- compile_data(sass_input, options)
      Encoding(css) <- "UTF-8"

      # Save a note in the output directory that we've already written
      # all the necessary files to this location
      if (!is.null(output)) {
        cat(cache_key, file = file.path(outdir, ".sass_cache_keys"), append = TRUE)
      }

      # In case this same code is running in two processes pointed at the same
      # cache dir, this could return FALSE (if the file didn't exist when we
      # tried to get it, but does exist when we try to write it here), but
      # that's OK -- it should have the same content.
      cache$set_content(cache_key, css)
    }

  } else {
    # If we got here, we're not using a cache.
    css <- compile_data(sass_input, options)
    Encoding(css) <- "UTF-8"
  }

  css <- as_html(css, "css")

  if (!is.null(output)) {
    write_utf8(css, output)
    maybe_write_attachments(file_attachments, outdir, write_attachments)
    return(
      attachDependencies(output, html_deps)
    )
  }

  # Attach HTML dependencies so that placing a sass::sass() call within HTML tags
  # will include the dependencies
  attachDependencies(css, html_deps)
}


#' Compile rules against a Sass Bundle or Sass Layer object
#'
#' Replaces the rules for a [sass_layer()] object with new rules, and compile it.
#' This is useful when (for example) you want to compile a set of rules using
#' variables derived from a theme, but you do not want the resulting CSS for the
#' entire theme -- just the CSS for the specific rules passed in.
#'
#' @param rules A set of sass rules, which will be used instead of the rules
#'   from `layer`.
#' @param bundle A [sass_bundle()] or [sass_layer()] object.
#' @inheritParams sass
#'
#' @examples
#' theme <- sass_layer(
#'   defaults = sass_file(system.file("examples/variables.scss", package = "sass")),
#'   rules = sass_file(system.file("examples/rules.scss", package = "sass"))
#' )
#'
#' # Compile the theme
#' sass(theme)
#'
#' # Sometimes we want to use the variables from the theme to compile other sass
#' my_rules <- ".someclass { background-color: $bg; color: $fg; }"
#' sass_partial(my_rules, theme)
#'
#' @export
sass_partial <- function(
  rules,
  bundle,
  options = sass_options_get(),
  output = NULL,
  write_attachments = NA,
  cache = sass_cache_get(),
  cache_key_extra = NULL)
{
  if (!is_sass_bundle(bundle)) {
    stop("`bundle` must be a `sass_bundle()` object.", call. = FALSE)
  }

  layer <- as_sass_layer(bundle)

  rules <- as_sass(rules)
  layer$rules <- rules
  sass(layer, options, output, write_attachments, cache, cache_key_extra)
}


#' An intelligent (temporary) output file
#'
#' Intended for use with [sass()]'s `output` argument for temporary file
#' generation that is `cache` and `options` aware. In particular, this ensures
#' that new redundant file(s) aren't generated on a [sass()] cache hit, and that
#' the file's extension is suitable for the [sass_options()]'s `output_style`.
#'
#' @param basename a non-empty character string giving the outfile name (without
#'   the extension).
#' @param dirname a non-empty character string giving the initial part of the
#'   directory name.
#' @param fileext the output file extension. The default is `".min.css"` for
#'   compressed and compact output styles; otherwise, its `".css"`.
#' @param path the output file's root directory path.
#'
#' @return A function with two arguments: `options` and `suffix`. When called inside
#' [sass()] with caching enabled, the caching key is supplied to `suffix`.
#'
#' @export
#' @examples
#' sass("body {color: red}", output = output_template())
#'
#' func <- output_template(basename = "foo", dirname = "bar-")
#' func(suffix = "baz")
#'
output_template <- function(basename = "sass", dirname = basename, fileext = NULL, path = tempdir()) {
  function(options = list(), suffix = NULL) {
    fileext <- fileext %||% if (isTRUE(options$output_style %in% c(2, 3))) ".min.css" else ".css"
    # If caching is enabled, then make sure the out dir is unique to the cache key;
    # otherwise, do the more conservative thing of making sure there is a fresh start everytime
    out_dir <- if (is.null(suffix)) {
      tempfile(tmpdir = path, pattern = dirname)
    } else {
      file.path(path, paste0(dirname, suffix))
    }
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    file.path(out_dir, paste0(basename, fileext))
  }
}

maybe_write_attachments <- function(file_attachments, outdir, write_attachments) {
  if (length(file_attachments) == 0) {
    return()
  }

  if (isTRUE(write_attachments)) {
    write_file_attachments(file_attachments, outdir)
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
