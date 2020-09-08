.globals <- new.env(parent = emptyenv())

#' Get and set the default cache used for compiled css files
#'
#' The default cache is used by the [sass()] function to cache outputs. In some
#' cases when developing and modifying .scss files, [sass()] might not detect
#' changes, and keep using cached .css files instead of rebuilding them. To be
#' safe, if you are developing a theme with sass, it's best to turn off caching
#' by setting `cache` to `NULL`.
#'
#' If caching is enabled, [sass()] will attempt to bypass the compilation
#' process by reusing output from previous [sass()] calls that used equivalent
#' inputs. This mechanism works by computing a \emph{cache key} from each
#' [sass()] call's `input`, `option`, and `cache_key_extra` arguments. If an
#' object with that hash already exists within the cache directory, its contents
#' are used instead of performing the compilation. If it does not exist, then
#' compilation is performed and usual and the results are stored in the cache.
#'
#' If a file that is included using [sass_file()] changes on disk (i.e. its
#' last-modified time changes), its previous cache entries will effectively be
#' invalidated (not removed from disk, but they'll no longer be matched).
#' However, if a file imported using [sass_file()] itself imports other sass
#' files using \code{@import}, changes to those files are invisible to the cache
#' and you will end up with stale results. Caching should be disabled in cases
#' like this.
#'
#' If `sass_cache_get()` is called before `sass_cache_set()`, then it will
#' create a cache in a subdirectory of the system temp directory named
#' `R-sass-cache-username` and set it as the default. Because this cache
#' directory is not in the R process's temp directory, it will persist longer
#' than the R process, typically until a system reboot. Additionally, it will be
#' shared across R processes for the same user on the same system.
#'
#' By default, the maximum size of the cache is 40 MB. If it grows past that
#' size, the least-recently-used objects will be evicted from the cache to keep
#' it under that size. To set the options for the cache, create a [FileCache]
#' object and set it as the default cache.
#'
#' To clear the cache (but keep using it in the future), call
#' `sass_cache_get()$reset()`.
#'
#' @param cache A [sass_file_cache()] object, or `NULL` if you don't want to use a
#'   cache.
#'
#' @seealso sass_file_cache
#'
#' @examples
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
#' @export
sass_cache_get <- function() {
  if (!exists("cache", .globals, inherits = FALSE)) {
    username <- Sys.info()[["user"]]
    sass_cache_set(sass_file_cache())
  }
  .globals$cache
}

#' @rdname sass_cache_get
#' @export
sass_cache_set <- function(cache = sass_file_cache()) {
  if (!is.null(cache) && !inherits(cache, "FileCache")) {
    stop("`cache` must be a FileCache object or NULL.")
  }
  .globals$cache <- cache
}


#' Returns a hash of the object, including sass_file mtimes
#'
#' This function returns a hash of the object `x`, intended for use in caching.
#' It recurses into lists, and any [sass_file()] objects will have the file's
#' mtime attached as an attribute. This is useful for detecting if the file has
#' been modified.
#'
#' @param x A list with sass objects.
#' @noRd
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

#' Caching Options for Sass (defunct)
#'
#' This function is no longer used. Please see [sass_cache_get()].
#' @param cache No longer used.
#' @param cache_dir  No longer used.
#' @export
#' @keywords internal
sass_cache_options <- function(cache, cache_dir) {
  message(
    "The function `sass_cache_options` is no longer used. ",
    "Please see ?sass_cache_set and ?sass_file_cache"
  )
}
