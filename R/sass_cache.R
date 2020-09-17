# A registry of caches for different directories
.caches <- new.env(parent = emptyenv())

# Global variable keeping track of the current cache's directory
.globals <- new.env(parent = emptyenv())
.globals$cache_dir <- NA_character_

#' Get/set sass cache, globally
#'
#' @description
#'
#' Use these functions to globally configure [sass()] caching. To disable
#' caching, do `sass_cache_set(NULL)`. To set a caching directory, provide
#' the directory to `sass_cache_set()`. For finer control for the caching size
#' and eviction policy, provide a `sass_file_cache()` (or `[FileCache]`) to
#' `sass_cache_set()`. Note that, by default, caching is enabled and the directory
#' is determined by `sass_cache_context_dir()` (see the details section below).
#'
#' @details
#'

#'
#' @param default a definition for the default cache to use if no global cache
#' has been set. Like `cache`, this can be either a [FileCache] object or a character
#' string specifying a caching directory.
#'
#' @seealso [sass()]
#'
#' @export
sass_cache_get <- function(default = sass_file_cache()) {
  # By default, we want global caching enabled in such a way that the
  # directory doesn't get resolved until we ask for the cache
  if (is.na(.globals$cache_dir)) {
    return(as_sass_cache(default))
  }
  # Support disabling of the global cache via sass_cache_set(NULL)
  if (is.null(.globals$cache_dir)) {
    return(NULL)
  }

  .caches[[.globals$cache_dir]]
}

#' @rdname sass_cache_get
#' @param cache either a [FileCache] object or a character string specifying a caching directory.
#' If `NULL`, then caching is disabled (globally).
#' @export
sass_cache_set <- function(cache = sass_file_cache()) {
  cache <- as_sass_cache(cache)
  # Disable the global cache
  if (is.null(cache) || identical(cache, FALSE)) {
    .globals$cache_dir <- NULL
    return(NULL)
  }
  # Otherwise, update the global registry
  .globals$cache_dir <- cache$directory()
  .caches[[.globals$cache_dir]] <- cache
  invisible(cache)
}

as_sass_cache <- function(cache) {
  if (is.null(cache)) {
    return(cache)
  }
  if (inherits(cache, "FileCache")) {
    return(cache)
  }
  if (is.character(cache)) {
    return(sass_file_cache(cache))
  }
  stop("Can't coerce object of type '", typeof(cache), "' to a cache object.")
}

#' Return the cache directory for the current context.
#'
#' @keywords internal
#' @export
sass_cache_context_dir <- function() {
  tryCatch(
    {
      # The usual place we'll look. This may be superseded below.
      cache_dir <- rappdirs::user_cache_dir("R-sass")

      if (is_shiny_app()) {
        # We might use ./cache/sass, if it's a hosted app, or if the directory
        # already exists. (We won't automatically create the directory on
        # locally-running apps.)
        app_dir <- shiny::getShinyOption("appDir")
        app_cache_dir <- file.path(app_dir, "cache", "sass")
        if (is_hosted_app()) {
          # On hosted platforms, always create a ./cache/sass subdir for caching
          # sass stuff.
          cache_dir <- app_cache_dir

        } else {
          # When running an app in a normal R session...
          if (dir.exists(app_cache_dir) || dir.exists(dirname(app_cache_dir))) {
            # If ./cache/sass or ./cache already exists, use it.
            cache_dir <- app_cache_dir
          }
        }
      }

      if (!dir.exists(cache_dir)) {
        res <- dir.create(cache_dir, recursive = TRUE)
        if (!res) {
          stop("Error creating cache directory")
        }
      }
    },
    error = function(e) {
      # If all of the attempts to find/create a dir failed, just use a temp dir.
      warning("Error using cache directory at '", cache_dir,
              "'. Using temp dir instead.")

      cache_dir <<- tempfile("sass-")
      dir.create(cache_dir)
    }
  )
  normalizePath(cache_dir)
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
