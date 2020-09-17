# A registry of caches for different directories
.caches <- new.env(parent = emptyenv())


#' Get and set the cache object used for a specific directory
#'
#' @details
#'
#' If `sass_cache_get()` is called for a given directory, before
#' `sass_cache_set()` has been called for that directory, then a new
#' [sass_file_cache()] object will be created and set for that directory.
#'
#' After `sass_cache_set()` is called for a directory, any future calls to
#' `sass_cache_get()` with that directory will return that specific cache
#' object. This can be useful if you customize parameters for the cache object,
#' like maximum size or age.
#'
#' @param dir A directory. If `NULL`, this function will return `NULL`.
#' @param cache A [sass_file_cache()] object, or `NULL` if you don't want to use
#'   a cache.
#'
#' @seealso [sass_file_cache()], [sass()]
#'
#' @keywords internal
#' @export
sass_cache_get <- function(dir) {
  if (is.null(dir)) {
    return(NULL)
  }

  dir <- fs::path_abs(dir)
  if (!exists(dir, .caches, inherits = FALSE)) {
    sass_cache_set(dir, sass_file_cache(dir))
  }
  .caches[[dir]]
}

#' @rdname sass_cache_get
#' @export
sass_cache_set <- function(dir, cache) {
  if (!is.null(cache) && !inherits(cache, "FileCache")) {
    stop("`cache` must be a FileCache object or NULL.")
  }
  .caches[[dir]] <- cache
}

#' Get default sass cache object
#'
#' @description
#'
#' Get the default sass cache object or directory, for the given context. The
#' context depends on factors described below.
#'
#' `sass_default_cache()` first checks the `sass.cache` option. If it is set to
#' `FALSE`, then this function returns `NULL`. If it has been set to a string,
#' it is treated as a directory name, and this function returns a
#' `sass_file_cache()` object using that directory. If the option has been set
#' to a `sass_file_cache()` object, then it will return that object.
#'
#' In most cases, this function uses the user's cache directory, by calling
#' `rappdirs::user_cache_dir("R-sass")`.
#'
#' If this function is called from a Shiny application, it will also look for a
#' subdirectory named `cache/`. If it exists, it will use a directory named
#' `cache/sass/` to store the cache.
#'
#' When running a Shiny application in a user R session, it will not create the
#' `cache/` subdirectory, but it will use it if present. This scopes the cache
#' to the application.
#'
#' With Shiny applications hosted on Shiny Server and Connect, it _will_ create
#' a `cache/sass/` subdirectory, so that the cache is scoped to the application
#' and will not interfere with another application's cache.
#'
#' @seealso [sass_cache_get()], [sass()]
#'
#' @export
sass_default_cache <- function() {
  cache_option <- getOption("sass.cache", default = TRUE)
  if (is.null(cache_option) || identical(cache_option, FALSE)) {
    return(NULL)

  } else if (inherits(cache_option, "FileCache")) {
    return(cache_option)

  } else if (is.character(cache_option)) {
    sass_cache_get(cache_option)
  }

  # Default case:
  sass_cache_get(sass_context_cache_dir())
}

#' Return the cache directory for the current context.
#'
#' @keywords internal
#' @export
sass_context_cache_dir <- function() {
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
