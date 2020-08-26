#' Create a disk cache object
#'
#' A disk cache object is a key-object store that saves the values as files in a
#' directory on disk. The objects are files on disk. They are stored and
#' retrieved using the `get()` and `set()` methods. Objects are automatically
#' pruned from the cache according to the parameters `max_size`, `max_age`,
#' `max_n`, and `evict`.
#'
#' @section Cache pruning:
#'
#'   Cache pruning occurs when `set()` is called, or it can be invoked manually
#'   by calling `prune()`.
#'
#'   The disk cache will throttle the pruning so that it does not happen on
#'   every call to `set()`, because the filesystem operations for checking the
#'   status of files can be slow. Instead, it will prune once in every 20 calls
#'   to `set()`, or if at least 5 seconds have elapsed since the last prune
#'   occurred, whichever is first. These parameters are currently not
#'   customizable, but may be in the future.
#'
#'   When a pruning occurs, if there are any objects that are older than
#'   `max_age`, they will be removed.
#'
#'   The `max_size` and `max_n` parameters are applied to the cache as a whole,
#'   in contrast to `max_age`, which is applied to each object individually.
#'
#'   If the number of objects in the cache exceeds `max_n`, then objects will be
#'   removed from the cache according to the eviction policy, which is set with
#'   the `evict` parameter. Objects will be removed so that the number of items
#'   is `max_n`.
#'
#'   If the size of the objects in the cache exceeds `max_size`, then objects
#'   will be removed from the cache. Objects will be removed from the cache so
#'   that the total size remains under `max_size`. Note that the size is
#'   calculated using the size of the files, not the size of disk space used by
#'   the files --- these two values can differ because of files are stored in
#'   blocks on disk. For example, if the block size is 4096 bytes, then a file
#'   that is one byte in size will take 4096 bytes on disk.
#'
#'   Another time that objects can be removed from the cache is when `get()` is
#'   called. If the target object is older than `max_age`, it will be removed
#'   and the cache will report it as a missing value.
#'
#' @section Eviction policies:
#'
#'   If `max_n` or `max_size` are used, then objects will be removed from the
#'   cache according to an eviction policy. The available eviction policies are:
#'
#'   \describe{ \item{`"lru"`}{ Least Recently Used. The least recently used
#'   objects will be removed. This uses the filesystem's mtime property. When
#'   "lru" is used, each `get()` is called, it will update the file's mtime. }
#'   \item{`"fifo"`}{ First-in-first-out. The oldest objects will be removed. }
#'   }
#'
#'   Both of these policies use files' mtime. Note that some filesystems
#'   (notably FAT) have poor mtime resolution. (atime is not used because
#'   support for atime is worse than mtime.)
#'
#' @section Sharing among multiple processes:
#'
#'   The directory for a DiskCache can be shared among multiple R processes. To
#'   do this, each R process should have a DiskCache object that uses the same
#'   directory. Each DiskCache will do pruning independently of the others, so
#'   if they have different pruning parameters, then one DiskCache may remove
#'   cached objects before another DiskCache would do so.
#'
#'   Even though it is possible for multiple processes to share a DiskCache
#'   directory, this should not be done on networked file systems, because of
#'   slow performance of networked file systems can cause problems. If you need
#'   a high-performance shared cache, you can use one built on a database like
#'   Redis, SQLite, mySQL, or similar.
#'
#'   When multiple processes share a cache directory, there are some potential
#'   race conditions. For example, if your code calls `exists(key)` to check if
#'   an object is in the cache, and then call `get(key)`, the object may be
#'   removed from the cache in between those two calls, and `get(key)` will
#'   throw an error. Instead of calling the two functions, it is better to
#'   simply call `get(key)`, and use `tryCatch()` to handle the error that is
#'   thrown if the object is not in the cache. This effectively tests for
#'   existence and gets the object in one operation.
#'
#'   It is also possible for one processes to prune objects at the same time
#'   that another processes is trying to prune objects. If this happens, you may
#'   see a warning from `file.remove()` failing to remove a file that has
#'   already been deleted.
#'
#' @keywords internal
#' @importFrom R6 R6Class
#' @import brio
DiskCache <- R6Class("DiskCache",
  public = list(
    #' @description Create a DiskCache object.
    #' @param dir Directory to store files for the cache. If `NULL` (the default) it
    #'   will create and use a temporary directory.
    #' @param max_age Maximum age of files in cache before they are evicted, in
    #'   seconds. Use `Inf` for no age limit.
    #' @param max_size Maximum size of the cache, in bytes. If the cache exceeds
    #'   this size, cached objects will be removed according to the value of the
    #'   `evict`. Use `Inf` for no size limit.
    #' @param max_n Maximum number of objects in the cache. If the number of objects
    #'   exceeds this value, then cached objects will be removed according to the
    #'   value of `evict`. Use `Inf` for no limit of number of items.
    #' @param evict The eviction policy to use to decide which objects are removed
    #'   when a cache pruning occurs. Currently, `"lru"` and `"fifo"` are supported.
    #' @param destroy_on_finalize If `TRUE`, then when the DiskCache object is
    #'   garbage collected, the cache directory and all objects inside of it will be
    #'   deleted from disk. If `FALSE` (the default), it will do nothing when
    #'   finalized.
    #' @param logfile An optional filename or connection object to where logging
    #'   information will be written. To log to the console, use `stdout()`.
    initialize = function(
      dir = NULL,
      max_size = 40 * 1024 ^ 2,
      max_age = Inf,
      max_n = Inf,
      evict = c("lru", "fifo"),
      destroy_on_finalize = FALSE,
      logfile = NULL
    ) {
      if (is.null(dir)) {
        dir <- tempfile("DiskCache-")
      }
      if (!is.numeric(max_size)) stop("max_size must be a number. Use `Inf` for no limit.")
      if (!is.numeric(max_age))  stop("max_age must be a number. Use `Inf` for no limit.")
      if (!is.numeric(max_n))    stop("max_n must be a number. Use `Inf` for no limit.")

      if (!dir.exists(dir)) {
        private$log(paste0("initialize: Creating ", dir))
        dir.create(dir, recursive = TRUE)
      }

      private$dir                 <- normalizePath(dir)
      private$max_size            <- max_size
      private$max_age             <- max_age
      private$max_n               <- max_n
      private$evict               <- match.arg(evict)
      private$destroy_on_finalize <- destroy_on_finalize
      private$logfile             <- logfile

      private$prune_last_time     <- as.numeric(Sys.time())
    },

    #' @description Get the content associated with `key`, and save in a file
    #'   named `outfile`.
    #' @return `TRUE` if the object is found in the cache and copying succeeds,
    #'   `FALSE` otherwise.
    #' @param outfile Name of output file. If `NULL`, return the content as
    #' @param key Key. Must be lowercase numbers and letters.
    #' @param overwrite If the output file already exists, should it be
    #'   overwritten?
    get = function(key, outfile, overwrite = TRUE) {
      private$log(paste0('get: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      if (!is.character(outfile) || length(outfile) != 1) {
        stop("`outfile` must be a length-1 character vector.")
      }

      private$maybe_prune_single(key)

      cache_file <- private$filename_full_path(key)

      if (file.copy(cache_file, outfile, overwrite = overwrite)) {
        private$log(paste0('get: key "', key, '" found and copied to ', normalizePath(outfile)))
        return(TRUE)
      }

      private$log(paste0('get: key "', key, '" is missing'))
      FALSE
    },

    #' @description Get the content associated with `key`, and return as either
    #'   string or a raw vector.
    #' @return A character or raw vector if the object is found in the cache,
    #'   `NULL` otherwise.
    #' @param key Key. Must be lowercase numbers and letters.
    #' @param mode If `"text"`, return the content as a UTF-8-encoded text
    #'   string (a one element char vector). If `"lines"`, return the content as
    #'   a character vector with one element per line. If `"raw"`, return the
    #'   content as a raw vector.
    get_content = function(key, mode = c("string", "lines", "raw")) {
      private$log(paste0('get_content: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      mode <- match.arg(mode)

      private$maybe_prune_single(key)

      cache_file <- private$filename_full_path(key)

      errored <- FALSE
      tryCatch(
        {
          if (mode == "string") {
            result <- read_file(cache_file)
          } else if (mode == "lines") {
            result <- read_lines(cache_file)
          } else if (mode == "raw") {
            result <- read_file_raw(cache_file)
          }
        },
        error = function(e) { errored <<- TRUE }
      )

      if (errored) {
        private$log(paste0('get_content: key "', key, '" is missing'))
        return(NULL)
      }

      result
    },

    #' @description Sets content associated with `key`, from a file named
    #' `infile`.
    #' @return `TRUE` if copying the file into the cache succeeds, `FALSE`
    #'   otherwise.
    #' @param key Key. Must be lowercase numbers and letters.
    #' @param infile Name of input file.
    set = function(key, infile) {
      private$log(paste0('set: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      if (!is.character(infile) || length(infile) != 1) {
        stop("`infile` must be a length-1 character vector.")
      }
      infile <- normalizePath(infile)

      cache_file <- private$filename_full_path(key)

      success <- file.copy(infile, cache_file, overwrite = TRUE)
      if (success) {
        private$log(paste0('set: key "', key, ' from file ', infile))
      } else {
        private$log(paste0('set: error setting key "', key, ' from file ', infile))
      }

      private$prune_throttled()

      success
    },

    #' @description Sets content associated with `key`, from a single-element
    #'   vector.
    #' @return `TRUE` if setting the content in the cache succeeds, `FALSE`
    #'   otherwise.
    #' @param key Key. Must be lowercase numbers and letters.
    #' @param content A character or raw vector. If it's a character vector,
    #'   it will be written with elements collapsed with `\n`, with UTF-8
    #'   encoding.
    set_content = function(key, content) {
      private$log(paste0('set_content: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      if (!is.character(content) && !is.raw(content)) {
        stop("`content` must be a character or raw vector.")
      }
      cache_file <- private$filename_full_path(key)

      success <- TRUE
      tryCatch(
        {
          if (is.character(content)) {
            write_lines(content, cache_file)
          } else if (is.raw(content)) {
            writeBin(content, cache_file)
          }
        },
        error = function(e) { success <<- FALSE }
      )
      if (!success) {
        private$log(paste0('set_content: error setting key "', key, '"'))
      }

      private$prune_throttled()

      success
    },

    #' @description Check if content associated with `key` exists in cache
    #' @param key Key. Must be lowercase numbers and letters.
    #' @return `TRUE` if the object is in the cache, `FALSE` otherwise.
    exists = function(key) {
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      file.exists(private$filename_full_path(key))
    },

    #' @description Get all keys
    #' @return A character vector of all keys currently in the cache.
    keys = function() {
      self$is_destroyed(throw = TRUE)
      dir(private$dir)
    },

    #' @description Remove an object
    #' @param key Key. Must be lowercase numbers and letters.
    #' @return `TRUE` if the object was found and successfully removed, `FALSE`
    #'   otherwise.
    remove = function(key) {
      private$log(paste0('remove: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      file.remove(private$filename_full_path(key))
    },

    #' @description Clear all objects from the cache.
    reset = function() {
      private$log(paste0('reset'))
      self$is_destroyed(throw = TRUE)
      file.remove(dir(private$dir, full.names = TRUE))
      invisible(self)
    },

    #' @description Prune the cache, using the parameters specified by
    #'   `max_size`,   `max_age`, `max_n`, and `evict`.
    prune = function() {
      # TODO: It would be good to add parameters `n` and `size`, so that the
      # cache can be pruned to `max_n - n` and `max_size - size` before adding
      # an object. Right now we prune after adding the object, so the cache
      # can temporarily grow past the limits. The reason we don't do this now
      # is because it is expensive to find the size of the serialized object
      # before adding it.

      private$log(paste0('prune'))
      self$is_destroyed(throw = TRUE)

      current_time <- Sys.time()

      filenames <- dir(private$dir, full.names = TRUE)
      info <- file.info(filenames)
      info <- info[info$isdir == FALSE, ]
      info$name <- rownames(info)
      rownames(info) <- NULL
      # Files could be removed between the dir() and file.info() calls. The
      # entire row for such files will have NA values. Remove those rows.
      info <- info[!is.na(info$size), ]

      # 1. Remove any files where the age exceeds max age.
      if (is.finite(private$max_age)) {
        timediff <- as.numeric(current_time - info$mtime, units = "secs")
        rm_idx <- timediff > private$max_age
        if (any(rm_idx)) {
          private$log(paste0("prune max_age: Removing ", paste(info$name[rm_idx], collapse = ", ")))
          file.remove(info$name[rm_idx])
          info <- info[!rm_idx, ]
        }
      }

      # Sort objects by priority. The sorting is done in a function which can be
      # called multiple times but only does the work the first time.
      info_is_sorted <- FALSE
      ensure_info_is_sorted <- function() {
        if (info_is_sorted) return()

        info <<- info[order(info$mtime, decreasing = TRUE), ]
        info_is_sorted <<- TRUE
      }

      # 2. Remove files if there are too many.
      if (is.finite(private$max_n) && nrow(info) > private$max_n) {
        ensure_info_is_sorted()
        rm_idx <- seq_len(nrow(info)) > private$max_n
        private$log(paste0("prune max_n: Removing ", paste(info$name[rm_idx], collapse = ", ")))
        rm_success <- file.remove(info$name[rm_idx])
        info <- info[!rm_success, ]
      }

      # 3. Remove files if cache is too large.
      if (is.finite(private$max_size) && sum(info$size) > private$max_size) {
        ensure_info_is_sorted()
        cum_size <- cumsum(info$size)
        rm_idx <- cum_size > private$max_size
        private$log(paste0("prune max_size: Removing ", paste(info$name[rm_idx], collapse = ", ")))
        rm_success <- file.remove(info$name[rm_idx])
        info <- info[!rm_success, ]
      }

      private$prune_last_time <- as.numeric(current_time)

      invisible(self)
    },

    #' @description Return the number of items currently in the cache.
    size = function() {
      self$is_destroyed(throw = TRUE)
      length(dir(private$dir))
    },

    #' @description Clears all objects in the cache, and removes the cache
    #'   directory from disk.
    destroy = function() {
      if (self$is_destroyed()) {
        return(invisible(self))
      }

      private$log(paste0("destroy: Removing ", private$dir))
      # First create a sentinel file so that other processes sharing this
      # cache know that the cache is to be destroyed. This is needed because
      # the recursive unlink is not atomic: another process can add a file to
      # the directory after unlink starts removing files but before it removes
      # the directory, and when that happens, the directory removal will fail.
      file.create(file.path(private$dir, "._destroyed__"))
      # Remove all the cache files. This will not remove the sentinel file.
      file.remove(dir(private$dir, full.names = TRUE))
      # Next remove dir recursively, including sentinel file.
      unlink(private$dir, recursive = TRUE)
      private$destroyed <- TRUE
      invisible(self)
    },

    #' @description Reports whether the cache has been destroyed.
    #' @param throw Should this function throw an error if the cache has been
    #'   destroyed?
    is_destroyed = function(throw = FALSE) {
      if (!dir.exists(private$dir) ||
          file.exists(file.path(private$dir, "._destroyed__")))
      {
        # It's possible for another process to destroy a shared cache directory
        private$destroyed <- TRUE
      }

      if (throw) {
        if (private$destroyed) {
          stop("Attempted to use cache which has been destroyed:\n  ", private$dir)
        }

      } else {
        private$destroyed
      }
    },

    #' @description A finalizer for the cache.
    finalize = function() {
      if (private$destroy_on_finalize) {
        self$destroy()
      }
    }
  ),

  private = list(
    dir = NULL,
    max_age = NULL,
    max_size = NULL,
    max_n = NULL,
    evict = NULL,
    destroy_on_finalize = NULL,
    destroyed = FALSE,
    logfile = NULL,

    prune_throttle_counter = 0,
    prune_last_time = NULL,

    filename_full_path = function(filename) {
      file.path(private$dir, filename)
    },

    # A wrapper for prune() that throttles it, because prune() can be
    # expensive due to filesystem operations. This function will prune only
    # once every 20 times it is called, or if it has been more than 5 seconds
    # since the last time the cache was actually pruned, whichever is first.
    # In the future, the behavior may be customizable.
    prune_throttled = function() {
      # Count the number of times prune() has been called.
      private$prune_throttle_counter <- private$prune_throttle_counter + 1

      if (private$prune_throttle_counter > 20 ||
          private$prune_last_time - as.numeric(Sys.time()) > 5)
      {
        self$prune()
        private$prune_throttle_counter <- 0
      }
    },

    # Prunes a single object if it exceeds max_age. If the object does not
    # exceed max_age, or if the object doesn't exist, do nothing.
    maybe_prune_single = function(key) {
      obj <- private$cache[[key]]
      if (is.null(obj)) return()

      timediff <- as.numeric(Sys.time()) - obj$mtime
      if (timediff > private$max_age) {
        private$log(paste0("pruning single object exceeding max_age: Removing ", key))
        rm(list = key, envir = private$cache)
      }
    },

    log = function(text) {
      if (is.null(private$logfile)) return()

      text <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%OS3] DiskCache "), text)
      writeLines(text, private$logfile)
    }
  )
)


validate_key <- function(key) {
  if (!is.character(key) || length(key) != 1 || nchar(key) == 0) {
    stop("Invalid key: key must be single non-empty string.")
  }
  if (grepl("[^a-z0-9]", key)) {
    stop("Invalid key: ", key, ". Only lowercase letters and numbers are allowed.")
  }
}
