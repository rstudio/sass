# Turn off the default cache until the calling function exits; when that
# happens, the previous default cache will be restored.
local_disable_cache <- function(env = parent.frame()) {
  old_opts <- options(sass.cache = FALSE)
  withr::defer(
    options(old_opts),
    envir = env
  )
}

# Creates a temporary cache that is used as the default. When the calling
# function exits, the temporary cache is destroyed, and the previous default
# cache is restored.
local_temp_cache <- function(env = parent.frame()) {
  temp_cache <- sass_file_cache(dir = tempfile())
  old_opts <- options(sass.cache = temp_cache)
  withr::defer(
    {
      options(old_opts)
      temp_cache$destroy()
    },
    envir = env
  )
}
