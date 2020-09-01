# Turn off the default cache until the calling function exits; when that
# happens, the previous default cache will be restored.
local_disable_cache <- function(env = parent.frame()) {
  orig_cache <- sass_cache_get()
  withr::defer(
    sass_cache_set(orig_cache),
    envir = env
  )
  sass_cache_set(NULL)
}
