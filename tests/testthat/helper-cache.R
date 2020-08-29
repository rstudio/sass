# Turn off the default cache until the calling function exits; when that
# happens, the previous default cache will be restored.
local_disable_cache <- function(env = parent.frame()) {
  orig_cache <- sass_get_default_cache()
  withr::defer(
    sass_set_default_cache(orig_cache),
    envir = env
  )
  sass_set_default_cache(NULL)
}
