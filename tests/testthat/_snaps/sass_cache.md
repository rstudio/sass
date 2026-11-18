# failed cache directory lookup falls back to a temporary cache

    Code
      cache_dir <- sass_cache_context_dir()
    Condition
      Warning in `value[[3L]]()`:
      Error using cache directory. Using temp dir instead.

# failed cache directory creation falls back to a temporary cache

    Code
      cache_dir <- sass_cache_context_dir()
    Condition
      Warning in `dir.create()`:
      '<cache-file>' already exists
      Warning in `value[[3L]]()`:
      Error using cache directory at '<cache-file>'. Using temp dir instead.

