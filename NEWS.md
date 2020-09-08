# sass 0.2.0.9000

This release improves the caching of CSS compilation in `sass()`. Previously, caching was (by default) enabled in non-`interactive()` sessions and was allowed to grow indefinitely within `tempdir()` (i.e., within an R session). Now, caching is (by default) always enabled in the user's cache directory (i.e., it persists across R session) and is automatically pruned when the cache size exceeds 40MB. The cache's directory and size limitations are configurable via the new `sass_file_cache()` (in addition, other pruning details such as the eviction policy are configurable via the lower-level `FileCache` R6 class). These cache objects can be either passed directly to a `sass()` call and/or set globally via the new `sass_cache_set()` function.

Although caching is now enabled by default, it still may not be desirable for local `interactive()` use (because of a false-positive caching), so consider disabling it with `sass_cache_set(NULL)` during local development. Also, to help fight against false-positive caching, `sass()` now includes a `cache_key_extra` argument which may be used to capture information that the Sass `input` may not capture, such as file imports (for more, see the details section of `?sass`).

Other improvements include: 

* A new `output_template()` function for more convenient `output` path creation that is `cache` and `options` aware. 

* When `sass()` has a cache hit, and `output` is specified, the cached file is now simply copied to `output` at the OS level (previously, `sass()` was reading the cache file into R, then writing it to `output`). (#42)

## Breaking changes

* When `output` is specified, `sass()` now returns the output file path instead of the CSS content as a character vector.

* The `cache_options` argument in `sass()` has been renamed to `cache` and now defaults to `sass_cache_get()` instead of `sass_cache_options()`.

* `sass_cache_options()` has been deprecated (it no longer does anything) in favor of the new caching functions (`sass_file_cache()`).


# sass 0.2.0

* Added new `sass_layer()` and `sass_layer_merge()` functions. See [here](https://rstudio.github.io/sass/articles/sass.html#layers) for more details.

* The objects that `sass()` and `as_sass()`) return now have better print methods in **rmarkdown**. See [here](https://rstudio.github.io/sass/articles/sass.html#rmarkdown) for more details.

* Added the ability for `sass()` to retain `htmltools::htmlDependency()`s attached to it's `input`.  

* Fixed an issue with incorrect handling of length 2 or more character vector input (#37).

# sass 0.1.2

* No significant changes other than CRAN compliance.

# sass 0.1.1

* First release.
