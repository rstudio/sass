# sass 0.2.0.9000

This release improves the caching of CSS compilation in `sass()`. Previously, caching was (by default) enabled in non-`interactive()` sessions and was allowed to grow indefinitely within `tempdir()` (i.e., within an R session). Now, caching is enabled by default in both interactive and non-interactive R sessions. In most cases, the cache will be stored in a user-level cache directory and persist across R sessions. In some cases (such as deployment on Shiny Server or Connect), the cache will be stored in a subdirectory of the application named `cache/`, to eliminate the risk of cache poisoning across applications. For more information about where the cache is stored, see `?sass_cache_get`.

Although caching is now enabled by default, it still may not be desirable while developing Sass code, because of the possibility of a false positive. (For more, see the Caching section of `?sass`) Caching can be disabled with `options(sass.cache = FALSE)`. Also, to help reduce the chance of a false positive in caching, `sass()` now includes a `cache_key_extra` parameter which may be used to pass information that the Sass `input` may not capture, such as file imports.

Other improvements include:

* A new `output_template()` function for more convenient `output` path creation that is `cache` and `options` aware.

* Add support to remove `rules` from `sass_layer()` objects using the new function `sass_layer_remove_rule()`. Added `sass_removable()` and `is_sass_removable()` to create removable Sass definitions objects. Sass layer `defaults` and `declaration` should not impact the compiled css size, therefore removal is not supported. (#54)

* When `sass()` has a cache hit, and `output` is specified, the cached file is now simply copied to `output` at the OS level (previously, `sass()` was reading the cache file into R, then writing it to `output`). (#42)

* Add `sass_layers()` function which will combine Sass layer and Sass layers objects. Combining with `sass_layers_remove()`, Sass layers can be removed by name. (#54)

## Breaking changes

* When `output` is specified, `sass()` now returns the output file path instead of the CSS content as a character vector.

* The `cache_options` argument in `sass()` has been renamed to `cache` and now defaults to `sass_cache_get()` instead of `sass_cache_options()`.

* `sass_cache_options()` has been deprecated (it no longer does anything) in favor of the new caching functions (`sass_file_cache()`).

* Rename `sass_layer_merge()` to `sass_layers()` to reflect the data structures being returned. (#54)

# sass 0.2.0

* Added new `sass_layer()` and `sass_layer_merge()` functions. See [here](https://rstudio.github.io/sass/articles/sass.html#layers) for more details.

* The objects that `sass()` and `as_sass()`) return now have better print methods in **rmarkdown**. See [here](https://rstudio.github.io/sass/articles/sass.html#rmarkdown) for more details.

* Added the ability for `sass()` to retain `htmltools::htmlDependency()`s attached to it's `input`.

* Fixed an issue with incorrect handling of length 2 or more character vector input (#37).

# sass 0.1.2

* No significant changes other than CRAN compliance.

# sass 0.1.1

* First release.
