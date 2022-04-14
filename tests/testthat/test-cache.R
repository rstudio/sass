test_that("throws on invalid output dir", {
  local_temp_cache()

  # Prime the cache
  sass("div { border: 1px solid black; }")

  # Cache
  expect_error(
    sass(
      "div { border: 1px solid black; }",
      output = file.path("not", "a", "path.css")
    )
  )
})

test_that("reads from and writes to cache", {
  local_temp_cache()
  expected <- read_utf8("test-unicode-var-expected.css")

  # Wrap the sass_file up into a bundle to ensure it's attribute
  # doesn't get dropped when the bundle is collapsed (into a layer) and compiled
  input <- sass_bundle(sass_layer(rules = sass_file("test-unicode-var-input.scss")))

  expect_equal(as.character(sass(input)), expected)

  expect_equal(as.character(sass(input)), expected)

  expect_equal(sass_cache_get()$size(), 1)

  # Modifying the file busts the cache (even it if has the same contents)
  Sys.setFileTime("test-unicode-var-input.scss", Sys.time() + 5)
  expect_equal(as.character(sass(input)), expected)
  expect_equal(sass_cache_get()$size(), 2)
})

test_that("writes to cache", {

  expect_cached <- function(input, css) {
    local_temp_cache()
    cache <- sass_cache_get()
    # Allow input to be an expression, like layer(), that can generate
    # different input everytime it gets evaluated
    input_code <- substitute(input)
    # Did we get the right CSS result?
    expect_css(eval(input_code), css)
    # Did a cache entry get added?
    expect_equal(cache$size(), 1)
    # Compile again, now with an output file
    out_file <- tempfile(fileext = ".css")
    expect_css(eval(input_code), css, output = out_file)
    # If there was a cache hit, the size should be the same
    expect_equal(cache$size(), 1)
    # Now manipulate the cache directly to make sure it is actually being read
    # from. We'll change the value in the cache and see if sass() reads from it.
    cache$set_content(cache$keys(), "foo")
    expect_css(eval(input_code), "foo")
  }

  expect_cached(
    list(
      list(text_color = "#313131"),
      list("body { color: $text_color; }")
    ),
    "body{color:#313131;}"
  )

  # Make sure cache key doesn't change with new/temporary HTML dependencies
  my_layer <- function() {
    src <- tempfile()
    dir.create(src)
    sass_layer(
      "@function fib($x) {
         @if $x <= 1 {
           @return $x
         }
         @return fib($x - 2) + fib($x - 1);
       }
       body { width: fib(27);}",
      html_deps = htmltools::htmlDependency("foo", "1.0", src)
    )
  }

  expect_cached(
    my_layer(),
    "body{width:196418;}"
  )

})

test_that("unicode characters work OK after caching", {
  local_temp_cache()
  expected <- read_utf8("test-unicode-var-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-var-input.scss"))
  expect_equal(css, expected)

  css <- sass(sass_file("test-unicode-var-input.scss"))
  expect_equal(css, expected)

  expect_equal(sass_cache_get()$size(), 1)
})

test_that("cache isn't written if a compilation error occurs", {
  local_temp_cache()

  input <- list(
    list(text_color = "#a0a0a0"),
    list("body { color is: $text_color; }")
  )
  options <- sass_options(output_style = "compact")

  expect_error(sass(input, options), "must be followed")

  expect_equal(sass_cache_get()$size(), 0)
})

test_that("cache key components", {
  tmpfile <- tempfile(fileext = ".tmp")
  file.create(tmpfile)
  old_mtime <- file.mtime(tmpfile)

  input <- list(sass_file(tmpfile))
  options <- sass_options()

  key1 <- sass_hash(list(input, options))
  while (file.mtime(tmpfile) == old_mtime) {
    # Force timestamp to change
    Sys.sleep(0.1)
    file.remove(tmpfile)
    file.create(tmpfile)
  }

  # Files differing by mtime should have different keys
  key2 <- sass_hash(list(input, options))
  expect_true(key1 != key2)

  # Options that are identical should have same key
  options3 <- sass_options()
  key3 <- sass_hash(list(input, options3))
  expect_true(key2 == key3)

  # Options that are different should have different keys
  options4 <- sass_options(output_style = "compact")
  key4 <- sass_hash(list(input, options4))
  expect_true(key3 != key4)
})



test_that("output_template() is cache and options aware", {
  local_temp_cache()

  input <- list(
    list(color = "red"),
    "body{color:red}"
  )
  opts <- sass_options(output_style = "compressed")
  expect_red <- function(file) {
    expect_equal(
      "body{color:red}",
      gsub("(\\s+)|(\t)|(;)", "", paste(readLines(file), collapse = ""))
    )
  }

  # File is exactly the same with the same input+options
  output1 <- sass(input, output = output_template(), options = opts)
  output2 <- sass(input, output = output_template(), options = opts)
  expect_true(output1 == output2)
  expect_match(output1, ".min.css$")
  expect_red(output1)
  expect_red(output2)

  # If cache is different, should get a different output file
  output3 <- sass(input, output = output_template(), options = opts, cache_key_extra = "foo")
  expect_true(output1 != output3)
  expect_red(output3)

  # Not minimized by default (because output_style = "expanded")
  output4 <- sass(input, output = output_template())
  expect_false(grepl(".min.css$", output4))
  expect_red(output4)

  # If no caching, should get a different output files
  output5 <- sass(input, output = output_template(), cache = FALSE)
  output6 <- sass(input, output = output_template(), cache = FALSE)
  expect_true(dirname(output5) != dirname(output6))
  expect_red(output5)
  expect_red(output6)

  # File can be written in another dir and still be cache aware
  temp_dir <- withr::local_tempdir()
  output7 <- sass(input, output = output_template(path = temp_dir), options = opts)
  expect_true(basename(dirname(dirname(output7))) == basename(temp_dir))
  expect_true(basename(dirname(output7)) == basename(dirname(output1)))
})

test_that("Cache directory getting/setting", {
  cache_dir <- tempfile("sass-cache-test-")
  cache <- sass_file_cache(dir = cache_dir)

  # Can normalize path _after_ dir is created.
  cache_dir <- normalizePath(cache_dir)
  expect_identical(normalizePath(cache$dir()), cache_dir)

  # Setting the cache for a directory works
  sass_cache_set_dir(cache_dir, cache)
  expect_identical(sass_cache_get_dir(cache_dir), cache)

  # It checks that the specified dir and the cache's dir match
  expect_error(sass_cache_set_dir(file.path(cache_dir, "foo"), cache))

  # Check that the path is normalized. Create another cache object and set
  # it as the cache for this path. Then fetching the cache for the path should
  # return the new one.
  cache2 <- sass_file_cache(dir = cache_dir)
  expect_false(identical(cache, cache2))
  sass_cache_set_dir(
    file.path(cache_dir, "..", basename(cache_dir)),
    cache2
  )
  expect_identical(sass_cache_get_dir(cache_dir), cache2)

  # Can unset cache for a given directory
  sass_cache_set_dir(cache_dir, NULL)
  expect_false(exists(cache_dir, envir = .caches))

  # Calling sass_cache_get_dir() when it doesn't exist should error
  cache_dir <- tempfile("sass-cache-test-")
  expect_error(sass_cache_get_dir(cache_dir))

  # Calling sass_cache_get_dir() when the dir exists but no cache object has
  # been registered returns NULL
  dir.create(cache_dir)
  expect_null(sass_cache_get_dir(cache_dir))


  # Calling sass_cache_get_dir() when the dir doesn't exist (and cache object
  # isn't registered) with create=TRUE should create the dir and the cache
  # object.
  cache_dir <- tempfile("sass-cache-test-")
  cache_obj <- sass_cache_get_dir(cache_dir, create = TRUE)
  expect_true(dir.exists(cache_dir))
  expect_true(inherits(cache_obj, "FileCache"))

  # Calling sass_cache_get_dir() when the dir exists but cache object does not
  # with create=TRUE should create the cache object.
  cache_dir <- tempfile("sass-cache-test-")
  dir.create(cache_dir)
  cache_obj <- sass_cache_get_dir(cache_dir, create = TRUE)
  expect_true(dir.exists(cache_dir))
  expect_true(inherits(cache_obj, "FileCache"))
})


test_that("Can pass a cache directory to sass()", {
  cache_dir <- tempfile()
  sass("body {color: red; }", cache = cache_dir)

  # Cache directory should have been created
  cache_obj <- sass_cache_get_dir(cache_dir, create = FALSE)
  expect_true(inherits(cache_obj, "FileCache"))
})
