context("cache")

with_temp_cache <- function(expr) {
  orig_cache <- sass_get_default_cache()
  temp_cache <- DiskCache$new(tempfile())
  on.exit({
    sass_set_default_cache(orig_cache)
    temp_cache$destroy()
  })
  sass_set_default_cache(temp_cache)

  force(expr)
}


test_that("throws on invalid output dir", {
  with_temp_cache({
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
})

test_that("reads from and writes to cache", {
  with_temp_cache({
    expected <- read_utf8("test-unicode-var-expected.css")

    css <- sass(sass_file("test-unicode-var-input.scss"))
    expect_equal(as.character(css), expected)

    css <- sass(sass_file("test-unicode-var-input.scss"))
    expect_equal(as.character(css), expected)

    expect_equal(sass_get_default_cache()$size(), 1)
  })
})

test_that("writes to cache", {
  with_temp_cache({
    cache <- sass_get_default_cache()

    input <- list(
      list(text_color = "#313131"),
      list("body { color: $text_color; }")
    )

    options <- sass_options(output_style = "compact")

    res <- sass(input, options)

    # Did we get the right result?
    expect_equal(as.character(res), "body { color: #313131; }\n")

    # Did a cache entry get added?
    expect_equal(cache$size(), 1)

    # Again, now with an output file
    out_file <- tempfile(fileext = ".css")
    sass(input, options, out_file)
    expect_equal(read_utf8(out_file), c("body { color: #313131; }\n"))

    # Now manipulate the cache directly to make sure it is actually being read
    # from. We'll change the value in the cache and see if sass() reads from it.
    cache$set_content(cache$keys(), "foo")
    res <- sass(input, options)
    expect_equal(as.character(res), "foo")
  })
})

test_that("unicode characters work OK after caching", {
  with_temp_cache({
    expected <- read_utf8("test-unicode-var-expected.css")
    class(expected) <- c("css", "html", "character")
    attr(expected, "html") <- TRUE

    css <- sass(sass_file("test-unicode-var-input.scss"))
    expect_equal(css, expected)

    css <- sass(sass_file("test-unicode-var-input.scss"))
    expect_equal(css, expected)

    expect_equal(sass_get_default_cache()$size(), 1)
  })
})

test_that("cache isn't written if a compilation error occurs", {
  with_temp_cache({
    input <- list(
      list(text_color = "#a0a0a0"),
      list("body { color is: $text_color; }")
    )
    options <- sass_options(output_style = "compact")

    expect_error(sass(input, options), "must be followed")

    expect_equal(sass_get_default_cache()$size(), 0)
  })
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
