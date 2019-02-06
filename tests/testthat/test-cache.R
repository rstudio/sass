context("cache")

use_cache = sass_cache_options(cache = TRUE)

with_caching <- function(expr, cache_dir = getOption("sass.cache_dir", tempdir())) {
  op <- options(sass.cache = TRUE, sass.cache_dir = cache_dir)
  on.exit(options(op), add = TRUE)

  force(expr)
}

test_that("throws on invalid cache dir", {

  expect_error(
    sass(
      "div { border: 1px solid black; }",
      cache_options = sass_cache_options(
        cache = TRUE,
        cache_dir = file.path("not", "a", "path")
      )
    )
  )
})

test_that("throws on invalid output dir", {
  with_caching({

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

test_that("reads from cache", {
  with_caching({
    cache_dir <- getOption("sass.cache_dir", stop("Assertion failed"))

    input <- list(
      list(text_color = "#444444"),
      list("body { color: $text_color; }")
    )

    options <- sass_options()

    cache_file <- cache_file_path(cache_dir, input, options)
    contents <- paste("/*", Sys.time(), "*/")
    writeLines(contents, cache_file)

    res <- sass(input, options)

    expect_equal(as.vector(res), contents)

    out_file <- tempfile(fileext = ".css")
    sass(input, options, out_file)
    expect_equal(readLines(cache_file), readLines(out_file))

    # Ensure that out_file is overwritten if it already exists
    writeLines("bogus", out_file)
    # Check that we can overwrite the out_file
    expect_error(sass(input, options, out_file), NA)
    expect_equal(readLines(cache_file), readLines(out_file))
  })
})

test_that("writes to cache", {
  with_caching({
    cache_dir <- getOption("sass.cache_dir", stop("Assertion failed"))

    input <- list(
      list(text_color = "#313131"),
      list("body { color: $text_color; }")
    )

    options <- sass_options(output_style = "compact")

    cache_file <- cache_file_path(cache_dir, input, options)
    if (file.exists(cache_file))
      file.remove(cache_file)

    res <- sass(input, options)

    # Did we get the right result?
    expect_equal(as.vector(res), "body { color: #313131; }\n")

    # Did the cache file get written?
    expect_equal(readLines(cache_file), c("body { color: #313131; }", ""))
  })
})

test_that("unicode characters work OK after caching", {
  with_caching({
    expected <- paste0(readLines("test-unicode-var-expected.css", encoding = "UTF-8"), collapse = "\n")
    class(expected) <- c("css", "html", "character")
    attr(expected, "html") <- TRUE

    css <- sass(sass_file("test-unicode-var-input.scss"))

    expect_equal(css, expected)

    css <- sass(sass_file("test-unicode-var-input.scss"))

    expect_equal(css, expected)

    # TODO: Wish I had a way of verifying that the cache was actually used
  })
})

test_that("cache isn't written if a compilation error occurs", {
  with_caching({
    cache_dir <- getOption("sass.cache_dir", stop("Assertion failed"))

    input <- list(
      list(text_color = "#a0a0a0"),
      list("body { color is: $text_color; }")
    )

    options <- sass_options(output_style = "compact")

    cache_file <- cache_file_path(cache_dir, input, options)
    if (file.exists(cache_file))
      file.remove(cache_file)

    expect_error(sass(input, options), "must be followed")

    expect_false(file.exists(cache_file))
  })
})

test_that("cache key components", {
  tmpfile <- tempfile(fileext = ".tmp")
  file.create(tmpfile)
  old_mtime <- file.mtime(tmpfile)

  input <- list(sass_file(tmpfile))
  options <- sass_options()

  key1 <- cache_file_path(tempdir(), input, options)
  while (file.mtime(tmpfile) == old_mtime) {
    # Force timestamp to change
    Sys.sleep(0.1)
    file.remove(tmpfile)
    file.create(tmpfile)
  }

  # Files differing by mtime should have different keys
  key2 <- cache_file_path(tempdir(), input, options)
  expect_true(key1 != key2)

  # Options that are identical should have same key
  options3 <- sass_options()
  key3 <- cache_file_path(tempdir(), input, options3)
  expect_true(key2 == key3)

  # Options that are different should have different keys
  options4 <- sass_options(output_style = "compact")
  key4 <- cache_file_path(tempdir(), input, options4)
  expect_true(key3 != key4)
})
