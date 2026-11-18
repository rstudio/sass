test_that("the default cache directory is created when needed", {
  cache_dir <- file.path(withr::local_tempdir(), "sass")
  local_mocked_bindings(find_cache_dir = function(pkg) cache_dir)

  expect_identical(sass_cache_context_dir(), normalizePath(cache_dir))
  expect_identical(dir.exists(cache_dir), TRUE)
})

test_that("failed cache directory lookup falls back to a temporary cache", {
  local_mocked_bindings(
    find_cache_dir = function(pkg) stop("Cannot determine user cache directory")
  )

  expect_snapshot(cache_dir <- sass_cache_context_dir())
  withr::local_file(cache_dir)
  expect_identical(dir.exists(cache_dir), TRUE)
  # dirname() uses "/" while normalizePath() uses "\" on Windows
  expect_identical(normalizePath(dirname(cache_dir)), normalizePath(tempdir()))
  expect_match(basename(cache_dir), "^sass-")

  withr::local_options(sass.cache = cache_dir)
  withr::defer(sass_cache_set_dir(cache_dir, NULL))
  expect_css("body { color: red; }", "body { color: red; }")
  expect_equal(sass_cache_get()$size(), 1)
})

test_that("failed cache directory creation falls back to a temporary cache", {
  cache_file <- withr::local_tempfile(lines = "")
  local_mocked_bindings(find_cache_dir = function(pkg) cache_file)

  expect_snapshot(
    cache_dir <- sass_cache_context_dir(),
    # On Windows, the dir.create() warning renders the path in 8.3 short-name
    # form with backslashes, so match any quoted path instead of the literal
    transform = function(x) gsub("'[^']*'", "'<cache-file>'", x)
  )
  withr::local_file(cache_dir)
  expect_identical(dir.exists(cache_dir), TRUE)
  expect_identical(normalizePath(dirname(cache_dir)), normalizePath(tempdir()))
  expect_identical(file.exists(cache_file), TRUE)
})
