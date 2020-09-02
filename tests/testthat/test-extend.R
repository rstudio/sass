context("extend")

# Disable sass cache
local_disable_cache()

test_that("extend works", {
  expected_first_line <- ".message, .success, .error, .warning "
  css <- sass(sass_file("test-extend.scss"))
  expect_equal(
    strsplit(css, "\\{")[[1]][1],
    expected_first_line
  )
})
