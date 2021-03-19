# Disable sass cache
local_disable_cache()

test_that("writing to file works", {
  file_name <- tempfile(fileext = ".css")
  sass(
    sass_file("test-nesting-input.scss"),
    output = file_name
  )
  on.exit(unlink(file_name))
  output <- paste0(readLines(file_name), collapse = "\n")
  expected <- paste0(readLines("test-nesting-expected.css"), collapse = "\n")

  expect_true(file.exists(file_name))
  expect_equal(output, expected)
})

test_that("writing to invalid path fails", {
  expect_error(
    sass(
      sass_file("test-nesting-input.scss"),
      output = "not/path/output.txt"
    )
  )
})
