context('output')

test_that('writing to file works', {
  compile_sass(
    "test-nesting-input.scss",
    output = "output.txt"
  )
  output <- paste0(readLines("output.txt"), collapse = '\n')
  expected <- paste0(readLines("test-nesting-expected.css"), collapse = '\n')

  expect_true(file.exists("output.txt"))
  expect_equal(output, expected)

  unlink("output.txt")
})

test_that('writing to invalid path fails', {
  expect_warning(
    expect_error(
      compile_sass(
        "test-nesting-input.scss",
        output = "not/path/output.txt"
      )
    )
  )
})
