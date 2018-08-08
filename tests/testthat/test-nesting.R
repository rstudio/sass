context('nesting')

test_that('nesting works', {
  expected <- paste0(readLines("test-nesting-expected.css"), collapse = '\n')
  class(expected) <- c("css", class(expected))

  css <- compile('test-nesting-input.scss')

  expect_equal(css, expected)
})
