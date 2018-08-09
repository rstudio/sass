context('unicode')

test_that('unicode variables work', {
  expected <- paste0(readLines("test-unicode-var-expected.css"), collapse = '\n')
  class(expected) <- c('css', class(expected))

  css <- compile_sass('test-unicode-var-input.scss')

  expect_equal(css, expected)
})

test_that('unicode css works', {
  expected <- paste0(readLines("test-unicode-css-expected.css"), collapse = '\n')
  class(expected) <- c('css', class(expected))

  css <- compile_sass('test-unicode-css-input.scss')

  expect_equal(css, expected)
})

test_that('unicode bom', {
  expected <- paste0(readLines("test-unicode-bom-expected.css"), collapse = '\n')
  class(expected) <- c('css', class(expected))

  css <- compile_sass('test-unicode-bom-input.scss')

  expect_equal(css, expected)
})
