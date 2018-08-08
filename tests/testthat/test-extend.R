context('extend')

test_that('extend works', {
  expected_first_line <- '.message, .success, .error, .warning '
  css <- compile('test-extend.scss')
  expect_equal(
    strsplit(css, '\\{')[[1]][1],
    expected_first_line
  )
})
