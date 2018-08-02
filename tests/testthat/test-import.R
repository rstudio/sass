context('import')

test_that('import works', {
  imported_type <- 'html'
  css <- compile('base.scss')
  expect_equal(
    strsplit(css, ',')[[1]][1],
    imported_type
  )
})
