context('import')

test_that('import works', {
  imported_type <- 'html'
  css <- compile_sass('test-import.scss')
  expect_equal(
    strsplit(css, ',')[[1]][1],
    imported_type
  )
})
