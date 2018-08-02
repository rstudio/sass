context('mixins')

test_that('mixins work', {
  expected_first_word <- '.box'
  css <- compile("mixin.scss")
  expect_equal(
    strsplit(css, ' ')[[1]][1],
    expected_first_word
  )
})
