context("mixins")

# Disable sass cache
options(sass.cache = FALSE)

test_that("mixins work", {
  expected_first_word <- ".box"
  css <- sass(sass_file("test-mixins.scss"))
  expect_equal(
    strsplit(css, " ")[[1]][1],
    expected_first_word
  )
})
