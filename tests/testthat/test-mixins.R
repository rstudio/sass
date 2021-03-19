# Disable sass cache
local_disable_cache()

test_that("mixins work", {
  expected_first_word <- ".box"
  css <- sass(sass_file("test-mixins.scss"))
  expect_equal(
    strsplit(css, " ")[[1]][1],
    expected_first_word
  )
})
