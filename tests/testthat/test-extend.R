# Disable sass cache
local_disable_cache()

test_that("extend works", {
  # allow for css rules to be reordered within a definition
  split_sort <- function(x) {
    sort(
      sub("\\s+$", "", # remove tailing
        sub("^\\s+", "", # remove leading
          strsplit(x, ",")[[1]]
        )
      )
    )
  }
  expected_first_line <- ".message, .success, .error, .warning "
  css <- sass(sass_file("test-extend.scss"))
  expect_equal(
    split_sort(strsplit(css, "\\{")[[1]][1]),
    split_sort(expected_first_line)
  )
})
