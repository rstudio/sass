context("import")

# Disable sass cache
options(sass.cache = FALSE)

test_that("import works", {
  imported_type <- "html"
  css <- sass(sass_file("test-import.scss"))
  expect_equal(
    strsplit(css, ",")[[1]][1],
    imported_type
  )
})
