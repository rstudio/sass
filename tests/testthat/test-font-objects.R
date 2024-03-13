library(htmltools)

expect_remote_font <- function(x) {
  input <- list(
    list("font-variable" = x),
    list("body {font-family: $font-variable}")
  )
  tagz <- renderTags(tags$style(sass(input)))
  # Produces the correct CSS rules
  expect_snapshot(tagz$html, cran = TRUE)
  # Produces the right <link> tag via attached dependency
  expect_snapshot(tagz$dependencies[[1]]$head, cran = TRUE)
}

test_that("Remote font importing basically works", {
  # font_link()
  expect_remote_font(font_link("foo", "bar"))
  expect_remote_font(font_link("foo", "bar"))
  # font_face()
  expect_remote_font(font_face("foo", "bar"))
  expect_remote_font(
    font_face(
      "foo", src = c("bar", "baz"),
      weight = c(400, 600), display = "auto",
      style = c("oblique", "30deg", "50deg"),
      stretch = c("75%", "125%"),
      unicode_range = c("U+0025-00FF", "U+4??")
    )
  )
  # font_google()
  expect_remote_font(font_google("Pacifico", local = FALSE))
  expect_remote_font(font_google("Pacifico", local = FALSE, display = "auto"))
  expect_remote_font(
    font_google("Crimson Pro", local = FALSE, wght = "200..900")
  )
  expect_remote_font(
    font_google("Crimson Pro", local = FALSE, wght = "200..900", ital = 1)
  )
  expect_remote_font(
    font_google("Crimson Pro", local = FALSE, wght = c(400, 500), ital = 1)
  )
  expect_remote_font(
    font_google("Crimson Pro", local = FALSE, wght = c(400, 500), ital = c(0, 1))
  )
  expect_remote_font(
    font_google("Crimson Pro", local = FALSE, wght = c(600, 400, 500), ital = c(1, 0))
  )
})

test_that("font_google(local = TRUE) basically works", {
  skip_if_offline()

  # Setup a new file cache and pass it to font_google()
  tmpdir <- tempfile("sass-cache-test-")
  dir.create(tmpdir)
  cache <- sass_file_cache(tmpdir)
  expect_equal(cache$size(), 0)

  scss <- list(
    list("my-font" = font_google("Pacifico", cache = cache)),
    list("body {font-family: $my-font}")
  )

  # 1st time rendering font should add files to cache
  tagz <- renderTags(tags$style(sass(scss)))
  size <- cache$size()
  expect_true(size > 0)

  # 2md time should result in cache hit
  tagz <- renderTags(tags$style(sass(scss)))
  expect_true(size == cache$size())

  # Make sure the markup structure and files are as expected
  expect_snapshot(tagz$html, cran = TRUE)
  src <- tagz$dependencies[[1]]$src$file
  expect_snapshot_file(
    dir(src, pattern = "\\.css$", full.names = TRUE),
    name = "font-css",
    # Don't run on CRAN since the src is a hash that might get updated
    cran = FALSE,
    compare = compare_file_text
  )
  woff <- dir(src, pattern = "\\.woff2$", full.names = TRUE)
  expect_true(length(woff) > 0)


  # https://github.com/rstudio/bslib/issues/408
  scss <- list(
    list("my-font" = font_google("Crimson Pro")),
    list("body {font-family: $my-font}")
  )
  tagz <- renderTags(tags$style(sass(scss)))
  expect_equal(tagz$dependencies[[1]]$name, "Crimson_Pro")
})

expect_collection <- function(..., expected) {
  scss <- as_sass(list(font = font_collection(...)))
  expect_equal(
    scss, paste0("$font: ", expected, " !default;"),
    ignore_attr = TRUE
  )
}

test_that("font_collection() basically works", {
  expect_collection("foo", expected = "foo")
  expect_collection(
    "foo", "bar",
    expected = "foo, bar"
  )
  expect_collection(
    "foo", "foo bar",
    expected = "foo, 'foo bar'"
  )
  expect_collection(
    "foo", "foo bar, baz",
    expected = "foo, 'foo bar', baz"
  )
  expect_collection(
    "foo", c("foo bar", "baz"),
    expected = "foo, 'foo bar', baz"
  )
  # Don't attempt to quote if , is quoted
  expect_warning(
    expect_collection(
      "foo", "'foo, bar', baz",
      expected = "foo, 'foo, bar', baz"
    ),
    "quote"
  )
  # Can suppress the warning with quote = FALSE
  expect_warning(
    expect_collection(
      "'foo, bar', baz", quote = FALSE,
      expected = "'foo, bar', baz"
    ),
    NA
  )
  expect_collection(
    font_google("foo bar baz"), "foo",
    expected = "'foo bar baz', foo"
  )
  expect_collection(
    font_link("foo bar baz", "link"), "foo",
    expected = "'foo bar baz', foo"
  )
  expect_collection(
    font_face("foo bar baz", "..."), "foo",
    expected = "'foo bar baz', foo"
  )
})

test_that("Special named args in font_collection", {
  expect_remote_font(
    font_collection(google = list("foo", local = FALSE))
  )
  expect_remote_font(
    font_collection(link = list("foo", href = "bar"))
  )
  expect_remote_font(
    font_collection(collection = list(google = list("foo", local = FALSE)))
  )
  expect_error(
    font_collection(foo = "bar"),
    "foo"
  )
  expect_error(font_collection(NA))
  expect_error(font_collection(""))
})
