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
  # skip_if_offline() errors when curl isn't installed
  skip_if_not_installed("curl")
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
  expect_snapshot(
    tagz <- renderTags(tags$style(sass(scss))),
    transform = function(x) gsub("\\(.*\\)", "(<temp-cache>)", x)
  )
  size <- cache$size()
  expect_true(size > 0)

  # 2md time should result in cache hit
  expect_snapshot(tagz <- renderTags(tags$style(sass(scss))))
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

test_that("local Google font files get safe, deterministic names (#157)", {
  # Google Fonts now returns font URLs with query strings, whose basename
  # isn't a valid file name on Windows (e.g., it contains '?')
  css <- c(
    "@font-face {",
    "  font-family: 'Open Sans';",
    "  font-style: normal;",
    "  font-weight: 400;",
    "  font-display: swap;",
    "  src: url(https://fonts.gstatic.com/l/font?kit=memSYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWq8tWZ0Pw86hd0Rk8ZkC1w2MXmNxTZSmB0RWpjhI7Y&skey=62d9d2db33ab95cd&v=v44) format('woff2');",
    "  unicode-range: U+0000-00FF;",
    "}",
    "@font-face {",
    "  font-family: 'Open Sans';",
    "  font-style: normal;",
    "  font-weight: 700;",
    "  font-display: swap;",
    "  src: url(https://fonts.gstatic.com/l/font?kit=memQYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWq8tWZ0Pw86hd0Rk8ZkC1w2MXmNxTZSmB0RWpjhI7Y&skey=62d9d2db33ab95cd&v=v44) format('woff2');",
    "  unicode-range: U+0000-00FF;",
    "}"
  )
  files <- gfont_local_files(css)
  expect_equal(nrow(files), 2)
  # Names are unique and contain no characters that are
  # invalid in file names on Windows (e.g., ? & = : / etc)
  expect_equal(length(unique(files$name)), 2)
  expect_false(any(grepl("[?&=:/\\\\*\"<>|]", files$name)))
  # The file extension reflects the font format
  expect_true(all(grepl("\\.woff2$", files$name)))
  # Names are deterministic (important for the CSS rewrite + caching)
  expect_equal(files, gfont_local_files(css))
})

test_that("gfont_local_files() leaves already-local names unchanged", {
  # When font_dep_google_local() gets a cache hit on the CSS file, the CSS
  # already points at local file names -- those names must round-trip so
  # the font files can still be found in the cache
  css <- c(
    "  src: url(90f4a537786e4aba8d6c86f7abd13d0d.woff2) format('woff2');",
    "  src: url(90f4a537786e4aba8d6c86f7abd13d0d.woff2);"
  )
  files <- gfont_local_files(css)
  expect_equal(files$name, rep("90f4a537786e4aba8d6c86f7abd13d0d.woff2", 2))
})

test_that("local font names use URL extensions when format hints are absent", {
  urls <- c(
    "https://fonts.gstatic.com/font.woff2?kit=test#subset.woff",
    "https://fonts.gstatic.com/font.woff",
    "https://fonts.gstatic.com/font.ttf",
    "https://fonts.gstatic.com/font.otf",
    "https://fonts.gstatic.com/font.eot",
    "https://fonts.gstatic.com/font.svg",
    "https://fonts.gstatic.com/font?kit=test.woff2",
    "https://fonts.gstatic.com/font#subset.woff2",
    "https://fonts.gstatic.com/font.exe"
  )
  files <- gfont_local_files(paste0("src: url(", urls, ");"))
  expect_equal(files$url, urls)
  expect_equal(
    tools::file_ext(files$name),
    c("woff2", "woff", "ttf", "otf", "eot", "svg", "", "", "")
  )
  expect_false(any(grepl("[?&=:/\\\\*\"<>|]", files$name)))
})

test_that("explicit font format hints take precedence over URL extensions", {
  files <- gfont_local_files(
    "src: url(https://fonts.gstatic.com/font.ttf) format('woff2');"
  )
  expect_equal(tools::file_ext(files$name), "woff2")
})

test_that("font_google(local = TRUE) recovers when fonts are pruned from the cache", {
  skip_if_not_installed("curl")
  skip_if_offline()

  tmpdir <- tempfile("sass-cache-test-")
  dir.create(tmpdir)
  cache <- sass_file_cache(tmpdir)

  scss <- list(
    list("my-font" = font_google("Pacifico", cache = cache)),
    list("body {font-family: $my-font}")
  )
  render_src <- function() {
    renderTags(tags$style(sass(scss)))$dependencies[[1]]$src$file
  }
  # Check that every font file referenced by the CSS actually exists
  has_font_files <- function(src) {
    css_file <- dir(src, pattern = "\\.css$", full.names = TRUE)
    refs <- sub(
      ".*url\\(([^)]+)\\).*", "\\1",
      grep("src:", readLines(css_file), value = TRUE)
    )
    length(refs) > 0 && all(file.exists(file.path(src, refs)))
  }

  src <- render_src()
  expect_true(has_font_files(src))

  # Simulate the cache pruning the font files, but not the CSS file
  cached_fonts <- function() {
    cached <- dir(cache$dir(), full.names = TRUE)
    is_font <- vapply(
      cached,
      function(f) identical(readBin(f, "raw", 4), charToRaw("wOF2")),
      logical(1)
    )
    cached[is_font]
  }
  expect_true(length(cached_fonts()) > 1)

  # Partial pruning (the common case, since caches evict files one at a
  # time): fonts still in the cache must also have their URLs rewritten
  # to the local copies
  file.remove(cached_fonts()[1])
  src <- render_src()
  expect_true(has_font_files(src))

  # Full pruning: every font file is restored
  file.remove(cached_fonts())
  src <- render_src()
  expect_true(has_font_files(src))
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
    collection <- font_collection("foo", "'foo, bar', baz"),
    "quote"
  )
  expect_equal(
    as_sass(list(font = collection)),
    "$font: foo, 'foo, bar', baz !default;",
    ignore_attr = TRUE
  )
  # Can suppress the warning with quote = FALSE
  expect_warning(
    collection <- font_collection("'foo, bar', baz", quote = FALSE),
    NA
  )
  expect_equal(
    as_sass(list(font = collection)),
    "$font: 'foo, bar', baz !default;",
    ignore_attr = TRUE
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

test_that("font_google() errors informatively on an unknown font family", {
  # skip_if_offline() errors when curl isn't installed
  skip_if_not_installed("curl")
  skip_if_offline()

  # Use a fresh cache so the CSS download actually happens
  tmpdir <- tempfile("sass-cache-test-")
  dir.create(tmpdir)
  cache <- sass_file_cache(tmpdir)

  scss <- list(
    list("my-font" = font_google("Source Code", cache = cache)),
    list("body {font-family: $my-font}")
  )

  # The error should name the (misspelled) family and suggest checking the name
  expect_error(
    renderTags(tags$style(sass(scss))),
    "Source Code"
  )
  expect_error(
    renderTags(tags$style(sass(scss))),
    "fonts.google.com"
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
