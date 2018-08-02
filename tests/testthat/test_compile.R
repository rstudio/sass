context("compile")

test_that(".scss file compiles", {
  expect_equal(
    compile("foo.scss"),
    "foo {\n  margin: 36.6px; }\n\nbar {\n  margin: 63px; }\n"
  )
})

test_that("string input compiles", {
  expect_equal(
    compile("foo { margin: 122px * .3; }"),
    "foo {\n  margin: 36.6px; }\n"
  )
})
