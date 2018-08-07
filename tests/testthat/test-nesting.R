context('nesting')

test_that('nesting works', {
  expected <- "nav ul {
  margin: 0;
  padding: 0;
  list-style: none;
}

nav li {
  display: inline-block;
}

nav a {
  display: block;
  padding: 6px 12px;
  text-decoration: none;
}
"
  class(expected) <- c("css", class(expected))

  css <- compile('nesting.scss')

  expect_equal(css, expected)
})
