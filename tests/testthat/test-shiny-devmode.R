
test_that("shiny devmode shuts off default caching", {


  withr::local_options(list(
    shiny.devmode = TRUE,
    shiny.devmode.verbose = FALSE
  ))
  withr::local_envvar(list(
    TESTTHAT = "false"
  ))

  expect_equal(
    sass_cache_get(),
    NULL
  )

})
