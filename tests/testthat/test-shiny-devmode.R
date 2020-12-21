
test_that("shiny devmode shuts off default caching", {

  expect_equal(
    identical(
      sass_cache_get(),
      NULL
    ),
    FALSE
  )


  withr::local_options(list(
    shiny.devmode = TRUE,
    shiny.devmode.verbose = FALSE
  ))
  withr::local_envvar(list(
    TESTTHAT = "false"
  ))

  expect_equal(
    identical(
      sass_cache_get(),
      NULL
    ),
    TRUE
  )

})
