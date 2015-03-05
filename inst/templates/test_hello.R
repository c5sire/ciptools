context("hello")


test_that("hello function operates well", {
  # The following function should be used to reduce time on CRAN by avoiding running a test since time on CRAN is limited.
  skip_on_cran()
  
  expect_that(hello() == "Hello, world!", is_true())
  expect_that(hello(name = "Reinhard") == "Hello, Reinhard!", is_true())
  
  expect_that(hello(1), throws_error())
  expect_that(hello(TRUE), throws_error())
  expect_that(hello(NULL), throws_error())
}) 
