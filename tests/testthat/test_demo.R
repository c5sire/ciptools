context("demo")

test_that("demo just returns a string", {
  expect_that(demo(), prints_text("Hello!"))
})