context("get-conversions")

test_that("get_conversions", {
  
  x <- get_conversions(NULL)

  expect_that(x, is_a("list"))
  expect_identical(names(x), c("select", "centre", "standardise", "transform"))
  expect_identical(x$select, NULL)
  expect_identical(x$centre, NULL)
  expect_identical(x$standardise, NULL)
  expect_identical(x$transform, list())
  
  x <- get_conversions("x1")

  expect_that(x, is_a("list"))
  expect_identical(names(x), c("select", "centre", "standardise", "transform"))
  expect_identical(x$select, c("x1"))
  expect_identical(x$centre, character())
  expect_identical(x$standardise, character())
  expect_identical(x$transform, list())

  x <- get_conversions(c("x1", "x2+", "x3*", "log(x4)", "log(x5)*", "func(x6)+", "x7"))
  expect_that(x, is_a("list"))
  expect_identical(names(x), c("select", "centre", "standardise", "transform"))
  expect_identical(x$select, c("x1", "x2", "x3", "x4", "x5", "x6", "x7"))
  expect_identical(x$centre, c("x2", "x6"))
  expect_identical(x$standardise, c("x3", "x5"))
  expect_identical(x$transform, list(x4 = "log", x5 = "log", x6 = "func"))
})

