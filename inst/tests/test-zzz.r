context("aaa")

test_that("start mode is test", {
  expect_that(opts_jagr("mode"),is_identical_to("test"))
})
