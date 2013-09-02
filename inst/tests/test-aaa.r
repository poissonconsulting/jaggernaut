context("aaa")

opts_jagr(mode = "test")

test_that("start mode is test", {
  expect_that(opts_jagr("mode"),is_identical_to("test"))
})
