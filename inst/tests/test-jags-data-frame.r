context("jagr_data_frame")

test_that("inputs", {
  expect_that(jagr_data_frame(data.frame()), throws_error())
  expect_that(jagr_data_frame(data.frame(x = NULL)), throws_error())
  
  x <- data.frame(x = "1")
  x$x <- as.character(x$x)
  expect_that(jagr_data_frame(x), throws_error())
  
  x <- data.frame(x = 1, x = 2)
  colnames(x) <- c("x","x")
  expect_that(jagr_data_frame(x), throws_error())

  expect_that(jagr_data_frame(as.list(data.frame(x = 1))), throws_error())
  
  expect_that(jagr_data_frame(data.frame(x = 1)), is_a("jagr_data_frame"))
})
