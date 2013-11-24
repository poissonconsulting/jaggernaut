context("jagr_data_list")

test_that("inputs", {
  expect_that(jagr_data_list(list()), throws_error())
  expect_that(jagr_data_list(list(x = NULL)), throws_error())
  expect_that(jagr_data_list(list(1)), throws_error())
  expect_that(jagr_data_list(list(x = "1")), throws_error())
  expect_that(jagr_data_list(list(x = 1, x = 2)), throws_error())
  expect_that(jagr_data_list(list(x = 1)), is_a("jagr_data_list"))
})
