context("is_convertible_data")

test_that("is_convertible_data_list and is_convertible_data return TRUE", {
  
  data <- list(x = 1)
  expect_that(is_convertible_data_list(data), equals(TRUE))
  expect_that(is_convertible_data(data), equals(TRUE))
  
  data <- list(x = 1, y = 1)
  expect_that(is_convertible_data_list(data), equals(TRUE))
  expect_that(is_convertible_data(data), equals(TRUE))
  
  data <- list(x = 1, y = 1, z = 1)
  expect_that(is_convertible_data_list(data), equals(TRUE))
  expect_that(is_convertible_data(data), equals(TRUE))
  
  data <- list(x = 1, y = matrix(1), z = array(1))
  expect_that(is_convertible_data_list(data), equals(TRUE))
  expect_that(is_convertible_data(data), equals(TRUE))
  
  data <- list(x = 1, y = matrix(1:2), z = array(1:2))
  expect_that(is_convertible_data_list(data), equals(TRUE))
  expect_that(is_convertible_data(data), equals(TRUE))
  
  data <- list(x1 = 1, x2 = 1.0, x3 = FALSE, x4 = factor(1), 
               x5 = as.Date("2000-01-01"), x6 = ISOdate(2000,1,1))
  expect_that(is_convertible_data_list(data), equals(TRUE))
  expect_that(is_convertible_data(data), equals(TRUE))
})

test_that("is_convertible_data and is_convertible_data_list returns FALSE", {

  data <- 1
  expect_that(is_convertible_data_list(data), equals(FALSE))
  expect_that(is_convertible_data(data), equals(FALSE))
  
  data <- "1"
  expect_that(is_convertible_data_list(data), equals(FALSE))
  expect_that(is_convertible_data(data), equals(FALSE))
  
  data <- data.frame(x = 1)
  expect_that(is_convertible_data_list(data), equals(FALSE))
  expect_that(is_convertible_data(data), equals(TRUE))
  
  data <- list(x = "1")
  expect_that(is_convertible_data_list(data), equals(FALSE))
  expect_that(is_convertible_data(data), equals(FALSE))
  
  data <- list(x = 1, y = list(1))
  expect_that(is_convertible_data_list(data), equals(FALSE))
  expect_that(is_convertible_data(data), equals(FALSE))
})
