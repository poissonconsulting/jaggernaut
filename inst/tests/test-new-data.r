context("new_data")

test_that("generates data frame with correct number of rows", {
  data <- data.frame(
    dlogical = as.logical(0:9),
    dinteger = 1:10,
    dnumeric = 1:10 + 0.1,
    dfactor = factor(1:10),
    ddate = as.Date("2000-01-01") + 1:10
  )
  expect_that(new_data(data), is_a("data.frame"))
  expect_that(nrow(new_data(data)), equals(1))
  expect_that(nrow(new_data(data,sequence="dnumeric",length_out=30)), equals(30))  
  expect_that(nrow(new_data(data,sequence=c("dnumeric","dinteger"),length_out=30)), equals(300))  
  expect_that(nrow(new_data(data,sequence=c("dfactor","dinteger"),length_out=5)), equals(25))  
})

test_that("does not generate data list", {
  data <- list(
    dlogical = as.logical(0:9),
    dinteger = 1:10,
    dnumeric = 1:10 + 0.1,
    dfactor = factor(1:10),
    ddate = as.Date("2000-01-01") + 1:10
  )
  expect_that(new_data(data), throws_error())
})
