context("convert-data")

test_that("convert_data returns correct number of rows", {
  data <- data.frame(
    dlogical = as.logical(0:9),
    dnumeric = 1:10 + 0.1,
    dinteger = 1:10,
    dfactor = factor(1:10),
    ddate = as.Date("2000-01-01") + 1:10,
    dposixt = as.POSIXct("2000-01-01", tz= "GMT") + 1:10
  )
  expect_that(convert_data(data), is_a('data.frame'))
  expect_that(nrow(convert_data(data)), equals(10))
})