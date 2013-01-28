context("convert_variable")

test_that("convert_variable returns vector of correct class", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(convert_variable(dnumeric, 1:10), is_a('numeric'))
  expect_that(convert_variable(dinteger, 1:10), is_a('integer'))
  expect_that(convert_variable(dfactor, factor(1:10)), is_a('integer'))
  expect_that(convert_variable(ddate, as.Date("2000-01-01") + 1:10), is_a('integer'))
  expect_that(convert_variable(dposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10), is_a('integer'))
  
  expect_that(convert_variable(dnumeric, 1:10, centre = T), is_a('numeric'))
  expect_that(convert_variable(dinteger, 1:10, centre = T), is_a('integer'))
  expect_that(convert_variable(dfactor, factor(1:10), centre = T), is_a('integer'))
  expect_that(convert_variable(ddate, as.Date("2000-01-01") + 1:10, centre = T), is_a('integer'))
  expect_that(convert_variable(dposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10, centre = T), is_a('integer'))
  
  expect_that(convert_variable(dnumeric, 1:10, standardise = T), is_a('numeric'))
  expect_that(convert_variable(dinteger, 1:10, standardise = T), is_a('numeric'))
  expect_that(convert_variable(dfactor, factor(1:10), standardise = T), is_a('integer'))
  expect_that(convert_variable(ddate, as.Date("2000-01-01") + 1:10, standardise = T), is_a('numeric'))
  expect_that(convert_variable(dposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10, standardise = T), is_a('numeric'))
})

test_that("convert_variable returns vector of correct length", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(length(convert_variable(dnumeric, 1:10)), equals(10))
  expect_that(length(convert_variable(dinteger, 1:10)), equals(10))
  expect_that(length(convert_variable(dfactor, factor(1:10))), equals(10))
  expect_that(length(convert_variable(ddate, as.Date("2000-01-01") + 1:10)), equals(10))
  expect_that(length(convert_variable(dposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10)), equals(10))
})

test_that("convert_variable returns vector of correct values", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(convert_variable(dnumeric, 1), equals(1))
  expect_that(convert_variable(dinteger, 1), equals(1))
  expect_that(convert_variable(dfactor, factor(1)), equals(1))
  expect_that(convert_variable(ddate, as.Date("2000-01-01")), equals(1))
  expect_that(convert_variable(dposixt, as.POSIXct("2000-01-01", tz= "GMT")), equals(1))
})
