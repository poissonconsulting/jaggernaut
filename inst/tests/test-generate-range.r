context("generate_range")

test_that("generate_range returns vector of correct class", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(generate_range(dnumeric), is_a('numeric'))
  expect_that(generate_range(dinteger), is_a('integer'))
  expect_that(generate_range(dfactor), is_a('factor'))
  expect_that(generate_range(ddate), is_a('Date'))
  expect_that(generate_range(dposixt), is_a('POSIXct'))
})

test_that("generate_range returns vector of specified length", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(length(generate_range(dnumeric,24)), equals(24))
  expect_that(length(generate_range(dinteger,5)), equals(5))
  expect_that(length(generate_range(dinteger,20)), equals(10))
  expect_that(length(generate_range(dfactor)), equals(10))
  expect_that(length(generate_range(ddate,5)), equals(5))
  expect_that(length(generate_range(dposixt,11)), equals(10))
})

test_that("generate_range returns correct range", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(range(generate_range(dnumeric)), equals(c(1.1,10.1)))
  expect_that(range(generate_range(dinteger)), equals(c(1,10)))
  expect_that(generate_range(dfactor), equals(factor(1:10)))
  expect_that(range(generate_range(ddate)), equals(as.Date(c("2000-01-02","2000-01-11"))))
  expect_that(range(generate_range(dposixt)), equals(as.POSIXct("2000-01-01", tz= "GMT") + c(1,10)))
})