context("get_mean")

test_that("get_mean returns vector of correct class", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(get_mean(dnumeric), is_a('numeric'))
  expect_that(get_mean(dinteger), is_a('integer'))
  expect_that(get_mean(dfactor), is_a('factor'))
  expect_that(get_mean(ddate), is_a('Date'))
  expect_that(get_mean(dposixt), is_a('POSIXt'))
})

test_that("get_mean returns vector of length 1", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(length(get_mean(dnumeric)), equals(1))
  expect_that(length(get_mean(dinteger)), equals(1))
  expect_that(length(get_mean(dfactor)), equals(1))
  expect_that(length(get_mean(ddate)), equals(1))
  expect_that(length(get_mean(dposixt)), equals(1))
})

test_that("get_mean returns correct value", {
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(get_mean(dnumeric), equals(mean(1:10 + 0.1)))
  expect_that(get_mean(dinteger), equals(as.integer(round(mean(1:10)))))
  expect_that(get_mean(dfactor), equals(factor(1:10)[1]))
  expect_that(get_mean(ddate), equals(mean(as.Date("2000-01-01") + 1:10)))
  expect_that(get_mean(dposixt), equals(mean(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)))
})