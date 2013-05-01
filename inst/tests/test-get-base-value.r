context("get_base_value")

test_that("get_base_value returns vector of correct class", {
  vlogical <- dvariable(as.logical(0:9))
  vinteger <- dvariable(1:10)
  vnumeric <- dvariable(1:10 + 0.1)
  vfactor <- dvariable(factor(1:10))
  vdate <- dvariable(as.Date("2000-01-01") + 1:10)
  vposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(get_base_value(vlogical), is_a('logical'))
  expect_that(get_base_value(vinteger), is_a('integer'))
  expect_that(get_base_value(vnumeric), is_a('numeric'))
  expect_that(get_base_value(vfactor), is_a('factor'))
  expect_that(get_base_value(vdate), is_a('Date'))
  expect_that(get_base_value(vposixt), is_a('POSIXt'))
})

test_that("get_base_value returns vector of length 1", {
  vlogical <- dvariable(as.logical(0:9))
  vinteger <- dvariable(1:10)
  vnumeric <- dvariable(1:10 + 0.1)
  vfactor <- dvariable(factor(1:10))
  vdate <- dvariable(as.Date("2000-01-01") + 1:10)
  vposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(length(get_base_value(vlogical)), equals(1))
  expect_that(length(get_base_value(vinteger)), equals(1))
  expect_that(length(get_base_value(vnumeric)), equals(1))
  expect_that(length(get_base_value(vfactor)), equals(1))
  expect_that(length(get_base_value(vdate)), equals(1))
  expect_that(length(get_base_value(vposixt)), equals(1))
})

test_that("get_base_value returns correct value", {
  vlogical <- dvariable(as.logical(0:9))
  vinteger <- dvariable(1:10)
  vnumeric <- dvariable(1:10 + 0.1)
  vfactor <- dvariable(factor(1:10))
  vdate <- dvariable(as.Date("2000-01-01") + 1:10)
  vposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(get_base_value(vlogical), equals(FALSE))
  expect_that(get_base_value(vinteger), equals(as.integer(round(mean(1:10)))))
  expect_that(get_base_value(vnumeric), equals(mean(1:10 + 0.1)))
  expect_that(get_base_value(vfactor), equals(factor(1:10)[1]))
  expect_that(get_base_value(vdate), equals(mean(as.Date("2000-01-01") + 1:10)))
  expect_that(get_base_value(vposixt), equals(mean(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)))
})
