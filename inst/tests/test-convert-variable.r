context("convert_variable")

test_that("convert_variable returns vector of correct class", {
  vlogical <- dvariable(as.logical(0:9))
  vinteger <- dvariable(1:10)
  vnumeric <- dvariable(1:10 + 0.1)
  vfactor <- dvariable(factor(1:10))
  vdate <- dvariable(as.Date("2000-01-01") + 1:10)
  vposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  mlogical <- dvariable(matrix(as.logical(0:9)))
  minteger <- dvariable(matrix(as.integer(1:10)))
  mdouble <- dvariable(matrix(1:10 + 0.1))

  alogical <- dvariable(array(as.logical(0:9)))
  ainteger <- dvariable(array(as.integer(1:10)))
  adouble <- dvariable(array(1:10 + 0.1))
  
  expect_that(convert_variable(vlogical, as.logical(0:9)), is_a('integer'))
  expect_that(convert_variable(vinteger, 1:10), is_a('integer'))
  expect_that(convert_variable(vnumeric, 1:10 + 0.1), is_a('numeric'))
  expect_that(convert_variable(vfactor, factor(1:10)), is_a('integer'))
  expect_that(convert_variable(vdate, as.Date("2000-01-01") + 1:10), is_a('integer'))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10), is_a('integer'))
  
  expect_that(convert_variable(mlogical, matrix(as.logical(0:9))), is_a('matrix'))
  expect_that(convert_variable(minteger, matrix(1:10)), is_a('matrix'))
  expect_that(convert_variable(mdouble, matrix(1:10 + 0.1)), is_a('matrix'))

  expect_that(storage.mode(convert_variable(mlogical, matrix(as.logical(0:9)))), equals('integer'))
  expect_that(storage.mode(convert_variable(minteger, matrix(1:10))), equals('integer'))
  expect_that(storage.mode(convert_variable(mdouble, matrix(1:10 + 0.1))), equals('double'))
  
  expect_that(convert_variable(alogical, array(as.logical(0:9))), is_a('array'))
  expect_that(convert_variable(ainteger, array(1:10)), is_a('array'))
  expect_that(convert_variable(adouble, array(1:10 + 0.1)), is_a('array'))
  
  expect_that(storage.mode(convert_variable(alogical, array(as.logical(0:9)))), equals('integer'))
  expect_that(storage.mode(convert_variable(ainteger, array(1:10))), equals('integer'))
  expect_that(storage.mode(convert_variable(adouble, array(1:10 + 0.1))), equals('double'))
  
  expect_that(convert_variable(vlogical, as.logical(0:9), centre = T), is_a('integer'))
  expect_that(convert_variable(vinteger, 1:10, centre = T), is_a('integer'))
  expect_that(convert_variable(vnumeric, 1:10 + 0.1, centre = T), is_a('numeric'))
  expect_that(convert_variable(vfactor, factor(1:10), centre = T), is_a('integer'))
  expect_that(convert_variable(vdate, as.Date("2000-01-01") + 1:10, centre = T), is_a('integer'))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10, centre = T), is_a('integer'))
  
  expect_that(convert_variable(mlogical, matrix(as.logical(0:9)), centre = T), is_a('matrix'))
  expect_that(convert_variable(minteger, matrix(1:10), centre = T), is_a('matrix'))
  expect_that(convert_variable(mdouble, matrix(1:10 + 0.1), centre = T), is_a('matrix'))
  
  expect_that(storage.mode(convert_variable(mlogical, matrix(as.logical(0:9)), centre = T)), equals('integer'))
  expect_that(storage.mode(convert_variable(minteger, matrix(1:10), centre = T)), equals('integer'))
  expect_that(storage.mode(convert_variable(mdouble, matrix(1:10 + 0.1), centre = T)), equals('double'))
  
  expect_that(convert_variable(alogical, array(as.logical(0:9)), centre = T), is_a('array'))
  expect_that(convert_variable(ainteger, array(1:10), centre = T), is_a('array'))
  expect_that(convert_variable(adouble, array(1:10 + 0.1), centre = T), is_a('array'))
  
  expect_that(storage.mode(convert_variable(alogical, array(as.logical(0:9)), centre = T)), equals('integer'))
  expect_that(storage.mode(convert_variable(ainteger, array(1:10), centre = T)), equals('integer'))
  expect_that(storage.mode(convert_variable(adouble, array(1:10 + 0.1), centre = T)), equals('double'))  
  
  expect_that(convert_variable(vlogical, as.logical(0:9), standardise = T), is_a('integer'))
  expect_that(convert_variable(vinteger, 1:10, standardise = T), is_a('numeric'))
  expect_that(convert_variable(vnumeric, 1:10 + 0.1, standardise = T), is_a('numeric'))
  expect_that(convert_variable(vfactor, factor(1:10), standardise = T), is_a('integer'))
  expect_that(convert_variable(vdate, as.Date("2000-01-01") + 1:10, standardise = T), is_a('numeric'))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10, standardise = T), is_a('numeric'))
  
  expect_that(convert_variable(mlogical, matrix(as.logical(0:9)), standardise = T), is_a('matrix'))
  expect_that(convert_variable(minteger, matrix(1:10), standardise = T), is_a('matrix'))
  expect_that(convert_variable(mdouble, matrix(1:10 + 0.1), standardise = T), is_a('matrix'))
  
  expect_that(storage.mode(convert_variable(mlogical, matrix(as.logical(0:9)), standardise = T)), equals('integer'))
  expect_that(storage.mode(convert_variable(minteger, matrix(1:10), standardise = T)), equals('double'))
  expect_that(storage.mode(convert_variable(mdouble, matrix(1:10 + 0.1), standardise = T)), equals('double'))
  
  expect_that(convert_variable(alogical, array(as.logical(0:9)), standardise = T), is_a('array'))
  expect_that(convert_variable(ainteger, array(1:10), standardise = T), is_a('array'))
  expect_that(convert_variable(adouble, array(1:10 + 0.1), standardise = T), is_a('array'))
  
  expect_that(storage.mode(convert_variable(alogical, array(as.logical(0:9)), standardise = T)), equals('integer'))
  expect_that(storage.mode(convert_variable(ainteger, array(1:10), standardise = T)), equals('double'))
  expect_that(storage.mode(convert_variable(adouble, array(1:10 + 0.1), standardise = T)), equals('double')) 
  
})

test_that("convert_variable returns vector of correct length", {
  vlogical <- dvariable(as.logical(0:9))
  vinteger <- dvariable(1:10)
  vnumeric <- dvariable(1:10 + 0.1)
  vfactor <- dvariable(factor(1:10))
  vdate <- dvariable(as.Date("2000-01-01") + 1:10)
  vposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(length(convert_variable(vlogical, as.logical(0:9))), equals(10))
  expect_that(length(convert_variable(vinteger, 1:10)), equals(10))
  expect_that(length(convert_variable(vnumeric, 1:10 + 0.1)), equals(10))
  expect_that(length(convert_variable(vfactor, factor(1:10))), equals(10))
  expect_that(length(convert_variable(vdate, as.Date("2000-01-01") + 1:10)), equals(10))
  expect_that(length(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "GMT") + 1:10)), equals(10))
})

test_that("convert_variable returns vector of correct values", {
  vlogical <- dvariable(as.logical(0:9))
  vinteger <- dvariable(1:10)
  vnumeric <- dvariable(1:10 + 0.1)
  vfactor <- dvariable(factor(1:10))
  vdate <- dvariable(as.Date("2000-01-01") + 1:10)
  vposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(convert_variable(vlogical, TRUE), equals(1))
  expect_that(convert_variable(vinteger, as.integer(1)), equals(1))
  expect_that(convert_variable(vnumeric, 1), equals(1))
  expect_that(convert_variable(vfactor, factor(1)), equals(1))
  expect_that(convert_variable(vdate, as.Date("2000-01-01")), equals(1))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "GMT")), equals(1))
})
