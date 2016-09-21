context("convert_variable")

test_that("convert_variable returns vector of correct class", {
  vlogical <- variable(as.logical(0:9))
  vinteger <- variable(1:10)
  vnumeric <- variable(1:10 + 0.1)
  vfactor <- variable(factor(1:10))
  vdate <- variable(as.Date("2000-01-01") + 1:10)
  vposixt <- variable(as.POSIXct("2000-01-01", tz= "UTC") + 1:10)
  
  mlogical <- variable(matrix(as.logical(0:9)))
  minteger <- variable(matrix(as.integer(1:10)))
  mdouble <- variable(matrix(1:10 + 0.1))

  alogical <- variable(array(as.logical(0:9)))
  ainteger <- variable(array(as.integer(1:10)))
  adouble <- variable(array(1:10 + 0.1))
  
  expect_that(convert_variable(vlogical, as.logical(0:9)), is_a("integer"))
  expect_that(convert_variable(vinteger, 1:10), is_a("integer"))
  expect_that(convert_variable(vnumeric, 1:10 + 0.1), is_a("numeric"))
  expect_that(convert_variable(vfactor, factor(1:10)), is_a("integer"))
  expect_that(convert_variable(vdate, as.Date("2000-01-01") + 1:10), is_a("integer"))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10), is_a("integer"))
  
  expect_that(convert_variable(mlogical, matrix(as.logical(0:9))), is_a("matrix"))
  expect_that(convert_variable(minteger, matrix(1:10)), is_a("matrix"))
  expect_that(convert_variable(mdouble, matrix(1:10 + 0.1)), is_a("matrix"))

  expect_that(storage.mode(convert_variable(mlogical, matrix(as.logical(0:9)))), equals("integer"))
  expect_that(storage.mode(convert_variable(minteger, matrix(1:10))), equals("integer"))
  expect_that(storage.mode(convert_variable(mdouble, matrix(1:10 + 0.1))), equals("double"))
  
  expect_that(convert_variable(alogical, array(as.logical(0:9))), is_a("array"))
  expect_that(convert_variable(ainteger, array(1:10)), is_a("array"))
  expect_that(convert_variable(adouble, array(1:10 + 0.1)), is_a("array"))
  
  expect_that(storage.mode(convert_variable(alogical, array(as.logical(0:9)))), equals("integer"))
  expect_that(storage.mode(convert_variable(ainteger, array(1:10))), equals("integer"))
  expect_that(storage.mode(convert_variable(adouble, array(1:10 + 0.1))), equals("double"))
  
  expect_that(convert_variable(vlogical, as.logical(0:9), centre = T), is_a("integer"))
  expect_that(convert_variable(vinteger, 1:10, centre = T), is_a("integer"))
  expect_that(convert_variable(vnumeric, 1:10 + 0.1, centre = T), is_a("numeric"))
  expect_that(convert_variable(vfactor, factor(1:10), centre = T), is_a("integer"))
  expect_that(convert_variable(vdate, as.Date("2000-01-01") + 1:10, centre = T), is_a("integer"))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10, centre = T), is_a("integer"))
  
  expect_that(convert_variable(mlogical, matrix(as.logical(0:9)), centre = T), is_a("matrix"))
  expect_that(convert_variable(minteger, matrix(1:10), centre = T), is_a("matrix"))
  expect_that(convert_variable(mdouble, matrix(1:10 + 0.1), centre = T), is_a("matrix"))
  
  expect_that(storage.mode(convert_variable(mlogical, matrix(as.logical(0:9)), centre = T)), equals("integer"))
  expect_that(storage.mode(convert_variable(minteger, matrix(1:10), centre = T)), equals("integer"))
  expect_that(storage.mode(convert_variable(mdouble, matrix(1:10 + 0.1), centre = T)), equals("double"))
  
  expect_that(convert_variable(alogical, array(as.logical(0:9)), centre = T), is_a("array"))
  expect_that(convert_variable(ainteger, array(1:10), centre = T), is_a("array"))
  expect_that(convert_variable(adouble, array(1:10 + 0.1), centre = T), is_a("array"))
  
  expect_that(storage.mode(convert_variable(alogical, array(as.logical(0:9)), centre = T)), equals("integer"))
  expect_that(storage.mode(convert_variable(ainteger, array(1:10), centre = T)), equals("integer"))
  expect_that(storage.mode(convert_variable(adouble, array(1:10 + 0.1), centre = T)), equals("double"))  
  
  expect_that(convert_variable(vlogical, as.logical(0:9), standardise = T), is_a("integer"))
  expect_that(convert_variable(vinteger, 1:10, standardise = T), is_a("numeric"))
  expect_that(convert_variable(vnumeric, 1:10 + 0.1, standardise = T), is_a("numeric"))
  expect_that(convert_variable(vfactor, factor(1:10), standardise = T), is_a("integer"))
  expect_that(convert_variable(vdate, as.Date("2000-01-01") + 1:10, standardise = T), is_a("numeric"))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10, standardise = T), is_a("numeric"))
  
  expect_that(convert_variable(mlogical, matrix(as.logical(0:9)), standardise = T), is_a("matrix"))
  expect_that(convert_variable(minteger, matrix(1:10), standardise = T), is_a("matrix"))
  expect_that(convert_variable(mdouble, matrix(1:10 + 0.1), standardise = T), is_a("matrix"))
  
  expect_that(storage.mode(convert_variable(mlogical, matrix(as.logical(0:9)), standardise = T)), equals("integer"))
  expect_that(storage.mode(convert_variable(minteger, matrix(1:10), standardise = T)), equals("double"))
  expect_that(storage.mode(convert_variable(mdouble, matrix(1:10 + 0.1), standardise = T)), equals("double"))
  
  expect_that(convert_variable(alogical, array(as.logical(0:9)), standardise = T), is_a("array"))
  expect_that(convert_variable(ainteger, array(1:10), standardise = T), is_a("array"))
  expect_that(convert_variable(adouble, array(1:10 + 0.1), standardise = T), is_a("array"))
  
  expect_that(storage.mode(convert_variable(alogical, array(as.logical(0:9)), standardise = T)), equals("integer"))
  expect_that(storage.mode(convert_variable(ainteger, array(1:10), standardise = T)), equals("double"))
  expect_that(storage.mode(convert_variable(adouble, array(1:10 + 0.1), standardise = T)), equals("double")) 
  
})

test_that("convert_variable returns vector of correct length", {
  vlogical <- variable(as.logical(0:9))
  vinteger <- variable(1:10)
  vnumeric <- variable(1:10 + 0.1)
  vfactor <- variable(factor(1:10))
  vdate <- variable(as.Date("2000-01-01") + 1:10)
  vposixt <- variable(as.POSIXct("2000-01-01", tz= "UTC") + 1:10)
  
  expect_that(length(convert_variable(vlogical, as.logical(0:9))), equals(10))
  expect_that(length(convert_variable(vinteger, 1:10)), equals(10))
  expect_that(length(convert_variable(vnumeric, 1:10 + 0.1)), equals(10))
  expect_that(length(convert_variable(vfactor, factor(1:10))), equals(10))
  expect_that(length(convert_variable(vdate, as.Date("2000-01-01") + 1:10)), equals(10))
  expect_that(length(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10)), equals(10))
})

test_that("convert_variable returns vector of correct values", {
  vlogical <- variable(as.logical(0:9))
  vinteger <- variable(1:10)
  vnumeric <- variable(1:10 + 0.1)
  vfactor <- variable(factor(1:10))
  vdate <- variable(as.Date("2000-01-01") + 1:10)
  vposixt <- variable(as.POSIXct("2000-01-01", tz= "UTC") + 1:10)
  
  expect_that(convert_variable(vlogical, TRUE), equals(1))
  expect_that(convert_variable(vinteger, as.integer(1)), equals(1))
  expect_that(convert_variable(vnumeric, 1), equals(1))
  expect_that(convert_variable(vfactor, factor(1)), equals(1))
  expect_that(convert_variable(vdate, as.Date("2000-01-01")), equals(1))
  expect_that(convert_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC")), equals(1))
})
