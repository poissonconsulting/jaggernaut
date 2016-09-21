context("conversion_terms_variable")

test_that("conversion_terms_variable returns vector of correct class", {
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
  
  expect_that(conversion_terms_variable(vlogical, as.logical(0:9)), is_a("numeric"))
  expect_that(conversion_terms_variable(vinteger, 1:10), is_a("numeric"))
  expect_that(conversion_terms_variable(vnumeric, 1:10 + 0.1), is_a("numeric"))
  expect_that(conversion_terms_variable(vfactor, factor(1:10)), is_a("numeric"))
  expect_that(conversion_terms_variable(vdate, as.Date("2000-01-01") + 1:10), is_a("numeric"))
  expect_that(conversion_terms_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10), is_a("numeric"))
  
  expect_that(conversion_terms_variable(mlogical, matrix(as.logical(0:9))), is_a("numeric"))
  expect_that(conversion_terms_variable(minteger, matrix(1:10)), is_a("numeric"))
  expect_that(conversion_terms_variable(mdouble, matrix(1:10 + 0.1)), is_a("numeric"))

  expect_that(storage.mode(conversion_terms_variable(mlogical, matrix(as.logical(0:9)))), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(minteger, matrix(1:10))), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(mdouble, matrix(1:10 + 0.1))), equals("double"))
  
  expect_that(conversion_terms_variable(alogical, array(as.logical(0:9))), is_a("numeric"))
  expect_that(conversion_terms_variable(ainteger, array(1:10)), is_a("numeric"))
  expect_that(conversion_terms_variable(adouble, array(1:10 + 0.1)), is_a("numeric"))
  
  expect_that(storage.mode(conversion_terms_variable(alogical, array(as.logical(0:9)))), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(ainteger, array(1:10))), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(adouble, array(1:10 + 0.1))), equals("double"))
  
  expect_that(conversion_terms_variable(vlogical, as.logical(0:9), centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vinteger, 1:10, centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vnumeric, 1:10 + 0.1, centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vfactor, factor(1:10), centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vdate, as.Date("2000-01-01") + 1:10, centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10, centre = T), is_a("numeric"))
  
  expect_that(conversion_terms_variable(mlogical, matrix(as.logical(0:9)), centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(minteger, matrix(1:10), centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(mdouble, matrix(1:10 + 0.1), centre = T), is_a("numeric"))
  
  expect_that(storage.mode(conversion_terms_variable(mlogical, matrix(as.logical(0:9)), centre = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(minteger, matrix(1:10), centre = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(mdouble, matrix(1:10 + 0.1), centre = T)), equals("double"))
  
  expect_that(conversion_terms_variable(alogical, array(as.logical(0:9)), centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(ainteger, array(1:10), centre = T), is_a("numeric"))
  expect_that(conversion_terms_variable(adouble, array(1:10 + 0.1), centre = T), is_a("numeric"))
  
  expect_that(storage.mode(conversion_terms_variable(alogical, array(as.logical(0:9)), centre = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(ainteger, array(1:10), centre = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(adouble, array(1:10 + 0.1), centre = T)), equals("double"))  
  
  expect_that(conversion_terms_variable(vlogical, as.logical(0:9), standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vinteger, 1:10, standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vnumeric, 1:10 + 0.1, standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vfactor, factor(1:10), standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vdate, as.Date("2000-01-01") + 1:10, standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10, standardise = T), is_a("numeric"))
  
  expect_that(conversion_terms_variable(mlogical, matrix(as.logical(0:9)), standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(minteger, matrix(1:10), standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(mdouble, matrix(1:10 + 0.1), standardise = T), is_a("numeric"))
  
  expect_that(storage.mode(conversion_terms_variable(mlogical, matrix(as.logical(0:9)), standardise = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(minteger, matrix(1:10), standardise = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(mdouble, matrix(1:10 + 0.1), standardise = T)), equals("double"))
  
  expect_that(conversion_terms_variable(alogical, array(as.logical(0:9)), standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(ainteger, array(1:10), standardise = T), is_a("numeric"))
  expect_that(conversion_terms_variable(adouble, array(1:10 + 0.1), standardise = T), is_a("numeric"))
  
  expect_that(storage.mode(conversion_terms_variable(alogical, array(as.logical(0:9)), standardise = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(ainteger, array(1:10), standardise = T)), equals("double"))
  expect_that(storage.mode(conversion_terms_variable(adouble, array(1:10 + 0.1), standardise = T)), equals("double")) 
  
})

test_that("conversion_terms_variable returns vector of correct length", {
  vlogical <- variable(as.logical(0:9))
  vinteger <- variable(1:10)
  vnumeric <- variable(1:10 + 0.1)
  vfactor <- variable(factor(1:10))
  vdate <- variable(as.Date("2000-01-01") + 1:10)
  vposixt <- variable(as.POSIXct("2000-01-01", tz= "UTC") + 1:10)
  
  expect_that(length(conversion_terms_variable(vlogical, as.logical(0:9))), equals(2))
  expect_that(length(conversion_terms_variable(vinteger, 1:10)), equals(2))
  expect_that(length(conversion_terms_variable(vnumeric, 1:10 + 0.1)), equals(2))
  expect_that(length(conversion_terms_variable(vfactor, factor(1:10))), equals(2))
  expect_that(length(conversion_terms_variable(vdate, as.Date("2000-01-01") + 1:10)), equals(2))
  expect_that(length(conversion_terms_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC") + 1:10)), equals(2))
})

test_that("conversion_terms_variable returns vector of correct values", {
  vlogical <- variable(as.logical(0:9))
  vinteger <- variable(1:10)
  vnumeric <- variable(1:10 + 0.1)
  vfactor <- variable(factor(1:10))
  vdate <- variable(as.Date("2000-01-01") + 1:10)
  vposixt <- variable(as.POSIXct("2000-01-01", tz= "UTC") + 1:10)
  
  expect_equal(conversion_terms_variable(vlogical, TRUE), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vinteger, as.integer(1)), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vnumeric, 1), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vfactor, factor(1)), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vdate, as.Date("2000-01-01")), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC")), c(mean = NA_real_, sd = NA_real_))
  
    expect_equal(conversion_terms_variable(vlogical, TRUE, TRUE), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vinteger, as.integer(1), TRUE), c(mean = 6, sd = NA_real_))
  expect_equal(conversion_terms_variable(vnumeric, 1, TRUE), c(mean = 5.6, sd = NA_real_))
  expect_equal(conversion_terms_variable(vfactor, factor(1), TRUE), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vdate, as.Date("2000-01-01"), TRUE), c(mean = 6, sd = NA_real_))
  expect_equal(conversion_terms_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC"), TRUE), c(mean = 6, sd = NA_real_))
  
      expect_equal(conversion_terms_variable(vlogical, TRUE, TRUE, TRUE), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vinteger, as.integer(1), TRUE, TRUE), c(mean = 6, sd = 3.02765), tolerance = 0.00001)
  expect_equal(conversion_terms_variable(vnumeric, 1, TRUE, TRUE), c(mean = 5.6, sd = 3.02765), tolerance = 0.00001)
  expect_equal(conversion_terms_variable(vfactor, factor(1), TRUE, TRUE), c(mean = NA_real_, sd = NA_real_))
  expect_equal(conversion_terms_variable(vdate, as.Date("2000-01-01"), TRUE, TRUE), c(mean = 6, sd = 3.02765), tolerance = 0.00001)
  expect_equal(conversion_terms_variable(vposixt, as.POSIXct("2000-01-01", tz= "UTC"), TRUE, TRUE), c(mean = 6, sd = 3.02765), tolerance = 0.00001)
})
