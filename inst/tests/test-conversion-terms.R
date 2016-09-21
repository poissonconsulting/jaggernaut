context("conversion-terms")

test_that("conversion_terms on data.frame returns correct dims", {
  data <- data.frame(
    dlogical = as.logical(0:9),
    dnumeric = 1:10 + 0.1,
    dinteger = 1:10,
    dfactor = factor(1:10),
    ddate = as.Date("2000-01-01") + 1:10,
    dposixt = as.POSIXct("2000-01-01", tz= "UTC") + 1:10
  )
  expect_that(conversion_terms(data), is_a("matrix"))
  expect_that(nrow(conversion_terms(data)), equals(6))
  expect_that(ncol(conversion_terms(data)), equals(2))
  expect_that(colnames(conversion_terms(data)), equals(c("mean", "sd")))
  expect_that(rownames(conversion_terms(data)), equals(colnames(data)))
})

test_that("conversion_terms on data list returns correct dims", {
  data <- list(
    dlogical = as.logical(0:9),
    dnumeric = 1:10 + 0.1,
    dinteger = 1:10,
    dfactor = factor(1:10),
    ddate = as.Date("2000-01-01") + 1:10,
    dposixt = as.POSIXct("2000-01-01", tz= "UTC") + 1:10
  )
  expect_that(conversion_terms(data), is_a("matrix"))
  expect_that(nrow(conversion_terms(data)), equals(6))
  expect_that(ncol(conversion_terms(data)), equals(2))
  expect_that(colnames(conversion_terms(data)), equals(c("mean", "sd")))
  expect_that(rownames(conversion_terms(data)), equals(names(data)))
})

test_that("conversion_terms on data.frame returns correct values", {
  data <- data.frame(
    dlogical = as.logical(0:9),
    dnumeric = 1:10 + 0.1,
    dinteger = 1:10,
    dfactor = factor(1:10),
    ddate = as.Date("2000-01-01") + 1:10,
    dposixt = as.POSIXct("2000-01-01", tz= "UTC") + 1:10
  )
  expect_equivalent(conversion_terms(data, TRUE)[,1], c(NA_real_, 5.6, 6, NA_real_, 6, 6))
  expect_equivalent(conversion_terms(data, TRUE)[,2], rep(NA_real_, 6))

  expect_equivalent(conversion_terms(data, TRUE, TRUE)[,1], c(NA_real_, 5.6, 6, NA_real_, 6, 6))
   x <- c(NA_real_, 3.02765, 3.02765, NA_real_, 3.02765, 3.02765)
  names(x) <- names(data)
  expect_equal(conversion_terms(data, TRUE, TRUE)[,2], x, tolerance = 0.00001)
})

