context("dvariable")

test_that("creates correct class of dvariable", {
  dlogical <- dvariable(c(FALSE,TRUE,FALSE))
  dnumeric <- dvariable(1:10 + 0.1)
  dinteger <- dvariable(1:10)
  dfactor <- dvariable(factor(1:10))
  ddate <- dvariable(as.Date("2000-01-01") + 1:10)
  dposixt <- dvariable(as.POSIXct("2000-01-01", tz= "GMT") + 1:10)
  
  expect_that(dlogical, is_a('dlogical'))
  expect_that(dnumeric, is_a('dnumeric'))
  expect_that(dinteger, is_a('dinteger'))
  expect_that(dfactor, is_a('dfactor'))
  expect_that(ddate, is_a('ddate'))
  expect_that(dposixt, is_a('dposixt'))
})
