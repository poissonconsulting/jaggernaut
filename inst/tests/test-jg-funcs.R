context("jg_funcs")

test_that("recognises function", {
  expect_identical(jg_funcs("<- dnorm("), "dnorm")
  expect_identical(jg_funcs("<-dnorm ("), "dnorm")
  expect_identical(jg_funcs("<-  dnorm  ("), "dnorm")
  expect_identical(jg_funcs("dnorm("), "dnorm")
  expect_identical(jg_funcs(" dnorm ("), "dnorm")
  expect_identical(jg_funcs("<-\ndnorm\n("), "dnorm")
  expect_identical(jg_funcs("~<-\ndnorm\n("), "dnorm")
  expect_identical(jg_funcs("~<-\nd_1no.rm\n("), "d_1no.rm")
  expect_identical(jg_funcs(". d_1no.rm\n("), "d_1no.rm")
  expect_identical(jg_funcs("( d_1no.rm\n("), "d_1no.rm")
  expect_identical(jg_funcs("1 d_1no.rm\n("), "d_1no.rm")
})

test_that("does not recognise function", {
  expect_identical(jg_funcs("~dnorm"), character(0))
  expect_identical(jg_funcs("~ dnorm ("), character(0))
  expect_identical(jg_funcs("1dnorm ("), character(0))
  expect_identical(jg_funcs(" 1dnorm ("), character(0))
  expect_identical(jg_funcs("- 1dnorm ("), character(0))
  expect_identical(jg_funcs("- _dnorm ("), character(0))
  expect_identical(jg_funcs("- .dnorm ("), character(0))
})

test_that("recognises multiple unique sorted functions", {
  expect_identical(jg_funcs(" dunif( dnorm( dnorm("), c("dnorm","dunif"))
})

