context("jg_dists")

test_that("recognises distribution", {
  expect_identical(jg_dists("~ dnorm("), "dnorm")
  expect_identical(jg_dists("~ dnorm ("), "dnorm")
  expect_identical(jg_dists("~dnorm ("), "dnorm")
  expect_identical(jg_dists("~dnorm("), "dnorm")
  expect_identical(jg_dists("~\ndnorm\n("), "dnorm")
  expect_identical(jg_dists("<-~dnorm("), "dnorm")
  expect_identical(jg_dists("~dno_rm("), "dno_rm")
  expect_identical(jg_dists("~d.norm("), "d.norm")
  expect_identical(jg_dists("~d.norm.("), "d.norm.")
  expect_identical(jg_dists("~d.norm2("), "d.norm2")
})

test_that("does not recognise distribution", {
  expect_identical(jg_dists("<- dnorm("), character(0))
  expect_identical(jg_dists("~ dnorm["), character(0))
  expect_identical(jg_dists("~ .dnorm("), character(0))
  expect_identical(jg_dists("~ _dnorm("), character(0))
  expect_identical(jg_dists("~ 1dnorm("), character(0))
  expect_identical(jg_dists("dnorm("), character(0))
})

test_that("recognises multiple unique sorted distribution", {
  expect_identical(jg_dists("~ dunif( ~ dnorm("), c("dnorm","dunif"))
  expect_identical(jg_dists("~ dunif( ~ dunif("), "dunif")
  expect_identical(jg_dists("~dunif(~dunif(~dbeta("), c("dbeta","dunif"))
})
