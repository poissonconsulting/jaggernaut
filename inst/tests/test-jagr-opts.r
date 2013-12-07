context("jagr_opts")

test_that("changing to unknown mode throws error", {
  
  opts_jagr(mode = "report")
  expect_that(opts_jagr(mode = "unknown"),throws_error())
})

test_that("setting unknown option has not effect", {
  
  opts_jagr(mode = "report")
  expect_that(opts_jagr(unknown = 3),is_identical_to(opts_jagr()))
})

test_that("setting option sets custom mode", {
  
  opts_jagr(mode = "report")
  nchains <- opts_jagr()$nchains
  opts_jagr(nchains = nchains + 1)
  
  expect_that(opts_jagr("mode"),is_identical_to("custom"))
})

test_that("changing to current mode has no effect", {
  
  opts_jagr(mode = "report")
  opts <- opts_jagr()

  expect_that(opts_jagr(mode = "current"),is_identical_to(opts))
})

test_that("changing to known mode sets that mode", {
  
  opts_jagr(mode = "report")
  nchains <- opts_jagr()$nchains
  opts_jagr(nchains = nchains + 1)
  opts_jagr(nchains = nchains)
  
  expect_that(opts_jagr("mode"),is_identical_to("report"))
})

test_that("set for all modes", {
  
  opts_jagr(mode = "debug")
  expect_that(opts_jagr("mode"),is_identical_to("debug"))
  opts_jagr(mode = "explore")
  expect_that(opts_jagr("mode"),is_identical_to("explore"))
  opts_jagr(mode = "test")
  expect_that(opts_jagr("mode"),is_identical_to("test"))  
  opts_jagr(mode = "demo")
  expect_that(opts_jagr("mode"),is_identical_to("demo")) 
  opts_jagr(mode = "report")
  expect_that(opts_jagr("mode"),is_identical_to("report"))
  opts_jagr(mode = "paper")
  expect_that(opts_jagr("mode"),is_identical_to("paper"))
})

test_that("default mode is report", {
  
  opts_jagr(mode = "default")
  def <- opts_jagr(mode = "report")
  rep <- opts_jagr()
  
  expect_that(def,is_identical_to(rep))
})

test_that("cannot set parallel unless registered doPar backend", {
  
  expect_that(opts_jagr(parallel = TRUE),gives_warning())
})
