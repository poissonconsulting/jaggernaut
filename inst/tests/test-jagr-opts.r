context("jagr_opts")

test_that("default mode is report", {
  
    opts_jagr(mode = "default")
    def <- opts_jagr(mode = "report")
    rep <- opts_jagr()
    
    expect_that(def,is_identical_to(rep))
  })

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
