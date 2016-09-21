context("number of blocks")

test_that("jg_nblock counts correct blocks", {
  
  expect_equal(jg_nblock("model {}"), 1)
  expect_equal(jg_nblock("model{}"), 1)
  expect_equal(jg_nblock("model{}data{}"), 2)
  expect_equal(jg_nblock("model{} data{}"), 2)
  expect_equal(jg_nblock("model{{x <- bt[1:2]}} data{}"), 2)
  expect_equal(jg_nblock("settings{} \n\ndata{} model{}"), 3)
#  expect_equal(jg_nblock("data {X <- 2} model { Y ~ }"), 2)
  expect_equal(jg_nblock("data{X <- 2} model{ Y ~ dpois(X) }"), 2)
})
