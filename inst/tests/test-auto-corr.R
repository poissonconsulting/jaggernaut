context("auto_corr")

test_that("1 parameter", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test")
 
  x <- auto_corr(analysis)
  
  expect_that(x, is_a("list"))
  expect_that(names(x), is_a("character"))
  expect_equal(names(x), c("chain1", "chain2"))
  
  x <- x[[1]]
  
  expect_that(x, is_a("array"))
  expect_equal(dimnames(x)[[1]], c("Lag 0",  "Lag 1",  "Lag 5",  "Lag 10", "Lag 50"))
  expect_equal(dimnames(x)[[2]], c("bLambda"))
  expect_equal(dimnames(x)[[3]], c("bLambda"))
  
})
