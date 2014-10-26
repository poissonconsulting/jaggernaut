context("cross_corr")

test_that("1 parameter", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test")
  
  x <- cross_corr(analysis)
  
  expect_equal(rownames(x), c("bLambda"))
  expect_equal(colnames(x), c("bLambda"))
})
