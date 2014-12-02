context("is_parallel")

test_that("FALSE", {
  
  model <- jags_model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:length(x)) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  data <- data.frame(x = rpois(100,1))
  opts_jagr(parallel = FALSE)
  analysis <- jags_analysis (model, data, mode = "test")  
  
  expect_false(is_parallel(analysis))
})

