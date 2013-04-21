context("analysis")

test_that("analysis returns object of correct class", {
  
  model <- model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  data <- data.frame(x = rpois(100,1))
  an <- jags_analysis (model, data, mode = "test")
  
  expect_that(an, is_a("jags_analysis"))
})
