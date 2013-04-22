context("jags_model")

test_that("jags_model returns object of correct class", {
  
  mod <- jags_model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  expect_that(mod, is_a("jags_model"))
})
