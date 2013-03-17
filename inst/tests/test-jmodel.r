context("jmodel")

test_that("jmodel returns object of correct class", {
  
  model <- jmodel(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  expect_that(model, is_a("jmodel"))
})
