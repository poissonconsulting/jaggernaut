context("model")

test_that("model returns object of correct class", {
  
  model <- model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  expect_that(model, is_a("jmodel"))
})
