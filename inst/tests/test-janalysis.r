context("janalysis")

test_that("janalysis returns object of correct class", {
  
  model <- jmodel(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- janalysis (model, data, quiet = T)
  
  expect_that(analysis, is_a("janalysis"))
})
