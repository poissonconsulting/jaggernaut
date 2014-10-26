context("update")

test_that("analysis updates", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test") 
  
  niters <- niters(analysis)
  analysis <- update(analysis)
  analysis <- update(analysis)
  
  expect_equal(niters(analysis), niters * 4)
  })
