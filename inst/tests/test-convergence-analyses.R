context("convergence-models")

test_that("convergence models", {
  
  model1 <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) #  $\\lambda$
                      bLambda2 ~ dlnorm(0,10^-2) # # $\\lambda_2$
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
    model2 <- jags_model(" model { 
                      bLambda ~ dlnorm(0,5^-2) #  $\\lambda$
                      bLambda2 ~ dlnorm(0,5^-2) # # $\\lambda_2$
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis1 <- jags_analysis (model1, data, mode = "test")
  analysis2 <- jags_analysis (model1, data, mode = "test")
  
  expect_length(convergence_analyses(analysis1, analysis2), 1)
})
