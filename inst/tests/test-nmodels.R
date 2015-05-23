context("nmodels")

test_that("nmodels returns object of correct class and length", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test") 
  
  expect_that(nmodels(analysis), is_a("integer"))
  expect_equal(length(nmodels(analysis)), 1)
})

test_that("nmodels returns object of correct value", {
  
  mod1 <- jags_model(" model { 
    bLambda ~ dlnorm(0,10^-2) 
    for (i in 1:length(x)) { 
      x[i]~dpois(bLambda) 
    } 
  }")

  mod2 <- jags_model(" model { 
    bLambda ~ dlnorm(0,2^-2) 
    for (i in 1:length(x)) { 
      x[i]~dpois(bLambda) 
    } 
  }")
  mods <- combine(mod1,mod2)
  
  data <- data.frame(x = rpois(100,1))

  an <- jags_analysis (mods, data, mode = "test") 
  
  expect_equal(nmodels(an), 2)
})
