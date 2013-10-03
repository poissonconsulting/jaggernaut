context("nmodels")

test_that("nmodels returns object of correct class and length", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:nrow) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test") 
  
  expect_that(nmodels(analysis), is_a("integer"))
  expect_that(length(nmodels(analysis)), equals(1))
})

test_that("nmodels returns object of correct value", {
  
  mod1 <- jags_model(" model { 
    bLambda ~ dlnorm(0,10^-2) 
    for (i in 1:nrow) { 
      x[i]~dpois(bLambda) 
    } 
  }")

  mod2 <- jags_model(" model { 
    bLambda ~ dlnorm(0,2^-2) 
    for (i in 1:nrow) { 
      x[i]~dpois(bLambda) 
    } 
  }")
  mods <- add_jags(mod1,mod2)
  
  data <- data.frame(x = rpois(100,1))

  an <- jags_analysis (mods, data, mode = "test") 
  
  expect_that(nmodels(an), equals(2))
})
