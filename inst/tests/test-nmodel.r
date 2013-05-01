context("nmodel")

test_that("nmodel returns object of correct class and length", {
  
  mod <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:nrow) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  an <- jags_analysis (mod, data, mode = "test") 
  
  expect_that(nmodel(an), is_a("integer"))
  expect_that(length(nmodel(an)), equals(1))
})

test_that("nmodel returns object of correct value", {
  
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
  mods <- list(mod1,mod2)
  
  data <- data.frame(x = rpois(100,1))

  an <- jags_analysis (mods, data, mode = "test") 
  
  expect_that(nmodel(an), equals(2))
})