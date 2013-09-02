context("subset.jags_analysis")

test_that("subset.jags_analysis returns object of correct class", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:nrow) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  an <- jags_analysis (model, data) 
  expect_that(subset(an), is_a("jags_analysis"))
})

test_that("subset.jags_analysis subsets", {
  
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

  an <- jags_analysis (mods, data) 
  
  expect_that(subset(an), is_a("jags_analysis"))
  expect_that(subset(an, 0), is_a("jags_analysis"))
  expect_that(subset(an, 1), is_a("jags_analysis"))
  expect_that(subset(an, 2), is_a("jags_analysis"))
  expect_that(subset(subset(an)), is_a("jags_analysis"))
  expect_that(nmodel(subset(an, 2)), equals(1))
  expect_that(subset(an, 3), throws_error())
  expect_that(subset(an, - 1), throws_error())
  expect_that(subset(an, c(1,2)), throws_error())
})
