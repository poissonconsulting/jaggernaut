context("subset_jags")

test_that("subset_jags.jags_analysis returns object of correct class", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:nrow) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  an <- jags_analysis (model, data, mode = "test") 
  expect_that(subset_jags(an), is_a("jags_analysis"))
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
  mods <- add_jags(mod1,mod2)
  
  data <- data.frame(x = rpois(100,1))

  an <- jags_analysis (mods, data, mode = "test") 
  
  expect_that(subset_jags(an), is_a("jags_analysis"))
  expect_that(subset_jags(an, 0), is_a("jags_analysis"))
  expect_that(subset_jags(an, 1), is_a("jags_analysis"))
  expect_that(subset_jags(an, 2), is_a("jags_analysis"))
  expect_that(nmodels(subset_jags(an, 2)), is_equivalent_to(1))
  expect_that(nmodels(subset_jags(an, 1)), is_equivalent_to(1))
  expect_that(nmodels(subset_jags(an, 0)), is_equivalent_to(1))
  expect_that(subset_jags(subset_jags(an)), is_a("jags_analysis"))
  expect_that(subset_jags(an, 3), throws_error())
  expect_that(subset_jags(an, - 1), throws_error())
  expect_that(subset_jags(an, c(1,2)), throws_error())
})
