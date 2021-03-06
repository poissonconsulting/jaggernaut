context("subset")

test_that("subset.jags_analysis returns object of correct class", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  an <- jags_analysis (model, data, mode = "test") 
  expect_that(subset(an, model_id = 1), is_a("jags_analysis"))
})

test_that("subset.jags_analysis subsets", {
  
  mod1 <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}",
                     model_id = "m1")

  mod2 <- jags_model(" model { 
                      bLambda ~ dlnorm(0,2^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  mods <- combine(mod1,mod2)
  
  data <- data.frame(x = rpois(100,1))

  an <- jags_analysis (mods, data, mode = "test") 
  
  expect_that(subset(an, "Model2"), is_a("jags_analysis"))  
  expect_that(subset(an, 1:2), is_a("jags_analysis"))
  expect_that(subset(an, 1), is_a("jags_analysis"))
  expect_that(subset(an, 2), is_a("jags_analysis"))
  expect_that(nmodels(subset(an, 2)), is_equivalent_to(1))
  expect_that(nmodels(subset(an, 1)), is_equivalent_to(1))
  expect_that(nmodels(subset(an, 1:2)), is_equivalent_to(2))  
  expect_that(nmodels(subset(an, c(1,1,2,2))), is_equivalent_to(2))    
  expect_that(subset(subset(an, 1:2), 1:2), is_a("jags_analysis"))
  expect_that(subset(an, 3), throws_error())
  expect_that(subset(an, 0), throws_error())  
  expect_that(subset(an, - 1), throws_error())
})
