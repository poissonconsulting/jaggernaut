context("models")

test_that("models returns correct values", {
  
  model1 <- jags_model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:length(x)) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  expect_that(model1, is_a("jags_model"))
  expect_that(models(model1), is_a("list"))
  expect_equal(length(models(model1)), 1)
  expect_that(names(models(model1)), is_identical_to("Model1"))
})
