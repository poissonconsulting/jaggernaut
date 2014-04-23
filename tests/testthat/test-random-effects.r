context("random_effects")

test_that("random_effects works", {
  
  model <- jags_model(" model { 
                    bLambda ~ dlnorm(0,10^-2) 
                    for (i in 1:nrow) { 
                    x[i]~dpois(bLambda) 
                    } 
}")
  
  expect_that(is.null(random_effects(model)), is_true())
  
  random_effects(model) <- NULL
  expect_that(is.null(random_effects(model)), is_true())
})