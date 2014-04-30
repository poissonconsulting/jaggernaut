context("derived_code")

test_that("derived_code works", {
  
  model <- jags_model(" model { 
                    bLambda ~ dlnorm(0,10^-2) 
                    for (i in 1:nrow) { 
                    x[i]~dpois(bLambda) 
                    } 
}")
  
  expect_that(is.null(derived_code(model)), is_true())
  
  derived_code <- " model { 
      for (i in 1:nrow) { 
        predict[i] <- bLambda
    } 
  } "
  
  derived_code(model) <- derived_code
  expect_that(derived_code(model),is_identical_to(derived_code))
})
