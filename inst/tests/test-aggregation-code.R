context("aggregation_code")

test_that("aggregation_code works", {
  
  model <- jags_model(" model { 
    bLambda ~ dlnorm(0,10^-2) 
    for (i in 1:nrow) { 
    x[i]~dpois(bLambda) 
    } 
}")
  
  expect_that(is.null(aggregation_code(model)), is_true())
  
  aggregation_code <- "data { 
    for (i in 1:nrow) { 
      predict[i] <- bLambda
    } 
  } "
  
  aggregation_code(model) <- aggregation_code
  expect_that(aggregation_code(model),is_identical_to(aggregation_code))

  aggregation_code <- "model { 
    for (i in 1:nrow) { 
      predict[i] <- bLambda
    } 
  } "
  
  expect_message(aggregation_code(model) <- aggregation_code, "aggregation code converted to data block")
  
})
