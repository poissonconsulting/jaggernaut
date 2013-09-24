context("select")

test_that("select works", {
  
  model <- jags_model(" model { 
                    bLambda ~ dlnorm(0,10^-2) 
                    for (i in 1:nrow) { 
                    x[i]~dpois(bLambda) 
                    } 
}")
  
  expect_that(is.null(select(model)), is_true())
  
  select(model) <- "x"
  expect_that(select(model),is_identical_to("x"))
})
