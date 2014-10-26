context("select_data")

test_that("select_data works", {
  
  model <- jags_model(" model { 
                    bLambda ~ dlnorm(0,10^-2) 
                    for (i in 1:nrow) { 
                    x[i]~dpois(bLambda) 
                    } 
}")
  
  expect_that(is.null(select_data(model)), is_true())
  
  select_data(model) <- "x"
  expect_that(select_data(model),is_identical_to("x"))
})
