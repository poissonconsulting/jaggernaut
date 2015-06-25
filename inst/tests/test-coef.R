context("coef")

test_that("coef level = no", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) #  $\\lambda$
                      bLambda2 ~ dlnorm(0,10^-2) # # $\\lambda_2$
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test")
  
  x <- coef(analysis)
  expect_is(x, "data.frame")
  expect_identical(colnames(x), c("estimate", "lower", "upper", "sd", "error", "significance"))
  expect_identical(rownames(x), c("bLambda", "bLambda2"))

  x <- coef(analysis, latex = TRUE)
  expect_is(x, "data.frame")
  expect_identical(colnames(x), c("estimate", "lower", "upper", "sd", "error", "significance"))
  expect_identical(rownames(x), c("$\\lambda$", "$\\lambda_2$"))

  x <- coef(analysis, parm = "bLambda", level = "no")
  expect_is(x, "data.frame")
  expect_is(x, "jags_sample")
  expect_identical(rownames(x), "bLambda")
  expect_equal(ncol(x), 101)
  
  x <- coef(analysis, level = "no")
  expect_is(x, "data.frame")
  expect_is(x, "jags_sample")
  expect_identical(rownames(x), c("bLambda", "bLambda2"))
  expect_equal(ncol(x), 101)  
  
})

test_that("coef level = no []", {
  
  model <- jags_model(" model { 
                      bLambda[1] ~ dlnorm(0,10^-2) 
                      bLambda[2] ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda[1]) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test")
  
  x <- coef(analysis)
  expect_is(x, "data.frame")
  expect_identical(colnames(x), c("estimate", "lower", "upper", "sd", "error", "significance"))
  expect_identical(rownames(x), c("bLambda[1]", "bLambda[2]"))

  x <- coef(analysis, parm = "bLambda", level = "no")
  expect_is(x, "data.frame")
  expect_is(x, "jags_sample")
  expect_identical(rownames(x), c("bLambda[1]", "bLambda[2]"))
  expect_equal(ncol(x), 101)
  
  x <- coef(analysis, level = "no")
  expect_is(x, "data.frame")
  expect_is(x, "jags_sample")
  expect_identical(rownames(x), c("bLambda[1]", "bLambda[2]"))
  expect_equal(ncol(x), 101)  
  
})
