context("auto_corr")

test_that("1 parameter", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test")
 
  x <- auto_corr(analysis)
  
  expect_that(x, is_a("matrix"))
  expect_equal(dimnames(x)[[1]], paste("Lag", c("1", "5", "10")))
  expect_equal(dimnames(x)[[2]], "bLambda")
})


test_that("test multi par", {
  
  model <- jags_model("
                      model {
                      alpha ~ dunif(-20, 20)
                      beta1 ~ dunif(-10, 10)
                      beta2 ~ dunif(-10, 10)
                      beta3 ~ dunif(-10, 10)
                      
                      for (i in 1:length(Year)) {
                      log(eC[i]) <- alpha + beta1 * Year[i] 
                      + beta2 * Year[i]^2 + beta3 * Year[i]^3
                      C[i] ~ dpois(eC[i])
                      }
                      }",
 derived_code = "data{
 for (i in 1:length(Year)) {
 log(prediction[i]) <- alpha + beta1 * Year[i]
 + beta2 * Year[i]^2 + beta3 * Year[i]^3
 }
 }",
select_data = c("C","Year*")
  )

data <- jaggernaut::falcon

data$C <- data$Pairs

analysis <- jags_analysis (model, data, mode = "test")

x <- auto_corr(analysis)

expect_that(x, is_a("matrix"))
expect_equal(dimnames(x)[[1]], paste("Lag", c("1", "5", "10")))
expect_equal(dimnames(x)[[2]], c("alpha", "beta1", "beta2", "beta3"))
expect_equal(dimnames(auto_corr(analysis, parm = "alpha"))[[2]], "alpha")
expect_equal(dimnames(auto_corr(analysis, parm = c("alpha","beta2")))[[2]], c("alpha","beta2"))
expect_equal(dimnames(auto_corr(analysis, parm = c("beta2","alpha")))[[2]], c("alpha","beta2"))
expect_equal(dimnames(auto_corr(analysis, parm = c("beta2","zz")))[[2]], c("beta2"))
})

