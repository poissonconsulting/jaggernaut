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
  
  expect_that(x, is_a("list"))
  expect_that(names(x), is_a("character"))
  expect_equal(names(x), c("chain1", "chain2"))
  
  x <- x[[1]]
  
  expect_that(x, is_a("array"))
  expect_equal(dimnames(x)[[1]], c("Lag 0",  "Lag 1",  "Lag 5",  "Lag 10", "Lag 50"))
  expect_equal(dimnames(x)[[2]], c("bLambda"))
  expect_equal(dimnames(x)[[3]], c("bLambda"))
  
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

data(peregrine)
data <- peregrine

data$C <- data$Pairs

analysis <- jags_analysis (model, data, mode = "test")

x <- auto_corr(analysis)

expect_that(x, is_a("list"))
expect_that(names(x), is_a("character"))
expect_equal(names(x), c("chain1", "chain2"))

x <- x[[1]]

expect_that(x, is_a("array"))
expect_equal(dimnames(x)[[1]], c("Lag 0",  "Lag 1",  "Lag 5",  "Lag 10", "Lag 50"))
expect_equal(dimnames(x)[[2]], c("alpha", "beta1", "beta2", "beta3"))
expect_equal(dimnames(x)[[3]], c("alpha", "beta1", "beta2", "beta3"))
})

