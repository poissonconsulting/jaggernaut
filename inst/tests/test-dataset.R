context("dataset")

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

expect_equal(dataset(analysis), data)
})

