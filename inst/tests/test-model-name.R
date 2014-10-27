context("model_name")

test_that("model_name updates simple model", {
  
  mname <- "x.x_"
  mname2 <- "yt5"
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}",
                      model_name = mname)
  
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test") 
  
  expect_equal(model_name(model), mname)
  expect_equal(model_name(analysis), mname)

  model_name(model) <- mname2
  expect_equal(model_name(model), mname2)
  
  analysis <- jags_analysis (model, data, mode = "test") 
  expect_equal(model_name(analysis), mname2)
})

test_that("analysis updates glm - two model", {
  
  model1 <- jags_model("
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
model_name = "md 1",
select_data = c("C","Year*")
  )

model2 <- jags_model("
                     model {
                     alpha ~ dunif(-20, 20)
                     beta1 ~ dunif(-10, 10)
                     beta2 ~ dunif(-10, 10)
                     
                     for (i in 1:length(Year)) {
                     log(eC[i]) <- alpha + beta1 * Year[i] 
                     + beta2 * Year[i]^2 
                     C[i] ~ dpois(eC[i])
                     }
                     }",
                     model_name = "md22",
select_data = c("C","Year*")
)

data(peregrine)
data <- peregrine

data$C <- data$Pairs

models <- combine(model1, model2)

analysis <- jags_analysis (combine(model1, model2), data, mode = "test")

expect_equal(model_name(models), model_name(analysis))
expect_equal(model_name(models)$Model1, "md 1")
expect_equal(model_name(models)$Model2, "md22")
})


