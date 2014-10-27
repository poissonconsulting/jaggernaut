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
  expect_equal(model_name(model, reference = TRUE), mname2)
  
  
  analysis <- jags_analysis (model, data, mode = "test") 
  expect_equal(model_name(analysis), mname2)
  expect_equal(model_name(analysis, reference = TRUE), mname2)
  
  model_name(model) <- NA
  expect_true(is.na(model_name(model)))
  expect_equal(model_name(model, reference = TRUE), "Model1")
  
  analysis <- jags_analysis (model, data, mode = "test") 
  expect_true(is.na(model_name(analysis)))
  expect_equal(model_name(analysis, reference = TRUE), "Model1")
  
  expect_error(model_name(model) <- NULL)  
  expect_error(model_name(analysis) <- NA)  
  expect_error(model_name(analysis) <- NULL)
  expect_error(model_name(analysis) <- "xx")
  expect_error(model_name(model) <- c("xx","x"))
})

test_that("four models", {
  
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

model3 <- jags_model("
                     model {
                     alpha ~ dunif(-20, 20)
                     beta1 ~ dunif(-10, 10)
                     
                     for (i in 1:length(Year)) {
                     log(eC[i]) <- alpha + beta1 * Year[i] 
                     C[i] ~ dpois(eC[i])
                     }
                     }",
                     model_name = "md22",                     
                     select_data = c("C","Year*")
)

model4 <- jags_model("
                     model {
                     alpha ~ dunif(-20, 20)
                     
                     for (i in 1:length(Year)) {
                     log(eC[i]) <- alpha
                     C[i] ~ dpois(eC[i])
                     }
                     }",
                     select_data = c("C")
)

data(peregrine)
data <- peregrine

data$C <- data$Pairs

models <- combine(model1, model2, model3, model4)

model_name(models)

analysis <- jags_analysis (combine(model1, model2), data, mode = "test")

expect_equal(model_name(models), model_name(analysis))
expect_equal(model_name(models)$Model1, "md 1")
expect_equal(model_name(models)$Model2, "md22")
})


