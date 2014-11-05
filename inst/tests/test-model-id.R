context("model_id")

test_that("model_id updates simple model", {
  
  mname <- "x.x_"
  mname2 <- "yt5"
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}",
                      model_id = mname)
  
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test") 
  
  expect_equal(model_id(model), mname)
  expect_equal(model_id(analysis), mname)

  model_id(model) <- mname2
  expect_equal(model_id(model), mname2)
  expect_equal(model_id(model, reference = TRUE), mname2)
  
  
  analysis <- jags_analysis (model, data, mode = "test") 
  expect_equal(model_id(analysis), mname2)
  expect_equal(model_id(analysis, reference = TRUE), mname2)
  
  model_id(model) <- NA
  expect_true(is.na(model_id(model)))
  expect_equal(model_id(model, reference = TRUE), "Model1")
  
  analysis <- jags_analysis (model, data, mode = "test") 
  expect_true(is.na(model_id(analysis)))
  expect_equal(model_id(analysis, reference = TRUE), "Model1")
  
  expect_error(model_id(model) <- "Model1")    
  expect_error(model_id(model) <- NULL)  
  expect_error(model_id(analysis) <- NA)  
  expect_error(model_id(analysis) <- NULL)
  expect_error(model_id(analysis) <- "xx")
  expect_error(model_id(model) <- c("xx","x"))
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
model_id = "md 1",
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
                     model_id = "md22",
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
                     model_id = "md22",                     
                     select_data = c("C","Year*")
)

model4 <- jags_model("
                     model {
                     alpha ~ dunif(-20, 20)
                     
                     for (i in 1:length(C)) {
                     log(eC[i]) <- alpha
                     C[i] ~ dpois(eC[i])
                     }
                     }",
                     select_data = c("C")
)

data(peregrine)
data <- peregrine

data$C <- data$Pairs

expect_equal(model_id(model1, reference = TRUE), c("md 1"))
expect_equal(model_id(model2, reference = TRUE), c("md22"))
expect_equal(model_id(model3, reference = TRUE), c("md22"))
expect_equal(model_id(model4, reference = TRUE), c("Model1"))
expect_equal(model_id(combine(model1, model2, model3, model4), reference = TRUE), c("md 1", "Model2", "Model3", "Model4"))

expect_equal(model_id(model1), c("md 1"))
expect_equal(model_id(model2), c("md22"))
expect_equal(model_id(model3), c("md22"))
expect_true(is.na(model_id(model4)))

expect_is(model_id(combine(model1, model2)), "list")
expect_equal(names(model_id(combine(model1, model2))), c("md 1", "md22"))
expect_equal(unlist(model_id(combine(model1, model2)), use.names = FALSE), c("md 1", "md22"))
expect_equal(names(model_id(combine(model1, model2, model3))), c("md 1", "Model2", "Model3"))
expect_equal(unlist(model_id(combine(model1, model2, model3)), use.names = FALSE), c("md 1", "md22", "md22"))


expect_equal(model_id(combine(model2)), model_id(jags_analysis (combine(model2), data, mode = "test")))
expect_equal(model_id(combine(model1, model2)), model_id(jags_analysis (combine(model1, model2), data, mode = "test")))
expect_equal(model_id(combine(model1, model2, model4)), model_id(jags_analysis (combine(model1, model2, model4), data, mode = "test")))
})


