context("default_model_id")

test_that("default_model_id updates simple model", {
  
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
  expect_equal(default_model_id(analysis), mname)
  
  model_id(model) <- mname2
  analysis <- jags_analysis (model, data, mode = "test") 
  expect_equal(default_model_id(analysis), mname2)
  
  default_model_id(analysis) <- mname2
  expect_equal(default_model_id(analysis), mname2)
  
  expect_error(default_model_id(model) <- mname2)    
  expect_error(default_model_id(model) <- mname)    
  expect_error(default_model_id(analysis) <- mname)    
  expect_error(default_model_id(analysis) <- "Model1")    
  expect_error(default_model_id(analysis) <- "xx")    
  expect_error(default_model_id(analysis) <- NA)    
  expect_error(default_model_id(analysis) <- NULL)
  expect_error(default_model_id(analysis) <- 1)
  expect_error(default_model_id(analysis) <- c(mname, mname2))
  expect_error(default_model_id(analysis) <- c(mname2, mname2))
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

data <- jaggernaut::falcon

data$C <- data$Pairs

models <- combine(model1, model2, model3, model4)
analyses <- jags_analysis(models, data = data, mode = "test")
  
expect_equal(default_model_id(analyses), "md 1")

default_model_id(analyses) <- "Model2"
expect_equal(default_model_id(analyses), "Model2")

expect_error(default_model_id(analyses) <- "Model1")
expect_error(default_model_id(analyses) <- "md22")

expect_equal(as.jags_model(subset(analyses)), model2)
expect_equal(subset(analyses), subset(analyses, model_id = 2))
})


