context("modify_data_derived")

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
                       modify_data = function (data) data,
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
                       model_id = "md 2",
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
                       model_id = "md 3",                     
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
                       model_id = "md 4",                     
                       select_data = c("C")
  )
  
  data(peregrine)
  data <- peregrine
  
  data$C <- data$Pairs
  
  models <- combine(model1, model2, model3, model4)
  analyses <- jags_analysis (models, data, mode = "test")
  
  modify_data_derived(analyses) <- modify_data(analyses)[[1]]
  expect_identical(modify_data_derived(analyses)[[1]],modify_data(analyses)[[1]])
  expect_identical(modify_data_derived(analyses)[[2]],modify_data(analyses)[[1]])
  modify_data_derived(analyses) <- modify_data(analyses)
  expect_identical(modify_data_derived(analyses),modify_data(analyses))
  expect_error(modify_data_derived(analyses) <- modify_data(analyses)[1:2])
  expect_error(modify_data_derived(analyses) <- "txt")
})
