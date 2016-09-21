context("update")

test_that("analysis updates simple model", {
  
  model <- jags_model(" model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test") 
  
  niters <- niters(analysis)
  analysis <- update(analysis, mode = "test")
  analysis <- update(analysis, mode = "test")
  
  expect_equal(niters(analysis), niters * 4)
  })


test_that("analysis updates glm", {
  
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
select_data = c("C","Year*")
)

data <- jaggernaut::falcon

data$C <- data$Pairs

analysis <- jags_analysis (model, data, mode = "test")

  niters <- niters(analysis)
  analysis <- update(analysis, mode = "test")
  analysis <- update(analysis, mode = "test")
  
  expect_equal(niters(analysis), niters * 4)
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
select_data = c("C","Year*")
  )

data <- jaggernaut::falcon

data$C <- data$Pairs

analysis <- jags_analysis (combine(model1, model2), data, mode = "test")

expect_equal(niters(analysis)$Model1, niters(analysis)$Model2)

niters <- niters(analysis)
analysis <- update(analysis, mode = "test")
analysis <- update(analysis, mode = "test")

expect_equal(niters(analysis)$Model1, niters$Model1 * 4)
expect_equal(niters(analysis)$Model2, niters$Model2 * 4)
expect_equal(niters(analysis)$Model1, niters(analysis)$Model2)
})
