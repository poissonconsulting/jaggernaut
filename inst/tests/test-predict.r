context("predict")

test_that("predict works", {
  
  model1 <- jags_model(
    " model { 
    sVolume ~ dunif(0, 100)
    bIntercept ~ dnorm (0, 100^-2)
    bGirth ~ dnorm(0, 100^-2)
    for (i in 1:nrow) {
    eVolume[i] <- bIntercept + bGirth * Girth[i] 
    Volume[i] ~ dnorm(eVolume[i], sVolume^-2) 
    } 
}",
derived_code = "model {
    for (i in 1:nrow) {
      prediction[i] <- bIntercept + bGirth * Girth[i] 
      residual[i] <- (Volume[i] - prediction[i]) / sVolume
    }
}",
select = c("Volume","Girth")
  )
  
  model2 <- jags_model(
    " model { 
    sVolume ~ dunif(0, 100)
    bIntercept ~ dnorm (0, 100^-2)
    bGirth ~ dnorm(0, 100^-2)
    bHeight ~ dnorm(0, 100^-2)
    for (i in 1:nrow) {
    eVolume[i] <- bIntercept + bGirth * Girth[i] + bHeight * Height[i]
    Volume[i] ~ dnorm(eVolume[i], sVolume^-2) 
    } 
    }",
derived_code = "model {
    for (i in 1:nrow) {
      prediction[i] <- bIntercept + bGirth * Girth[i] + bHeight * Height[i]
      residual[i] <- (Volume[i] - prediction[i]) / sVolume
    }
}",
  select = c("Volume","Girth","Height")
  )
  
  models <- add_jags(model1, model2)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, mode = "test")
  
  prediction <- predict(analysis)
  
  expect_that(prediction, is_a("data.frame"))
  
  prediction <- predict(analysis, newdata = "Girth")
  expect_that(prediction, is_a("data.frame"))
  expect_that(nrow(prediction), is_equivalent_to(50))
  
  prediction <- predict(analysis, newdata = data[1,,drop=FALSE])
  expect_that(prediction, is_a("data.frame"))
  expect_that(nrow(prediction), is_equivalent_to(1))  
  
})
