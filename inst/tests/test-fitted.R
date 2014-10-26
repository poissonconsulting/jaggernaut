context("fitted")

test_that("fitted works", {
  
  model1 <- jags_model(
    " model { 
    sVolume ~ dunif(0, 100)
    bIntercept ~ dnorm (0, 100^-2)
    bGirth ~ dnorm(0, 100^-2)
    for (i in 1:length(Volume)) {
    eVolume[i] <- bIntercept + bGirth * Girth[i] 
    Volume[i] ~ dnorm(eVolume[i], sVolume^-2) 
    } 
}",
derived_code = "data {
    for (i in 1:length(Volume)) {
    prediction[i] <- bIntercept + bGirth * Girth[i] 
    residual[i] <- (Volume[i] - prediction[i]) / sVolume
    }
}",
select_data = c("Volume","Girth")
  )
  
  model2 <- jags_model(
    " model { 
    sVolume ~ dunif(0, 100)
    bIntercept ~ dnorm (0, 100^-2)
    bGirth ~ dnorm(0, 100^-2)
    bHeight ~ dnorm(0, 100^-2)
    for (i in 1:length(Volume)) {
    eVolume[i] <- bIntercept + bGirth * Girth[i] + bHeight * Height[i]
    Volume[i] ~ dnorm(eVolume[i], sVolume^-2) 
    } 
    }",
derived_code = "data {
    for (i in 1:length(Volume)) {
    prediction[i] <- bIntercept + bGirth * Girth[i] + bHeight * Height[i]
    residual[i] <- (Volume[i] - prediction[i]) / sVolume
    }
}",
  select_data = c("Volume","Girth","Height")
  )
  
  models <- combine(model1, model2)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, mode = "test")
  
  fitted <- fitted(analysis)
  
  expect_that(fitted, is_a("data.frame"))
  expect_that(nrow(fitted), is_equivalent_to(nrow(data)))
  
  data <- data[-1,,drop = FALSE]  
  fitted <- fitted(analysis, data = data)
  
  expect_that(fitted, is_a("data.frame"))
  expect_that(nrow(fitted), is_equivalent_to(nrow(data)))
  })
