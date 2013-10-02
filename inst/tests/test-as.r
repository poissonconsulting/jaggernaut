context("as")

test_that("as returns correct classes", {
  
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
    select = c("Volume","Girth","Height")
  )
  
  models <- add_jags(model1, model2)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, mode = "test")
  
  expect_that(class(analysis), equals(c("jags_analysis")))
  expect_that(class(as.jagr_analysis(analysis)), equals(c("list")))
  expect_that(class(as.jagr_analysis(analysis)[[1]]), equals(c("jagr_analysis")))
  expect_that(class(as.jags_model(analysis)), equals(c("jags_model")))
  expect_that(class(as.jags_mcmc(analysis)), equals(c("list")))
  expect_that(class(as.jags_mcmc(analysis)[[1]]), equals(c("jags_mcmc")))
})
