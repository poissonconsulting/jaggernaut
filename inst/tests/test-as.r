context("as")

test_that("as returns correct classes", {
  
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
    select_data = c("Volume","Girth","Height")
  )
  
  models <- combine(model1, model2)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, mode = "test")
  
  expect_that(analysis, is_a("jags_analysis"))
  expect_that(analyses(analysis), is_a("list"))
  expect_that(analyses(analysis)[[1]], is_a("jagr_analysis"))
  expect_that(as.jags_model(analysis), is_a("jags_model"))
})
