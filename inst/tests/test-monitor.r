context("monitor")

test_that("monitor works", {
  
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
select = c("Volume","Girth")
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
  select = c("Volume","Girth","Height")
)
  
  models <- add_jags(model1, model2)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, mode = "test")
  
  expect_that(is.null(monitor(model1)), is_true())
  expect_that(is.null(monitor(model2)), is_true())
  expect_that(monitor(models), is_a("list"))
  expect_that(monitor(analysis), is_a("list"))
  expect_that(length(monitor(analysis)), is_equivalent_to(2))
  expect_that(monitor(subset_jags(analysis,1)), is_identical_to(c("bGirth","bIntercept","deviance", "sVolume")))
  expect_that(monitor(subset_jags(analysis,2)), is_identical_to(c("bGirth","bHeight","bIntercept","deviance", "sVolume")))
  
  monitor(model1) <- "bGirth"
  expect_that(monitor(model1), is_identical_to("bGirth"))
  expect_that(monitor(analysis) <- "bGirth", throws_error())
})
