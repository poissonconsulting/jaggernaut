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
  
  expect_equal(monitor(model1), "^([^dei]|.[^A-Z])")
  expect_equal(monitor(model2), "^([^dei]|.[^A-Z])")
  expect_that(monitor(models), is_a("list"))
  expect_that(monitor(analysis), is_a("list"))
  expect_that(length(monitor(analysis)), is_equivalent_to(2))

  monitor(model1) <- "bGirth"
  expect_that(monitor(model1), is_identical_to("bGirth"))
  expect_that(monitor(analysis) <- "bGirth", throws_error())

  monitor(model1) <- c("bGirth", "sVolume-")
  expect_equal(monitor(model1), c("bGirth", "sVolume-"))
  monitor(model1) <- c("sVolume-", "bGirth")
  expect_equal(monitor(model1), c("bGirth", "sVolume-"))
  expect_equal(monitor(model1, trim_suffix = TRUE), c("bGirth", "sVolume"))
  expect_equal(monitor(model1, drop_suffixed = TRUE), c("bGirth"))






})
