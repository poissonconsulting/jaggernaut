context("predictive-check")

test_that("predictive_check works 1 check", {
  
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
    eVolume[i] <- bIntercept + bGirth * Girth[i] 
    NewVolume[i] ~ dnorm(eVolume[i], sVolume^-2) 
    VolumeDensity[i] <- log(dnorm(Volume[i], eVolume[i], sVolume^-2))
    NewVolumeDensity[i] <- log(dnorm(NewVolume[i], eVolume[i], sVolume^-2))
    }  
  discrepancy <- sum(VolumeDensity) - sum(NewVolumeDensity)
}",
select_data = c("Volume","Girth")
)
  
  models <- combine(model1)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, niters = 10^2, mode = "test")
  
  check <- predictive_check(analysis)
  expect_that(is.data.frame(check), is_true())
  expect_equal(nrow(check), 1)
  expect_that(colnames(check), is_identical_to(c("estimate", "lower", "upper", "sd", "error", "significance")))
})

test_that("predictive_check works 2 checks", {
  
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
eVolume[i] <- bIntercept + bGirth * Girth[i] 
NewVolume[i] ~ dnorm(eVolume[i], sVolume^-2) 
VolumeDensity[i] <- log(dnorm(Volume[i], eVolume[i], sVolume^-2))
NewVolumeDensity[i] <- log(dnorm(NewVolume[i], eVolume[i], sVolume^-2))
}  
discrepancy <- sum(VolumeDensity) - sum(NewVolumeDensity)
ppc2 <- sum(NewVolumeDensity) - sum(VolumeDensity)
}",
select_data = c("Volume","Girth")
)
  
  models <- combine(model1)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, niters = 10^2, mode = "test")
  
  check <- predictive_check(analysis, parm = c("discrepancy", "ppc2"))
  expect_that(is.data.frame(check), is_true())
  expect_equal(nrow(check), 2)
expect_that(colnames(check), is_identical_to(c("estimate", "lower", "upper", "sd", "error", "significance")))
  expect_equal(rownames(check), c("discrepancy", "ppc2"))

})
