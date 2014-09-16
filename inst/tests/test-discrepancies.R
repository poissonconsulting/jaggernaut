context("dicrepancies")

test_that("discrepancies works", {
  
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
aggregation_code = "data {
    for (i in 1:length(Volume)) {
    eVolume[i] <- bIntercept + bGirth * Girth[i] 
    NewVolume[i] ~ dnorm(eVolume[i], sVolume^-2) 
    VolumeDensity[i] <- log(dnorm(Volume[i], eVolume[i], sVolume^-2))
    NewVolumeDensity[i] <- log(dnorm(NewVolume[i], eVolume[i], sVolume^-2))
    }  
  VolumeDiscrepancy[1] <- sum(VolumeDensity)
  VolumeDiscrepancy[2] <- sum(NewVolumeDensity)
}",
select = c("Volume","Girth")
)
  
  models <- combine(model1)
  
  data <- trees
  
  analysis <- jags_analysis(models, data = data, mode = "test")
  
  discrepancies <- jags_discrepancies(analysis)
  coef <- coef(discrepancies)
  expect_that(is.data.frame(coef), is_true())
  expect_that(colnames(coef), is_identical_to(c("parameter", "estimate", "lower", "upper", "sd", "error", "significance")))
})
