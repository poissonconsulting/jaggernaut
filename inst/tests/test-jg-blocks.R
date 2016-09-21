context("jg-blocks")

test_that("jg_blocks gets blocks", {

  x <- "model {
for (i in 1:length(Volume)) {
prediction[i] <- bIntercept + bGirth * Girth[i]
E[i] <- pow(Volume[i] - prediction[i], 2) / prediction[i]
newVolume[i] ~ dnorm(prediction, sVolume^-2)
E2[i] <- pow(newVolume[i] - prediction[i], 2) / prediction[i]
}
EE[1] <- sum(E)
EE[2] <- sum(E2)
}"
  
  expect_identical(names(jg_blocks(x)), "model")
})

