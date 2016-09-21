context("jg_check")

test_that("TRUE for valid code", {
  
x <- "data {
  Y2 <- Y * 2
}
model {
  bIntercept ~ dnorm(0, 5^-2) #}}}}
  bX ~ dnorm(0, 5^-2)
  sY ~ dunif(0, 5)
  for(i in 1:length(Y)) {
    mu[i] <- bIntercept + bX * X[i]
    Y2[i] ~ dnorm (mu[i], sY^-2)
  }
} "
  
  expect_that(jg_check(x), is_true())
  
  "model {
    for (i in 1:length(Volume)) {
    prediction[i] <- bIntercept + bGirth * Girth[i]
    E[i] <- pow(Volume[i] - prediction[i], 2) / prediction[i]
    newVolume[i] ~ dnorm(prediction, sVolume^-2)
    E2[i] <- pow(newVolume[i] - prediction[i], 2) / prediction[i]
    }
    EE[1] <- sum(E)
    EE[2] <- sum(E2)
}"
  expect_that(jg_check(x), is_true())

x <- "data {
  Y2 <- Y * 2
}
model {
  bIntercept ~ dnorm(0, 5^-2) #}}}}
  bX ~ dnorm(0, 5^-2)
  sY ~ dunif(0, 5)
  for(i in (0+1):length(Y)) {
    mu[i] <- bIntercept + bX * X[i]
    Y2[i] ~ dnorm (mu[i], sY^-2)
  }
} "

expect_that(jg_check(x), is_true())
  
})

test_that("warnings and FALSE for invalid code", {
  
x <- "model {
  Y2 <- Y * 2
}
data {
  bIntercept ~ dnorm(0, 5^-2)
  bX <- dnorm(0, 5^-2)
  dnorm ~ mean(0, 5)

  fore(i in 1:length(Y)) {
    mu[i] <- bIntercept + bX * X[i]
    Y2[i] ~ dorm (mu[i], sY^-2)
  }
} "
  
  expect_warning(y <- jg_check(x))
  expect_false(y)
  expect_warning(jg_check("models{}"), "no model block")
  expect_warning(jg_check("model{} model{}"), "duplicated block names: 'model'")
  expect_warning(jg_check("model{} data{}"), "block order must be: 'data' and 'model'")
  expect_warning(jg_check("model{} data2{}"), "invalid block names: 'data2'")  
  expect_warning(jg_check("model{} predict{}"), "invalid block names: 'predict'")  
  expect_true(jg_check("model{} predict{}", TRUE), "invalid block names: 'predict'")    
  expect_warning(jg_check("predict{} model{}", TRUE), "block order must be: 'data', 'model', 'predict' and 'aggregate'")  
})
