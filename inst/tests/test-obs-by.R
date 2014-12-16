context("obs_by")

test_that("obs_by", {
  
  data <- data.frame(Time = 1:10)
  data$Period <- factor("Before", levels = c("Before", "After"))
  data$Period[data$Time >= 6] <- "After"
  data$Time <- factor(data$Time)
  
  data <- rbind(data, data)
  data$Response <- NA
  for(i in 1:nrow(data)) {
    data$Response[i] <- as.integer(data$Period[i]) + rnorm(1, 0, 1)
  }
  
  model <- jags_model(" model { 
    bResponse ~ dnorm(0, 1^-2)

    bPeriod[1] <- 0
    for(i in 2:nPeriod) {
      bPeriod[i] ~ dnorm(0, 1^-2)
    }

    sTime ~ dunif(0, 1)
    for(i in 1:nTime) {
      bTime[i] ~ dnorm(0, sTime^-2)
    }
    
    sResponse ~ dunif(0, 1)
    for(i in 1:length(Response)) {
      eResponse[i] <- bResponse + bPeriod[Period[i]] + bTime[Time[i]]
      Response[i] ~ dnorm(eResponse[i], sResponse^-2)
    }
}",
derived_code = "data{
    for(i in 1:length(Response)) {
      prediction[i] <- bResponse + bPeriod[Period[i]] + bTime[Time[i]]
    }
}")
  
  analysis <- jags_analysis (model, data, mode = "test")

  predict(analysis, newdata = c("Period", "Time"), obs_by = TRUE)

  obs_no <- predict(analysis, newdata = c("Period", "Time"))
  obs_yes <- predict(analysis, newdata = c("Period", "Time"), obs_by = TRUE)
  
  expect_equal(nrow(predict(analysis, newdata = c("Period", "Time"))), 20)
  expect_equal(nrow(predict(analysis, newdata = c("Period", "Time"), obs_by = TRUE)), 10)
  expect_equal(nrow(predict(analysis, newdata = c("Period", "Time"), obs_by = c("Period", "Time"))), 10)

  expect_equal(nrow(predict(analysis, newdata = "Period", obs_by = c("Period", "Time"))), 1)
})

