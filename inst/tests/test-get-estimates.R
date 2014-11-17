context("get-estimates")

test_that("bayesian_p_value", {
  
  level <- 0.95
  estimate <- "median"

  expect_equal(names(get_estimates(x = seq(1,100,length.out = 1), level = level, estimate = estimate)), c("estimate", "lower", "upper", "sd", "error", "significance"))
  
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1), level = level, estimate = estimate), c(1,1,1,NA,0,1))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10), level = level, estimate = estimate), c(50,3,98,33,93,0.1)) 
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 100), level = level, estimate = estimate), c(50.5,3.5,97.5,29,93,0.01))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1000), level = level, estimate = estimate), c(50.5,3.48,97.52,28.62,93,0.001))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10000), level = level, estimate = estimate), c(50.5,3.475,97.525,28.5830,93,0.0001))
  
  estimate <- "mean"
  
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1), level = level, estimate = estimate), c(1,1,1,NA,0,1))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10), level = level, estimate = estimate), c(50,3,98,33,93,0.1)) 
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 100), level = level, estimate = estimate), c(50.5,3.5,97.5,29,93,0.01))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1000), level = level, estimate = estimate), c(50.5,3.48,97.52,28.62,93,0.001))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10000), level = level, estimate = estimate), c(50.5,3.475,97.525,28.5830,93,0.0001))

  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1) + 10000, level = level, estimate = estimate), c(10001,10001,10001,NA,0,1))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10) + 10000, level = level, estimate = estimate), c(10050,10003,10098,33,0,0.1)) 
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 100) + 10000, level = level, estimate = estimate), c(10050.5,10003.5,10097.5,29,0,0.01))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1000) + 10000, level = level, estimate = estimate), c(10050.5,10003.48,10097.52,28.62,0,0.001))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10000) + 10000, level = level, estimate = estimate), c(10050.5,10003.475,10097.525,28.5830,0,0.0001))


  expect_equivalent(get_estimates(x = seq(1,100,length.out = 998) + 10000, level = level, estimate = estimate)["sd"], 28.6)
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 999) + 10000, level = level, estimate = estimate)["sd"], 28.6)
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1000) + 10000, level = level, estimate = estimate)["sd"], 28.62)
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1001) + 10000, level = level, estimate = estimate)["sd"], 28.62)
  
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1) - 10000, level = level, estimate = estimate), c(-9999,-9999,-9999,NA,0,1))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10) - 10000, level = level, estimate = estimate), c(-9950,-9997,-9902,33,0,0.1)) 
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 100) - 10000, level = level, estimate = estimate), c(-9949.5,-9996.5,-9902.5,29,0,0.01))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1000) - 10000, level = level, estimate = estimate), c(-9949.5,-9996.52,-9902.48,28.62,0,0.001))
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 10000) - 10000, level = level, estimate = estimate), c(-9949.5,-9996.525,-9902.475,28.5830,0,0.0001))
  
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 998) + 10000, level = level, estimate = estimate)["sd"], 28.6)
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 999) + 10000, level = level, estimate = estimate)["sd"], 28.6)
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1000) + 10000, level = level, estimate = estimate)["sd"], 28.62)
  expect_equivalent(get_estimates(x = seq(1,100,length.out = 1001) + 10000, level = level, estimate = estimate)["sd"], 28.62)
  
  expect_equivalent(get_estimates(x = seq(0.0001,0.01,length.out = 1), level = level, estimate = estimate), c(0.0001,0.0001,0.0001,NA,0,1))
  expect_equivalent(get_estimates(x = seq(0.0001,0.01,length.out = 10), level = level, estimate = estimate), c(0.005,0.0003,0.0098,0.0033,93,0.1)) 
  expect_equivalent(get_estimates(x = seq(0.0001,0.01,length.out = 100), level = level, estimate = estimate), c(0.00505,0.00035,0.00975,0.0029,93,0.01))  
})
