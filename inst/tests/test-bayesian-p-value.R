context("bayesian_p_value")

test_that("bayesian_p_value", {
  
  expect_equal(bayesian_p_value(c(rep(-1,1),rep(1,1))), 1)
  expect_equal(bayesian_p_value(c(rep(-1,10),rep(1,10))), 1)
  expect_equal(bayesian_p_value(c(rep(-1,100),rep(1,100))), 1)
  
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,1))), 1)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,10))), 0.1)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,100))), 0.01)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,1000))), 0.001)
  
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,5))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,50))), 0.02)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,500))), 0.002)
  
  expect_equal(bayesian_p_value(c(rep(-1,1),rep(1,9))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,1),rep(1,99))), 0.02)
  expect_equal(bayesian_p_value(c(rep(-1,1),rep(1,999))), 0.002)
  
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,9))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,99))), 0.02)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,999))), 0.002)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,998))), 0.002)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,100))), 0.01)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,101))), 0.01)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,102))), 0.01)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,103))), 0.01)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,899))), 0.002)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,998))), 0.002)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,999))), 0.002)
  
  expect_equal(bayesian_p_value(c(rep(-1,1),rep(1,2))), 0.7)
  expect_equal(bayesian_p_value(c(rep(-1,10),rep(1,20))), 0.67)
  expect_equal(bayesian_p_value(c(rep(-1,100),rep(1,200))), 0.667)
  expect_equal(bayesian_p_value(c(rep(-1,1000),rep(1,2000))), 0.6667)
  
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,2))), 0.5)  
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,3))), 0.4)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,4))), 0.3)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,5))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,6))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,7))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,8))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,9))), 0.2)
  expect_equal(bayesian_p_value(c(rep(-1,0),rep(1,10))), 0.1)
  
  expect_equal(bayesian_p_value(c(rep(-1,125),rep(1,875))), 0.25)
  
  expect_equal(bayesian_p_value(c(rep(-1,5),rep(1,95))), 0.1)
  expect_equal(bayesian_p_value(c(rep(-1,5),rep(1,995))), 0.01)
  expect_equal(bayesian_p_value(c(rep(-1,5),rep(1,9995))), 0.001)
})
