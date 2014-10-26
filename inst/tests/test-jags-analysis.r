context("jags_analysis")

test_that("analysis returns object of correct class", {
  
  model <- jags_model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:length(x)) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  data <- data.frame(x = rpois(100,1))
  analysis <- jags_analysis (model, data, mode = "test")  
  expect_that(analysis, is_a("jags_analysis"))
  expect_equal(nmodels(analysis), 1)
  expect_that(as.jags_model(analysis),is_a("jags_model"))
  expect_equal(model_code(analysis), model_code(model))
})

test_that("checks input", {
   
   model <- jags_model(" model { 
       bLambda ~ dlnorm(0,10^-2) 
       for (i in 1:length(x)) { 
         x[i]~dpois(bLambda) 
       } 
     }")
   
   data <- data.frame(x = rpois(100,1))
   
   expect_that(jags_analysis (model, data, mode = "test"), is_a("jags_analysis"))
   expect_that(jags_analysis (model = NULL, data = data, mode = "test"), throws_error())
   expect_that(jags_analysis (model = FALSE, data = data, mode = "test"), throws_error())
   expect_that(jags_analysis (model = data, data = data, mode = "test"), throws_error())
   expect_that(jags_analysis (model = 1, data = data, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data = NULL, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data = FALSE, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data = 1, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data, niters = NULL, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data, niters = FALSE, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data, niters = c(100,100), mode = "test"), throws_error())
   expect_that(jags_analysis (model, data, niters = 99, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data, niters = 10^6+1, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data, niters = 100, mode = "test"), is_a("jags_analysis"))
   expect_that(jags_analysis (model, data, niters = 100.1, mode = "test"), throws_error())
   expect_that(jags_analysis (model, data, mode = "unknown"), throws_error())
})


test_that("select check", {
  
  code <- " model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}"
  
  data <- data.frame(x = rpois(100,1))
  mod <- jags_model(code, select = "x") 
  expect_that(jags_analysis (mod, data, mode = "test"), is_a("jags_analysis"))
  mod <- jags_model(code, select = "y") 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  mod <- jags_model(code, select = c("y","x")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  
  data <- as.list(data)
  mod <- jags_model(code, select = c("x")) 
  expect_that(jags_analysis (mod, data, mode = "test"), is_a("jags_analysis"))
  mod <- jags_model(code, select = c("y")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  mod <- jags_model(code, select = c("y","x")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())  
})

test_that("select check transform", {
  
  code <- " model { 
  bLambda ~ dlnorm(0,10^-2) 
  for (i in 1:length(x)) { 
  x[i]~dpois(bLambda) 
  } 
}"
  
  trans <<- function (x) {
    return (x + 1)
  }
  data <- data.frame(x = rpois(100,1))
  mod <- jags_model(code, select = "x") 
  expect_that(jags_analysis (mod, data, mode = "test"), is_a("jags_analysis"))
  mod <- jags_model(code, select = "trans(y)") 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  mod <- jags_model(code, select = c("trans(y)","trans(x)")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  
  data <- as.list(data)
  mod <- jags_model(code, select = c("trans(x)")) 
  expect_that(jags_analysis (mod, data, mode = "test"), is_a("jags_analysis"))
  mod <- jags_model(code, select = c("trans(y)")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  mod <- jags_model(code, select = c("trans(y)","trans(x)")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())  
})
