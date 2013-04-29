context("jags_analysis")

test_that("analysis returns object of correct class", {
  
  model <- jags_model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  data <- data.frame(x = rpois(100,1))
  an <- jags_analysis (model, data, mode = "test")  
  expect_that(an, is_a("jags_analysis"))
})

test_that("checks input", {
   
   model <- jags_model(" model { 
       bLambda ~ dlnorm(0,10^-2) 
       for (i in 1:nrow) { 
         x[i]~dpois(bLambda) 
       } 
     }")
   
   data <- data.frame(x = rpois(100,1))
   
   expect_that(jags_analysis (model, data, mode = "test"), is_a("jags_analysis"))
   expect_that(jags_analysis (model = NULL, data = data), throws_error())
   expect_that(jags_analysis (model = FALSE, data = data), throws_error())
   expect_that(jags_analysis (model = data, data = data), throws_error())
   expect_that(jags_analysis (model = 1, data = data), throws_error())
   expect_that(jags_analysis (model, data = NULL), throws_error())
   expect_that(jags_analysis (model, data = FALSE), throws_error())
   expect_that(jags_analysis (model, data = 1), throws_error())
   expect_that(jags_analysis (model, data, niter = NULL), throws_error())
   expect_that(jags_analysis (model, data, niter = FALSE), throws_error())
   expect_that(jags_analysis (model, data, niter = c(100,100)), throws_error())
   expect_that(jags_analysis (model, data, niter = 99), throws_error())
   expect_that(jags_analysis (model, data, niter = 10^6+1), throws_error())
   expect_that(jags_analysis (model, data, niter = 100, mode = "test"), is_a("jags_analysis"))
   expect_that(jags_analysis (model, data, niter = 100.1, mode = "test"), is_a("jags_analysis"))
   expect_that(jags_analysis (model, data, mode = "unknown"), throws_error())
})


test_that("select check", {
  
  code <- " model { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:nrow) { 
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
  data$nrow <- length(data$x)
  mod <- jags_model(code, select = c("x","nrow")) 
  expect_that(jags_analysis (mod, data, mode = "test"), is_a("jags_analysis"))
  mod <- jags_model(code, select = c("y","nrow")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  mod <- jags_model(code, select = c("y","x","nrow")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())  
})

test_that("select check transform", {
  
  code <- " model { 
  bLambda ~ dlnorm(0,10^-2) 
  for (i in 1:nrow) { 
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
  data$nrow <- length(data$x)
  mod <- jags_model(code, select = c("trans(x)","nrow")) 
  expect_that(jags_analysis (mod, data, mode = "test"), is_a("jags_analysis"))
  mod <- jags_model(code, select = c("trans(y)","nrow")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())
  mod <- jags_model(code, select = c("trans(y)","trans(x)","nrow")) 
  expect_that(jags_analysis (mod, data, mode = "test"), throws_error())  
})
