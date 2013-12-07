context("jags_data_model")

test_that("jags_data_model returns object of correct class", {
  
  model <- jags_data_model(" data { 
                      bLambda ~ dlnorm(0,10^-2) 
                      for (i in 1:length(x)) { 
                      x[i]~dpois(bLambda) 
                      } 
}")
  
  expect_that(model, is_a("jagr_model"))
  expect_that(model, is_a("jags_data_model"))
  expect_equal(nmodels(model), 1)
  
  expect_that(model_code(model), is_a("character"))
  expect_that(length(model_code(model)), is_equivalent_to(1))
  
  expect_that(monitor(model), equals(NULL))
  expect_that(select(model), equals(NULL))
  expect_that(modify_data(model), equals(NULL))
  expect_that(gen_inits(model), equals(NULL))
  expect_that(extract_data(model), equals(NULL))
  
  expect_that(derived_code(model), throws_error())
  expect_that(random_effects(model), throws_error())
  
  monitor(model) <- c("bLambda")
  expect_that(monitor(model), equals("bLambda"))

  select(model) <- c("x")
  expect_that(select(model), equals("x"))

  modify_data(model) <-function (data) data
  expect_that(modify_data(model), is_a("function"))

  gen_inits(model) <-function (data) list()
  expect_that(gen_inits(model), is_a("function"))

  extract_data(model) <- function (data) data
  expect_that(extract_data(model), is_a("function"))
  
  expect_that(combine(model,model), throws_error())
  expect_that(subset_jags(model,1), throws_error())
  expect_that(update_jags(model), throws_error())
  expect_that(model(model), throws_error())
})
