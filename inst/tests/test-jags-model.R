context("jags_model")

test_that("jags_model returns object of correct class", {
  
  model <- jags_model(" model { 
      bLambda ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x[i]~dpois(bLambda) 
      } 
    }")
  
  expect_that(model, is_a("jags_model"))
  expect_equal(nmodels(model), 1)
  expect_true(is_one_model(model))
  
  expect_that(model_code(model), is_a("character"))
  expect_that(names(model_code(model)), equals(NULL))
  expect_that(length(model_code(model)), is_equivalent_to(1))
  
  expect_equal(monitor(model), "^([^dei]|.[^A-Z])")
  expect_that(select_data(model), equals(NULL))
  expect_that(modify_data(model), equals(NULL))
  expect_that(gen_inits(model), equals(NULL))
  expect_that(derived_code(model), equals(NULL))
  expect_that(random_effects(model), equals(NULL))
  
  expect_that(extract_data(model), throws_error())
  
  monitor(model) <- c("bLambda")
  expect_that(monitor(model), equals("bLambda"))
  
  select_data(model) <- c("x")
  expect_that(select_data(model), equals("x"))
  
  modify_data(model) <-function (data) data
  expect_that(modify_data(model), is_a("function"))
  
  gen_inits(model) <-function (data) list()
  expect_that(gen_inits(model), is_a("function"))

  derived_code(model) <- "data { for (i in 1:nrow) {prediction[i] <- bLambda} }"
  expect_that(derived_code(model), is_a("character"))
  
  random_effects(model) <- list(bLambda = "x")
  expect_that(random_effects(model), is_a("list"))
  expect_that(names(random_effects(model)), equals("bLambda"))
  
  model2 <- model
  
  model_code(model2) <- " model { 
      bLambda2 ~ dlnorm(0,10^-2) 
      for (i in 1:nrow) { 
        x2[i]~dpois(bLambda) 
      } 
    }"
  
  model <- combine(model,model2)
  
  expect_that(model, is_a("jags_model"))
  expect_equal(nmodels(model), 2)
  expect_false(is_one_model(model))
  
  expect_that(model_code(model), is_a("list"))
  expect_that(names(model_code(model)), equals(c("Model1","Model2")))
  expect_that(length(model_code(model)), is_equivalent_to(2))
  expect_that(model_code(model)[[2]], is_a("character"))
  
  expect_that(monitor(model), is_a("list"))
  expect_that(names(monitor(model)), equals(c("Model1","Model2")))
  expect_that(length(monitor(model)), is_equivalent_to(2))
  expect_that(monitor(model)[[2]], is_a("character"))
  
  expect_that(select_data(model), is_a("list"))
  expect_that(names(select_data(model)), equals(c("Model1","Model2")))
  expect_that(length(select_data(model)), is_equivalent_to(2))
  expect_that(select_data(model)[[2]], is_a("character"))
  
  expect_that(modify_data(model), is_a("list"))
  expect_that(names(modify_data(model)), equals(c("Model1","Model2")))
  expect_that(length(modify_data(model)), is_equivalent_to(2))
  expect_that(modify_data(model)[[2]], is_a("function"))
  
  expect_that(gen_inits(model), is_a("list"))
  expect_that(names(gen_inits(model)), equals(c("Model1","Model2")))
  expect_that(length(gen_inits(model)), is_equivalent_to(2))
  expect_that(gen_inits(model)[[2]], is_a("function"))
  
  expect_that(derived_code(model), is_a("list"))
  expect_that(names(derived_code(model)), equals(c("Model1","Model2")))
  expect_that(length(derived_code(model)), is_equivalent_to(2))
  expect_that(derived_code(model)[[2]], is_a("character"))
  
  expect_that(random_effects(model), is_a("list"))
  expect_that(names(random_effects(model)), equals(c("Model1","Model2")))
  expect_that(length(random_effects(model)), is_equivalent_to(2))
  expect_that(random_effects(model)[[2]], is_a("list"))
  
  model3 <- combine(model2, model2)
  model_code(model3) <- model_code(model)
  select_data(model3) <- select_data(model)
  monitor(model3) <- monitor(model)
  gen_inits(model3) <- gen_inits(model)
  modify_data_derived(model3) <- modify_data_derived(model)
  derived_code(model3) <- derived_code(model)
  random_effects(model3) <- random_effects(model)
  
  expect_true(identical(model_code(model3), model_code(model)))
  expect_true(identical(select_data(model3), select_data(model)))
  expect_true(identical(monitor(model3), monitor(model)))
  expect_true(identical(gen_inits(model3), gen_inits(model)))
  expect_true(identical(modify_data_derived(model3), modify_data_derived(model)))
  expect_true(identical(derived_code(model3), derived_code(model)))
  expect_true(identical(random_effects(model3), random_effects(model)))
  expect_true(identical(modify_data_aggregation(model3), modify_data_aggregation(model)))
  expect_true(identical(select_data_aggregation(model3), select_data_aggregation(model)))
  expect_true(identical(select_data_derived(model3), select_data_derived(model)))
  
  model <- subset(model,model = 2)
  expect_that(model, is_a("jags_model"))
  expect_equal(nmodels(model), 1)
  
  expect_true(identical(model_code(model2), model_code(model)))
  expect_true(identical(select_data(model2), select_data(model)))
  expect_true(identical(monitor(model2), monitor(model)))
  expect_true(identical(gen_inits(model2), gen_inits(model)))
  expect_true(identical(modify_data_derived(model2), modify_data_derived(model)))
  expect_true(identical(derived_code(model2), derived_code(model)))
  expect_true(identical(random_effects(model2), random_effects(model)))
  expect_true(identical(modify_data_aggregation(model2), modify_data_aggregation(model)))
  expect_true(identical(select_data_aggregation(model2), select_data_aggregation(model)))
  expect_true(identical(select_data_derived(model2), select_data_derived(model)))
  
  expect_that(model(model), is_a("jagr_model"))
  
  model <- subset(model, model = c("Model1"))
  expect_that(model, is_a("jags_model"))
  expect_equal(nmodels(model), 1)  
  
  expect_that(update_jags(model), throws_error())
  expect_that(dataset(model), throws_error())
})
