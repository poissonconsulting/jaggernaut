context("block names")

test_that("jg_block_names gets correct names", {
  
  expect_identical(jg_block_names("model {}"), "model")
  expect_identical(jg_block_names("model{}"), "model")
  expect_identical(jg_block_names("model{}data{}"), c("model","data"))
  expect_identical(jg_block_names("data{} model{}"), c("data","model"))
  expect_identical(jg_block_names("model{{x <- bt[1:2]}} data{}"), c("model","data"))
  expect_identical(jg_block_names("settings{} \n\ndata{} model{}"), c("settings","data","model"))
  expect_identical(jg_block_names("data {X <- 2} model { Y ~ }"),c("data","model"))
  expect_identical(jg_block_names("data{X <- 2} model{ Y ~ dpois(X) }"), c("data","model"))
})

test_that("jg_block_names replacement function", {
  
  x <- "\n\ndata{X <- 2}model \n{ Y ~ dpois(X) }  "
  
  jg_block_names(x) <- c("model", "data")
  
  expect_identical(x, "model {X <- 2} data { Y ~ dpois(X) }")
})
