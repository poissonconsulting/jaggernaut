context("pass-blocks")

test_that("pass_blocks passes correct blocks", {

  expect_equivalent(pass_blocks("model{}"), "{}")
  expect_equivalent(pass_blocks("model {}"), "{}")
  expect_equivalent(pass_blocks("model\n{}"), "{}")
  expect_equivalent(pass_blocks("model{}data{}"), c("{}","{}"))
  expect_equivalent(pass_blocks("model{} data{}"), c("{}","{}"))
  expect_equivalent(pass_blocks("model{{x <- bt[1:2]}} data{}"), c("{{x <- bt[1:2]}}","{}"))
})

test_that("pass_blocks gets names correct blocks", {
  
  expect_identical(names(pass_blocks("model{}")), "model")
  expect_identical(names(pass_blocks("model{}data{}")), c("model","data"))
  expect_identical(names(pass_blocks("model{} data{}")), c("model","data"))
  expect_identical(names(pass_blocks("settings{}model{} \ndata{}")), c("settings","model","data"))
})

test_that("pass_blocks throws error", {
  
  expect_error(names(pass_blocks("{}")))
  expect_error(names(pass_blocks("data()")))
  expect_error(names(pass_blocks("data{}{}")))
  expect_error(names(pass_blocks("data{{}")))
  expect_error(names(pass_blocks("data{(}")))
  expect_error(names(pass_blocks("data{]}")))
  expect_error(names(pass_blocks("data{}{}")))
  expect_error(names(pass_blocks("data{}1model{}")))
})
