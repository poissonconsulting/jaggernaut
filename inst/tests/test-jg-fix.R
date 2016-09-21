context("jg-fix")

test_that("jg-fix works", {
  
  expect_identical(jg_fix("model{} data{} models{}"), "data {} model {}")
  expect_identical(jg_fix("predict{} data{} model{}"), "data {} model {}")
  expect_identical(jg_fix("predict{} data{} model{}", TRUE), "data {} model {} predict {}")
})

test_that("jg-fix returns FALSE", {
  
#  expect_false(jg_fix("model{{}"))
})
