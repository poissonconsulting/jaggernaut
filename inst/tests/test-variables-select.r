context("variables_select")

test_that("variables_select works", {
  
  expect_that(variables_select("test"), is_a("character"))
  expect_that(variables_select("test"), equals("test"))
  expect_that(variables_select("test*"), equals("test"))
  expect_that(variables_select("log(test)*"), equals("test"))
  expect_that(variables_select(c("log(test1)*","unknown(test2)")), equals(c("test1","test2")))
})
