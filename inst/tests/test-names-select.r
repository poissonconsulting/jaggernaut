context("names_select")

test_that("names_select works", {
  
  expect_that(names_select("test"), is_a("character"))
  expect_that(names_select("test"), equals("test"))
  expect_that(names_select("test*"), equals("test"))
  expect_that(names_select("log(test)*"), equals("test"))
  expect_that(names_select(c("log(test1)*","unknown(test2)")), equals(c("test1","test2")))
})
