context("names_select")

test_that("names_select works", {
  
  expect_that(names_select("test"), is_a("character"))
  expect_equal(names_select("test"), "test")
  expect_equal(names_select("test*"), "test")
  expect_equal(names_select("log(test)*"), "test")
  expect_equal(names_select(c("log(test1)*","unknown(test2)")), c("test1","test2"))
})
