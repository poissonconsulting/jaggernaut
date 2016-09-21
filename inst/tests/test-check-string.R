context("check-string")

test_that("check_string returns same string", {
  expect_identical(check_string("()"), "()")
  expect_identical(check_string("())))))))"), "())))))))")
})

test_that("check_string collapses character string", {
  expect_message(x <- check_string(c("(",")")), "collapsing x into string")
  expect_identical(x, "(\n)")
})

test_that("check_string errors", {
  expect_error(check_string(factor("(")), "x must be class character")
})

