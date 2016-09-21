context("reverse-brackets works")

test_that("reverse_brackets does all brackets", {
  expect_identical(reverse_brackets("()"), ")(")
  expect_identical(reverse_brackets("()[]{}"), ")(][}{")
  expect_identical(reverse_brackets("{([}])"), "})]{[(")
})

test_that("reverse_brackets doesn't mess with text", {
  expect_identical(reverse_brackets("(123)"), ")123(")
  expect_identical(reverse_brackets("0(123)4"), "0)123(4")
})

test_that("reverse_brackets works vectors", {
  expect_identical(reverse_brackets(c("()","{")), c(")(","}"))
})
