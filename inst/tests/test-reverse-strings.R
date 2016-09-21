context("reverse-strings")

test_that("reverse-strings reverses strings", {
  expect_identical(reverse_strings("abc"), "cba")
  expect_identical(reverse_strings(c("abc","def")), c("cba","fed"))
  expect_identical(reverse_strings(c("ab c","def")), c("c ba","fed"))
})
