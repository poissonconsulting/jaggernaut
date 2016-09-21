context("paste-names")

test_that("paste_names works", {
  expect_identical(paste_names(NULL), "''")
  expect_identical(paste_names(character(0)), "''")
  expect_identical(paste_names(1), "'1'")
  expect_identical(paste_names(1:2), "'1' and '2'")
  expect_identical(paste_names(1:3), "'1', '2' and '3'")
})
