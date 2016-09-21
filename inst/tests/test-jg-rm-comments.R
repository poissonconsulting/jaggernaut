context("rm-comments")

test_that("jg_rm_comments works", {
  expect_identical(jg_rm_comments("x#1\ny#2"), "x\ny")
  expect_identical(jg_rm_comments("x#1\ny#2"), "x\ny")
  expect_identical(jg_rm_comments("#"), "")
  expect_identical(jg_rm_comments("#1#"), "")
  expect_identical(jg_rm_comments("(#)"), "(")
  expect_identical(jg_rm_comments("#\n#1"), "\n")
})
