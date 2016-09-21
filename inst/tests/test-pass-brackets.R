context("pass_brackets forward")

test_that("pass_brackets works on paired brackets", {
  expect_equivalent(pass_brackets("()"), 2)
  expect_equivalent(pass_brackets("(())"), 4)
  expect_equivalent(pass_brackets("((({})))"), 8)
  expect_equivalent(pass_brackets("()()"), 2)
  expect_equivalent(pass_brackets("()()", 3), 4)
  expect_equivalent(pass_brackets("([])()"), 4)
  expect_equivalent(pass_brackets("())))))))"), 2)
})

test_that("pass_brackets named by brackets", {
  expect_equivalent(names(pass_brackets("()")), "()")
  expect_equivalent(names(pass_brackets("(())")), "(())")
  expect_equivalent(names(pass_brackets("((({})))")), "((({})))")
  expect_equivalent(names(pass_brackets("()()")), "()")
  expect_equivalent(names(pass_brackets("()()", 3)), "()")
  expect_equivalent(names(pass_brackets("([ \n ])()")), "([ \n ])")
  expect_equivalent(names(pass_brackets("())))))))")), "()")
  expect_equivalent(names(pass_brackets("name[index1[index2[i]]]", 5)), "[index1[index2[i]]]")
  expect_equivalent(names(pass_brackets("name[index1[index2 [ i ]]]", 5)), "[index1[index2 [ i ]]]")
})

test_that("pass_brackets fails on unpaired brackets", {
  expect_error(pass_brackets("("))
  expect_error(pass_brackets(")"))
  expect_error(pass_brackets(""))
  expect_error(pass_brackets(NULL))
  expect_error(pass_brackets("(()"))
  expect_error(pass_brackets("(})"))
  expect_error(pass_brackets("(])"))
  expect_error(pass_brackets("(]"))
})

test_that("pass_brackets fails wrong starting point", {
  expect_error(pass_brackets("x()"))
  expect_error(pass_brackets("x()", 3))
})

context("pass_brackets backwards")

test_that("pass_brackets backwards works on paired brackets", {
  expect_equivalent(pass_brackets("()", forward = FALSE), 1)
  expect_equivalent(pass_brackets("(wet)", forward = FALSE), 1)
  expect_equivalent(pass_brackets("((wet{}))", 7, forward = FALSE), 6)
  expect_equivalent(pass_brackets("((wet{}))", forward = FALSE), 1)
})

test_that("pass_brackets backwards preserves text", {
  expect_identical(names(pass_brackets("(test)", forward = FALSE)), "(test)")
  expect_identical(names(pass_brackets("index[index1[index2[1:3]]]", forward = FALSE)), "[index1[index2[1:3]]]")
})
