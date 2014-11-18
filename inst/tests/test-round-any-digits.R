context("round_any_digits")

test_that("round_any_digits", {
  
  expect_equal(round_any_digits(0.11111, -1), 0)
  expect_equal(round_any_digits(0.11111), 0)
  expect_equal(round_any_digits(0.11111, 1), 0.1)
  expect_equal(round_any_digits(0.11111, 2), 0.11)

  expect_equal(round_any_digits(-0.11111, -1), 0)
  expect_equal(round_any_digits(-0.11111), 0)
  expect_equal(round_any_digits(-0.11111, 1), -0.1)
  expect_equal(round_any_digits(-0.11111, 2), -0.11)

  expect_equal(round_any_digits(0.88888, -1), 0)
  expect_equal(round_any_digits(0.88888, -1.99), 0)
  expect_equal(round_any_digits(0.88888), 1)
  expect_equal(round_any_digits(0.88888, 1), 0.9)
  expect_equal(round_any_digits(0.88888, 2), 0.89)
  expect_equal(round_any_digits(0.88888, 1.5), 0.9)
  expect_equal(round_any_digits(0.88888, 1.99), 0.9)
  
  expect_equal(round_any_digits(-0.88888, -1), 0)
  expect_equal(round_any_digits(-0.88888), -1)
  expect_equal(round_any_digits(-0.88888), -1)
  expect_equal(round_any_digits(-0.88888, 1), -0.9)
  expect_equal(round_any_digits(-0.88888, 2), -0.89)

  expect_equal(round_any_digits(0.1, 0, ceiling), 1)
  expect_equal(round_any_digits(0.1, 0, floor), 0)
  expect_equal(round_any_digits(-0.1, 0, ceiling), 0)
  expect_equal(round_any_digits(-0.1, 0, floor), -1)  
})
