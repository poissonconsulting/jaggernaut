context("round_up")

test_that("round_up", {
  
  expect_equal(round_up(0.111111, 0), 1)
  expect_equal(round_up(0.111111, 1), 0.2)
  expect_equal(round_up(0.111111, 2), 0.12)
  expect_equal(round_up(0.111111, 3), 0.112)
  expect_equal(round_up(0.111111, 1.1), 0.12)
  expect_equal(round_up(0.111111, 1.5), 0.12)
})
