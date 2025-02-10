context("test-split_at")

test_that("split_at() returns correct lists", {

  x <- c(1, 2, 2, 2, 3, 3, 3, 3)

  y <- split_at(x, pos = c(2, 5))

  expect_length(object = y, n = 3)

  expect_equal(object = y[[1]], expected = 1)
  expect_equal(object = y[[2]], expected = c(2, 2, 2))
  expect_equal(object = y[[3]], expected = c(3, 3, 3, 3))

})
