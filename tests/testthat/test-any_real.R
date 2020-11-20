context("test-any_real")

test_that("any_real works for NaN and Inf", {

  x <- c(1, 2, 3, NA, 5, NaN, Inf)

  expect_true(any_real(x))

  y <- c(1, 2, 3, 4, 5, 6, 7)

  expect_true(any_real(y))
})

test_that("any_real works for NaN, Inf and NA", {

  x <- c(1, 2, 3, NA, 5, NaN, Inf)

  expect_true(any_real(x, include_na = TRUE))

  y <- c(1, 2, 3, 4, 5, 6, NA)

  expect_true(any_real(y, include_na = TRUE))
})


test_that("any_real returns errors", {

  x <- matrix(c(1, 2, 3, NA, 5, NaN), ncol = 2)

  expect_error(any_real(x),
               regexp = "input must be a vector.")

  y <- c("A", "B", "C", NA, "E", NaN, Inf)

  expect_error(any_real(y),
               regexp = "input must be of class 'numeric'.")
})
