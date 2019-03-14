context("test-is_real")

test_that("is_real works for NaN and Inf", {

  x <- c(1, 2, 3, NA, 5, NaN, Inf)

  expect_equal(is_real(x),
               expected = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))

  y <- c(1, 2, 3, 4, 5, 6, 7)

  expect_true(all(is_real(y)))
})

test_that("is_real works for NaN, Inf and NA", {

  x <- c(1, 2, 3, NA, 5, NaN, Inf)

  expect_equal(is_real(x, include_na = TRUE),
               expected = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

  y <- c(1, 2, 3, 4, 5, 6, NA)

  expect_false(all(is_real(y, include_na = TRUE)))
})


test_that("is_real returns errors", {

  x <- matrix(c(1, 2, 3, NA, 5, NaN), ncol = 2)

  expect_error(is_real(x),
               regexp = "input must be a vector.")

  y <- c("A", "B", "C", NA, "E", NaN, Inf)

  expect_error(is_real(y),
               regexp = "input must be of class 'numeric'.")
})
