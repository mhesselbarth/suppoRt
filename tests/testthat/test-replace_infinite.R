context("test-replace_infinite")

data <- c(1, 2, 3, NaN, 5, Inf)

test_that("replace_infinite replaces all Inf and NaN values", {

  result <- replace_infinite(x = data, verbose = FALSE)

  expect_true(all(!is.infinite(result)))
  expect_true(all(!is.nan(result)))
})

test_that("replace_infinite uses replace values", {

  data <- c(1, 2, 3, NaN, 5, Inf)

  result <- replace_infinite(x = data, value = c(-1, -2), verbose = FALSE)

  expect_true(any(result == -1))
  expect_true(any(result == -2))
})

test_that("replace_infinite only replace one value replace values", {

  data <- c(1, 2, 3, NaN, 5, Inf)

  result <- replace_infinite(x = data, what = "Inf", value =  -999, verbose = FALSE)

  expect_true(any(result == -999))
  expect_true(any(is.nan(result)))
})

test_that("replace_infinite returns errors", {

  data <- matrix(c(1, 2, 3, NaN, 5, Inf), ncol = 2)

  expect_error(replace_infinite(x = data, verbose = FALSE),
               regexp = "input must be a vector.")

  data <- c("A", "B", "C")

  expect_error(replace_infinite(x = data, verbose = FALSE),
               regexp = "input must be of class 'numeric'.")
})
