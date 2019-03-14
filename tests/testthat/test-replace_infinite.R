context("test-replace_infinite")

test_that("replace_infinite replaces all Inf and NaN values", {

  data <- c(1, 2, 3, NaN, 5, Inf)

  result <- replace_infinite(x = data)

  expect_true(all(!is.infinite(result)))
  expect_true(all(!is.nan(result)))
  expect_true(anyNA(result))
})

test_that("replace_infinite replaces all Inf and NaN values with only 1 value", {

  data <- c(1, 2, 3, NaN, 5, Inf)

  result <- replace_infinite(x = data, value = -999)

  expect_true(all(!is.infinite(result)))
  expect_true(all(!is.nan(result)))
  expect_true(any(result == -999))
})

test_that("replace_infinite uses replace values", {

  data <- c(1, 2, 3, NaN, 5, Inf)

  result <- replace_infinite(x = data, value = c(-1, -2))

  expect_true(all(!is.infinite(result)))
  expect_true(all(!is.nan(result)))
  expect_true(any(result == -1))
  expect_true(any(result == -2))
})

test_that("replace_infinite only replace Inf OR NaN", {

  data <- c(1, 2, 3, NaN, 5, Inf)

  result_inf <- replace_infinite(x = data, what = "Inf", value =  -999)

  expect_true(any(result_inf == -999))
  expect_true(any(is.nan(result_inf)))

  result_nan <- replace_infinite(x = data, what = "NaN")

  expect_true(anyNA(result_nan))
  expect_true(any(is.infinite(result_nan)))

})

test_that("replace_infinite returns errors", {

  data <- matrix(c(1, 2, 3, NaN, 5, Inf), ncol = 2)

  expect_error(replace_infinite(x = data),
               regexp = "input must be a vector.")

  data <- c("A", "B", "C")

  expect_error(replace_infinite(x = data),
               regexp = "input must be of class 'numeric'.")

  data <- c(1, 2, 3, NaN, 5, Inf)

  expect_error(replace_infinite(x = data, what = "all"),
               grep = "'what' must be 'NaN', 'Inf' or 'c(NaN, Inf)'.")

})
