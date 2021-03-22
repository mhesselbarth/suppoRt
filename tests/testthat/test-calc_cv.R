context("test-calc_cv")

test_that("calc_cv works for vector", {

  x <- runif(n = 100)

  expect_is(calc_cv(x = x),
              class = "numeric")
})

test_that("calc_cv returns correct value", {

  x <- runif(n = 100)

  cv_man <- sd(x) / mean(x) * 100

  expect_equal(object = calc_cv(x = x), expected = cv_man)
})
