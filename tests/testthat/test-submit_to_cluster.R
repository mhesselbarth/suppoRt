context("test-submit_to_cluster")

test_that("submit_to_cluster works ", {

  fx = function(x) x * 2

  result <- submit_to_cluster(fx, x = 1:3, n_jobs = 1)

  expect_identical(object = result, expected = list(2,4,6))
})
