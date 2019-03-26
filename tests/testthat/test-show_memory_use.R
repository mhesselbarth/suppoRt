context("test-show_memory_use")

# test_that("show_memory_use returns returns data frame", {
#
#   vec <- 1:100
#   df <- data.frame(1:5000)
#   mat <- matrix(c(1:100), ncol = 10)
#
#   result <- show_memory_use()
#
#   expect_equal(nrow(result), expected = 3)
#   expect_is(result, "data.frame")
# })

test_that("show_memory_use returns warning", {

  expect_error(show_memory_use(),
               regexp = "No objects in environment.")
})
