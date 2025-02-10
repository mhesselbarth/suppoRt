context("test-show_temp")

test_that("show_temp() finds temporary files", {

  p <- tempfile("test", fileext = ".rds")
  x <- 5
  saveRDS(object = x, file = p)

  y <- show_temp()
  expect_gte(object = length(y), expected = 1)

  show_temp(remove = TRUE)
  y <- show_temp()
  expect_equal(object = length(y), expected = 0)

})

test_that("show_temp() removes temporary files", {

  show_temp(remove = TRUE)
  y <- show_temp()
  expect_equal(object = length(y), expected = 0)

})
