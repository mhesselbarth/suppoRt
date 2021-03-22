context("test-as_data_table")

ras <- raster::raster(nrow = 10, ncol = 10)

vals <- sample(x = c(1, 2, 3), size = 100, replace = TRUE)

raster::values(ras) <- vals


test_that("as_data_table returns data.table or data.frame", {

  x_dt <- as_data_table(x = ras, return_df = FALSE)

  x_df <- as_data_table(x = ras, return_df = TRUE)

  expect_is(object = x_dt, class = "data.table")

  expect_is(object = x_df, class = "data.frame")

})

test_that("data.frame has all values", {

  x_df <- as_data_table(x = ras, return_df = TRUE)

  expect_equal(object = x_df$layer_1, expected = vals)

})
