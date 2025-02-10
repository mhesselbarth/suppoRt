context("test-expand_grid_unique")

x <- 1:5
y <- 1:5

full_df <- expand.grid(x,y)

test_that("expand_grid_unique returns only unique combinations", {

  unique_df <- expand_grid_unique(x, y)

  expect_lt(nrow(unique_df), nrow(full_df))
  expect_equal(object = sum(unique_df[, 1] == unique_df[, 2]), expected = 0)

})

test_that("expand_grid_unique returns equal combinations", {

  unique_df <- expand_grid_unique(x, y, equals = TRUE)

  expect_lt(nrow(unique_df), nrow(full_df))
  expect_equal(object = sum(unique_df[, 1] == unique_df[, 2]), expected = 5)

})

