context("test-show_memory_use")

vec_a <- 1:100
vec_b <- 101:1000

mat <- matrix(c(1:1000), ncol = 20)
df <- data.frame(x = 1:10)

test_that("show_memory_use works if list is provided", {

  result <- show_memory_use(what = list("vec_a" = vec_a, "vec_b" = vec_b, "df" = df, "mat" = mat))

  expect_equal(nrow(result), expected = 4)
  expect_is(result, "data.frame")
  expect_true(all(result$name %in% c("vec_a", "vec_b", "df", "mat")))
  expect_true(all(diff(result$size) <= 0))
})

test_that("show_memory_use works if list with no names is provided", {

  result <- show_memory_use(what = list(vec_a, vec_b, df, mat))

  expect_equal(nrow(result), expected = 4)
  expect_is(result, "data.frame")
  expect_true(all(result$name %in% c("object_01", "object_02", "object_03", "object_04")))
})

test_that("show_memory_use can sort increasing", {

  result <- show_memory_use(what = list(vec_a, vec_b, df, mat),
                            decreasing = FALSE)

  expect_true(all(diff(result$size) >= 0))
})

test_that("show_memory_use can sort alphabetical", {

  result <- show_memory_use(what = list("vec_a" = vec_a, "vec_b" = vec_b, "df" = df, "mat" = mat),
                            sort = "name",
                            decreasing = FALSE)

  expect_true(all(result$name == c("df", "mat", "vec_a", "vec_b")))
})

test_that("show_memory_use only returns n rows", {

  result <- show_memory_use(what = list("vec_a" = vec_a, "vec_b" = vec_b, "df" = df, "mat" = mat),
                            n = 2)

  expect_equal(nrow(result), expected = 2)

})

test_that("show_memory_use returns warnings and errors", {

  expect_error(show_memory_use(what = list()),
               regexp = "No objects in environment.")

  expect_warning(show_memory_use(what = list("vec_a" = vec_a, "vec_b" = vec_b, "df" = df, "mat" = mat),
                                 sort = "wrong"),
                 regexp = "sort argument unkown - using size")
})
