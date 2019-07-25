context("test-save_rds")

vec <- data.frame(x = 1:5, y = 6:10)

test_that("save_rds works for filename and path specified ", {

  save_rds(object = vec,
           path = test_path(),
           filename = "test_vector.rds")

  check <- "test_vector.rds" %in% list.files(test_path())

  expect_true(check)

  file.remove(paste0(test_path(), "/test_vector.rds"))
})

test_that("save_rds works does not overwrite a existing file ", {

  save_rds(object = vec,
           path = test_path(),
           filename = "test_vector.rds")

  expect_warning(save_rds(object = vec,
                        filename = "test_vector.rds",
                        path = test_path(),
                        overwrite = FALSE),
               regexp = "Existing file not overwriten")

  file.remove(paste0(test_path(), "/test_vector.rds"))
})


test_that("save_rds can overwrite object ", {

  save_rds(object = vec,
           path = test_path(),
           filename = "test_vector.rds")

  expect_message(save_rds(object = vec,
                          filename = "test_vector.rds",
                          path = test_path(),
                          overwrite = TRUE),
           regexp =  "> Existing file overwriten")

  file.remove(paste0(test_path(), "/test_vector.rds"))
})

test_that("save_rds assigns name ", {

  save_rds(object = vec,
           path = test_path())

  check <- "rds_file.rds" %in% list.files(test_path())

  expect_true(check)

  file.remove(paste0(test_path(), "/rds_file.rds"))
})
