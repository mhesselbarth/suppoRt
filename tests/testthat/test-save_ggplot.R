context("test-save_ggplot")

vec <- data.frame(x = 1:5, y = 6:10)

gg_example <- ggplot(data = vec) +
  geom_point(aes(x = x, y = y))

test_that("save_ggplot works for filename and pach specified ", {

  save_ggplot(plot = gg_example,
              path = test_path(),
              filename = "gg_example.png")

  check <- "gg_example.png" %in% list.files(test_path())

  expect_true(check)

  file.remove(paste0(test_path(), "/gg_example.png"))
})

test_that("save_ggplot works does not overwrite a existing file ", {

  save_ggplot(plot = gg_example,
              path = test_path(),
              filename = "gg_example.png")

  expect_warning(save_ggplot(plot = gg_example,
                           filename = "gg_example.png",
                           path = test_path(),
                           overwrite = FALSE),
               regexp = "Existing ggplot not overwriten")

  file.remove(paste0(test_path(), "/gg_example.png"))
})


test_that("save_ggplot can overwrite object ", {

  save_ggplot(plot = gg_example,
              path = test_path(),
              filename = "gg_example.png")

  expect_message(save_ggplot(plot = gg_example,
                             path = test_path(),
                             filename = "gg_example.png",
                             overwrite = TRUE),
                 regexp =  "> Existing ggplot overwriten")

  file.remove(paste0(test_path(), "/gg_example.png"))
})

test_that("save_ggplot assigns name ", {

  save_ggplot(plot = gg_example,
              path = test_path())

  check <- "R_plot.jpeg" %in% list.files(test_path())

  expect_true(check)

  file.remove(paste0(test_path(), "/R_plot.jpeg"))
})
