library(testthat)
library(ggplot2)
library(patchwork)

test_that("Function works correctly for NULL vars", {
  data(mtcars)
  plot_hist_vars(mtcars)
})

test_that("Function works correctly for given vars", {
  data(mtcars)
  plot_hist_vars(mtcars, c("mpg", "cyl"))
})

test_that("Function returns the warning of incorrect vars", {
  data(iris)
  expect_warning(plot_hist_vars(iris, c("Petal.Length", "Species")))
})

test_that("Function returns the warning for none numeric vars specified only", {
  data(iris)
  expect_warning(plot_hist_vars(iris, c("Species")))
})

test_that("Function works correctly for different values of plots_per_page", {
  data(mtcars)
  expect_output(plot_hist_vars(mtcars, NULL, 3), "plot_layout")
  expect_output(plot_hist_vars(mtcars, NULL, 2), "plot_layout")
})

