test_that("Function works correctly for NULL vars", {
  data(mtcars)
  expect_no_error(plot_hist_vars(mtcars))
})

test_that("Function works correctly for given vars", {
  data(mtcars)
  expect_no_error(plot_hist_vars(mtcars, c("mpg", "cyl")))
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
  list1 <- plot_hist_vars(mtcars, plots_per_page = 3)
  list2 <- plot_hist_vars(mtcars, plots_per_page = 2)
  expect_length(list1, 11)
  expect_length(list2, 11)
})

