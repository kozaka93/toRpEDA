test_that("check arguments",{
  my_data <- data.frame(
    target = c("R", "Python", "R", "R", "Python", "Python"),
    a = c(1, 2, 3, 4, 2, 2),
    b = c("a", "b", "c", "b", "c", "c"),
    c = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
    d = c("dog ", "cat", "dog", "dog", "dog", "cat"),
    e = c("PL", "PL", "PL", "USA", "USA", "USA"),
    f = c("Europa", "Europa", "America", "Europa", "America", "Europa")
  )
  expect_error(plot_bar_plot(c(1,2,3)))
  expect_error(plot_bar_plot(my_data, 12))
  expect_error(plot_bar_plot(my_data, "target", col_number = "one"))
  expect_error(plot_bar_plot(my_data, "notcolumn"))
  expect_error(plot_bar_plot(my_data, "target", variables = c("b", "z")))
  expect_error(plot_bar_plot(my_data, "a"))
  expect_error(plot_bar_plot(my_data, "target", variables = c("a", "d")))
  expect_error(plot_bar_plot(my_data[1:2], "target"))
})

test_that("function works", {
  my_data <- data.frame(
    target = c("R", "Python", "R", "R", "Python", "Python"),
    a = c(1, 2, 3, 4, 2, 2),
    b = c("a", "b", "c", "b", "c", "c"),
    c = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
    d = c("dog ", "cat", "dog", "dog", "dog", "cat"),
    e = c("PL", "PL", "PL", "USA", "USA", "USA"),
    f = c("Europa", "Europa", "America", "Europa", "America", "Europa")
  )
  expect_type(plot_bar_plot(my_data, "target"), "list")
})
