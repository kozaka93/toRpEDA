
test_that('errors',{
  expect_error(plot_num_plots(c(2,3), target = ""))
  expect_error(plot_num_plots(mtcars, variables = 1.34, target = "gear"))
  expect_error(plot_num_plots(mtcars, variables = c("ht", "hp"), target="targ"))
  expect_error(plot_num_plots(mtcars, target="mpg"))
})



test_that('messages',{
  expect_message(plot_num_plots(mtcars, variables = c("wt", "mpg"), target = "gear", plot_type="b"))
  expect_message(plot_num_plots(mtcars, variables = c("wt", "mpg", "lol"), target = "gear",
                                plot_type="violin"))
})



test_that('result type and length', {
expect_type(plot_num_plots(mtcars,
                           target = "gear", plot_type="boxplot"), "list")
  expect_type(plot_num_plots(iris,
                             target = "Species", plot_type="violin"), "list")
  expect_length( plot_num_plots(mtcars, variables = c("wt", "disp", "mpg", "qsec", "drat"),
                 target = "gear", plot_type="boxplot"), 2)

})
