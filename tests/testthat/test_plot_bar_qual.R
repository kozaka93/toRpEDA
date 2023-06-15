test_that("Does it return a NULL",
          {expect_null(plot_bar_qual(iris))})

test_that("Numeric return error",
          {expect_error(plot_bar_qual(iris,c(1)))})

test_that("Numeric return error",
          {expect_error(plot_bar_qual(mtcars,c(1,3)))})

test_that("Does it work for one column",
          {expect_error(plot_bar_qual(iris,c(5)),NA)})


