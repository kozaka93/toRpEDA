test_that("Czy wartosc korelacji pearsona jest ok",
          {expect_null(plot_bar_qual(iris))})

test_that("Czy numeryczne wyrzuca error",
          {expect_error(plot_bar_qual(iris,c(1)))})

test_that("Czy numeryczne wyrzuca error",
          {expect_error(plot_bar_qual(mtcars,c(1,3)))})

test_that("Czy dziala dla stringow, jedna kolumna",
          {expect_error(plot_bar_qual(iris,c(5)),NA)})


