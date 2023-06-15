test_that("errors and warnings work properly - types of inputs", {
  expect_error(normalise())
  expect_error(normalise(c(1,2,3)))
  expect_error(normalise(iris, variables = c(1,2,3)))
  expect_error(normalise(iris, variables = c()))
  expect_error(normalise(iris, mode = "Goblin"))
  expect_error(normalise(data.frame()))
  expect_error(normalise(iris, variables = c("Flowers", "Species")))

  expect_warning(normalise(iris, variables = c("Flowers", "Sepal.Width")))
})

test_that("errors and warnings work properly - characteristics of variables", {
  # categorical warning
  expect_warning(normalise(mtcars, variables = "vs"))
  expect_message(normalise(mtcars, variables = "carb"))

  # constant variable warning
  df <- data.frame(const_col = 1, some_col = seq(1, 10, 0.5))
  expect_warning(normalise(df))
  expect_warning(normalise(df, mode = "interval"))
})

test_that("normalisation works properly", {
  library(stats)
  expect_equal(normalise(iris, variables = c("Petal.Length", "Petal.Width")),
               apply(iris[,c(3,4)], 2, function(x) (x - mean(x))/sd(x)))
  expect_equal(normalise(iris, variables = c("Petal.Length", "Petal.Width"),
                         mode = 'interval'),
               apply(iris[,c(3,4)], 2, function(x) (x - min(x))/
                       (max(x) - min(x))))
})
