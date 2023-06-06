
test_that("function works", {
  df <- iris
  df$index <- 1:NROW(df)
  df$static <- 5
  expect_equal(redundant_cols(df), c("index", "static"))
})

test_that("result is a character vector", {
  df <- iris
  df$index <- 1:NROW(df)
  df$static <- 5
  expect_type(redundant_cols(df), "character")
})

test_that("result is null", {
  expect_equal(redundant_cols(iris), character(0))
})

test_that("errors work", {
  expect_error(redundant_cols(c(1,2,3)))
  expect_error(redundant_cols(iris, variables = c(1,2,3)))
  expect_error(redundant_cols(iris, variables = c()))
  expect_error(redundant_cols(iris, delete = "Yes"))
  expect_error(redundant_cols(iris, correlated = "Yes"))
  expect_error(redundant_cols(data.frame()))
})

test_that("messages work", {
  expect_message(redundant_cols(iris, variables = "variable"))
  expect_message(redundant_cols(iris, variables = ""))
  expect_message(redundant_cols(iris, corr_treshold = "something"))
  expect_message(redundant_cols(iris, corr_treshold = c(0,1)))
  expect_message(redundant_cols(iris, corr_treshold = 1.5))
  expect_message(redundant_cols(iris, corr_treshold = -0.5))
})

test_that("result is a list when delete = TRUE", {
  expect_type(redundant_cols(iris, delete = TRUE), "list")
})

test_that("function works with only one variable", {
  df <- iris
  df$index <- 1:NROW(df)
  df$static <- 5
  expect_equal(redundant_cols(df, variables = "index"), "index")
  expect_equal(redundant_cols(iris, variables = "Sepal.Length"), character(0))
})

test_that("finding highly correlated columns works", {
  df <- mtcars
  df$variable <- df$hp * 5
  expect_equal(redundant_cols(df, correlated = TRUE), c("cyl", "hp"))
})

test_that("finding duplicated columns works", {
  df <- iris
  df$something <- df$Sepal.Length
  expect_equal(redundant_cols(df), "something")
})

test_that("corr_treshold works without setting correlated", {
  df <- mtcars
  df$variable <- df$hp * 5
  expect_equal(redundant_cols(df, corr_treshold = 0.8), c("mpg", "cyl", "disp", "hp"))
})

test_that("columns with unique float values aren't treated as index", {
  df <- iris
  df$col1 <- df$Sepal.Length + rnorm(length(df$Sepal.Length), 0, 0.3)
  df$col2 <- df$Sepal.Length + rnorm(length(df$Sepal.Length), 0, 1)
  expect_equal(redundant_cols(df, correlated = TRUE), c("Petal.Length", "Sepal.Length"))
})

test_that("finding duplicated columns with the same names works", {
  iris_double <- cbind(iris, iris)
  expect_equal(length(redundant_cols(iris_double)), 5)
})
