
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
  expect_null(redundant_cols(iris))
})

test_that("errors work", {
  expect_error(redundant_cols(c(1,2,3)))
  expect_error(redundant_cols(iris, variables = c(1,2,3)))
  expect_error(redundant_cols(iris, delete = "Yes"))
  expect_error(redundant_cols(iris, correlated = "Yes"))
})

test_that("messages work", {
  expect_message(redundant_cols(data.frame()))
  expect_message(redundant_cols(iris, variables = "variable"))
})

test_that("result is a list when delete = TRUE", {
  expect_type(redundant_cols(iris, delete = TRUE), "list")
})

test_that("function works with only one variable", {
  df <- iris
  df$index <- 1:NROW(df)
  df$static <- 5
  expect_equal(redundant_cols(df, variables = "index"), "index")
  expect_null(redundant_cols(iris, variables = "Sepal.Length"))
})

test_that("finding highly correlated columns works", {
  df <- mtcars
  mtcars$variable <- mtcars$hp * 5
  expect_equal(redundant_cols(mtcars, correlated = TRUE), c("cyl", "hp"))
})