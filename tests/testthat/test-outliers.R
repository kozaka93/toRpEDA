library(glue)
library(isotree)

test_that("Outliers function correctly detects outliers in a numeric vector", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1000)
  expect_message(outliers(x), "Potential outlier indexes: 1, 10")
})

test_that("Outliers function correctly detects the absence of outliers in a numeric vector", {
  x <- c(1, 2, 3, 1, 2, 3, 1, 1)
  expect_message(outliers(x), "Vector has no outliers")
})

test_that("Outliers function correctly detects outliers in numeric columns of a data frame", {
  df <- data.frame(
    x = c(2, 2, 3, 4, 5, 3, 4, 3, 2, 10),
    y = c(10, 20, 30, 20, 10, 10, 20, 30, 30, 100)
  )
  expect_message(outliers(df), "For column x potential outliers indexes: 10. For column y potential outliers indexes: 10. Based on all columns: no outliers were found. ")
})

test_that("Outliers function correctly detects the absence of outliers in numeric columns of a data frame", {
  df <- data.frame(
    x = c(3, 3, 4, 1, 3, 4, 1, 2, 1, 4),
    y = c(20, 20, 30, 20, 10, 10, 20, 30, 30, 30)
  )
  expect_message(outliers(df), "For x, no outliers were detected. For y, no outliers were detected. Based on all columns: no outliers were found. ")
})

test_that("Outliers function correctly detects outliers in all numeric columns of a data frame", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100),
    y = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  )
  expect_message(outliers(df), "For column x potential outliers indexes: 1, 10. For column y potential outliers indexes: 1, 10. Based on all columns: no outliers were found. ")
})


