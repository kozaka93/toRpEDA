
test_that('errors',{
  expect_error(cor_matrix(c(1,2,3)))
  expect_error(cor_matrix(mtcars,variables = 1.34))
  expect_error(cor_matrix(mtcars, variables = c("ht", "hp")))
  expect_error(cor_matrix(mtcars, method = "pers"))
  expect_error(cor_matrix(matrix(ncol=3,nrow=3)))
})



test_that('warnings',{
  expect_warning(cor_matrix(iris))
})

test_that('value',{
  expect_equal(cor_matrix(cars), cor(cars))
  expect_equal(cor_matrix(cars, method = "spearman"), cor(cars, method = "spearman"))
  expect_equal(cor_matrix(iris, variables = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")), cor(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]))

})

test_that('result type', {
  expect_type(cor_matrix(mtcars), "double")
})
