test_that('errors',{
  expect_error(cor_matrix(mtcars,variables = 1.34))
  expect_error(cor_matrix(mtcars, variables = c("ht", "hp")))
  expect_error(cor_matrix(mtcars, method = "pers"))
  expect_error(cor_matrix(matrix(ncol=3,nrow=3)))
  expect_error(cor_matrix(iris$Sepal.Length, "Sepal.Length"))
  expect_error(cor_matrix(array(list(c(1,3),c(1,5,4)))))
})

test_that('warnings',{
  expect_warning(cor_matrix(iris))
})

test_that('value',{
  expect_equal(cor_matrix(cars), cor(cars))
  expect_equal(cor_matrix(cars, method = "spearman"), cor(cars, method = "spearman"))
  expect_equal(cor_matrix(iris, variables = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")), cor(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]))
  expect_equal(cor_matrix(iris, "Sepal.Length"), matrix(1, dimnames = list("Sepal.Length", "Sepal.Length")))
  expect_equal(cor_matrix(iris$Sepal.Length), matrix(1, dimnames = list("v", "v")))
})

test_that('result type', {
  expect_type(cor_matrix(mtcars), "double")
})
