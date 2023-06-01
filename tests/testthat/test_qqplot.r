test_that('errors',{
  expect_error(qqplot(c("pii", "haha")))
  expect_error(qqplot(mtcars, variables = 1.3))
  expect_error(qqplot(mtcars, variables = c("ht", "hp")))
  expect_error(qqplot(mtcars, y = "c"))
  expect_error(qqplot(matrix(ncol=3,nrow=3)))
  expect_error(qqplot(iris$Sepal.Length, rt(130, df = 4)))
})

test_that('warnings',{
  expect_warning(qqplot(iris))
})
