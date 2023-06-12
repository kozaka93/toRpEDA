test_that('errors',{
  expect_error(dataset_info(mtcars,info="b"))
  expect_error(dataset_info("izaisgreat"))
  expect_error(dataset_info(mtcars, variables=c("easy","gold")))
  expect_error(dataset_info(mtcars, variables=3.14))
})
test_that('warnings',{
  expect_warning(dataset_info(matrix(ncol=3,nrow=2)))
})

test_that('value',{
  expect_equal(dataset_info(mtcars)$nrow, nrow(mtcars))
  expect_equal(dataset_info(mtcars)$ncol, ncol(mtcars))
  expect_equal(dataset_info(mtcars,"mpg")$ncol, 1)
  })

test_that('result type', {
  expect_type(dataset_info(mtcars)$classes, "character")
  })

