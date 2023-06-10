
test_that('errors',{
  expect_error(table_one(numeric_vars = 'Species',data = iris))
  expect_error(table_one(groupby = "Spcies", data= iris))
  expect_error(table_one(groupby = c("Species",'Petal.Length'), data= iris))
  expect_error(table_one(groupby = c("Species",'Petal.Length','Petal.Length'), data= iris))
  expect_error(table_one(data = iris))
  expect_error(table_one(groupby = 1, data = iris))
  expect_error(table_one(groupby = 'Species'))
  expect_error(table_one(c('Petal.Len'),groupby = 'Species',data = iris))
  expect_error(table_one('cars', 'cars', list('cars' = c(1,2,3))))
  expect_error(table_one(c("Petal.Length","Petal.Width"), c(1), iris))
})


test_that('warnings',{
  iris_with_NA <- iris
  iris_with_NA[,'NAs'] <- 'NA'
  expect_warning(table_one(colnames(iris), 'Species', iris))
  expect_warning(table_one(c("Petal.Length","Petal.Width","NAs"),
                           groupby = "Species", data= iris_with_NA))})

test_that('value',{
  try_2 <- iris %>% group_by(Species) %>% summarise(n=n())
  try <- (table_one(c("Petal.Length","Petal.Width"),'Species',iris))
  expect_equal(as.numeric(try[1,1]),try_2[[1,2]] )
})

test_that('result type', {
  expect_type(table_one(c("Petal.Length","Petal.Width"),'Species',iris), "list")
})
test_that('result class', {
  expect_is(table_one(c("Petal.Length","Petal.Width"),'Species',iris), "data.frame")
})
