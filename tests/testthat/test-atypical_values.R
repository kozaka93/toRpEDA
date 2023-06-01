yes_no <- c("yES", "n",'y',"No",'yes',"nO")
true_false <- c('f','t','TrUe','FaLsE')

df <- data.frame(
  'a' = gsub('\\.',',',as.character(rnorm(10))),
  'b' = c(gsub('\\.',',',as.character(rnorm(5))),rnorm(5)),
  'c' = as.character(c(TRUE,FALSE), replace=TRUE, size=10),
  'd' = sample(yes_no, 10, replace=TRUE),
  'e' = sample(true_false,10, replace=TRUE),
  'f' = c(sample(yes_no,5, replace=TRUE),sample(true_false,5, replace=TRUE)),
  'g' = as.character(sample(1:100,10))
)


test_that('errors work',{
  expect_error(atypical_values(TRUE))
  expect_error(atypical_values(list(
    'f' = c(sample(yes_no,5, replace=TRUE),sample(true_false,5, replace=TRUE)),
    'g' = as.character(sample(1:100,10))
  )))
  expect_error(atypical_values(df, variables = c('a','b','x')))
  expect_error(atypical_values(df, analyses = c('int','bool')))
})

test_that("correct values",{
  expect_type(atypical_values(df),'list')
  expect_type(atypical_values(df)$boolean,'double')
  expect_type(atypical_values(df)$integer,'logical')
  expect_type(atypical_values(df)$integer,'logical')

  expect_equal(atypical_values(df,analyses = c("integer","boolean"))$numeric,NULL)
  expect_equal(atypical_values(df,analyses = c("integer","numeric"))$boolean,NULL)
  expect_equal(atypical_values(df,analyses = c("boolean"))$integer,NULL)

  test7 <- rep(7,3)
  names(test7) <- c('integer','boolean','numeric')
  expect_equal(sapply(atypical_values(df),length), test7)
  test2 <- rep(2,3)
  names(test2) <- c('integer','boolean','numeric')
  expect_equal(sapply(atypical_values(df, variables = c('a','c')),length), test2)

  tnum <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)
  names(tnum) <- colnames(df)
  expect_equal(atypical_values(df)$numeric,tnum)

  tbool <- c(0,0,1,2,1,0,0)
  names(tbool) <- colnames(df)
  expect_equal(atypical_values(df)$boolean,tbool)

  tint <- c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)
  names(tint) <- colnames(df)
  expect_equal(atypical_values(df)$integer,tint)
})

