get_sample_df_simple <- function() {
  data.frame(list(
    'num1' = c(1, 2, 3, 4),
    'num2' = c(1.1, 2.2, 3.3, 4.4),
    'catg1' = c('Mason Mount', 'Hi Mom!', 'asdsadasdasd', 'qwertyuiop'),
    'catg2' = c('1', '2', '3', '4')
  ))
}

get_sample_df_long <- function() {
  data.frame(list(
    'num1' = rep(28, 100),
    'num2' = 1:100,
    'catg1' = rep('R', 100),
    'catg2' = as.character(1:100)
  ))
}

get_sample_df_with_na <- function() {
  data.frame(list(
    'num1' = c(1, 2, 3, 4),
    'num2' = c(1.1, 2.2, 3.3, 4.4),
    'num3na' = c(1.1, 2, NA, 8),
    'catg1' = c('elo', 'Mason Mount', 'Hi Mom!', 'cheers'),
    'catg2na' = c(NA, 'Mason Mount', 'Hi Mom!', NA)
  ))
}

test_that("error when argument not a data frame", {
  expect_error(get_descriptive_stat(4))
  expect_error(get_descriptive_stat('elo'))
  expect_error(get_descriptive_stat(list('mason' = 1, 'mount' = 9)))
})

test_that("correctly distinguish between continuous and categorical vars", {
  df <- get_sample_df_simple()
  result <- get_descriptive_stat(df)
  continous_names <- rownames(result$ContinuousVariables)
  categorical_names <- names(result)[names(result) != 'ContinuousVariables']
  expect_setequal(continous_names, c('num1', 'num2'))
  expect_setequal(categorical_names, c('catg1', 'catg2'))
})

test_that("get stats only for selected variables", {
  df <- get_sample_df_simple()
  result <- get_descriptive_stat(df, vars = c('num1', 'catg1'))
  continous_names <- rownames(result$ContinuousVariables)
  categorical_names <- names(result)[names(result) != 'ContinuousVariables']
  expect_setequal(continous_names, c('num1'))
  expect_setequal(categorical_names, c('catg1'))
})

test_that("include_long_catg=TRUE includes all vars from data frame", {
  df <- get_sample_df_long()
  result <- get_descriptive_stat(df, include_long_catg = TRUE)
  categorical_names <- names(result)[names(result) != 'ContinuousVariables']
  expect_setequal(categorical_names, c('catg1', 'catg2'))
})

test_that("include_long_catg=FALSE ignores some vars from data frame", {
  df <- get_sample_df_long()
  result <- get_descriptive_stat(df, include_long_catg = FALSE)
  categorical_names <- names(result)[names(result) != 'ContinuousVariables']
  expect_setequal(categorical_names, c('catg1'))
})

test_that("ignore NA when calculating stats for numerical variables", {
  df <- get_sample_df_with_na()
  result <- get_descriptive_stat(df, include_long_catg = FALSE)
  expect_false(any(is.na(result$ContinuousVariables)))
})

test_that("do not return ContinuousVariables element when no numerical vars", {
  df <- get_sample_df_simple()[c('catg1', 'catg2')]
  result <- get_descriptive_stat(df)
  expect_false('ContinuousVariables' %in% names(result))
})
