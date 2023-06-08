df <- data.frame(
  logical_col = sample(c(TRUE, FALSE), 10, replace = TRUE),
  integer_col = sample(1:10),
  numeric_col = runif(10),
  character_col = LETTERS[1:10],
  factor_col = factor(sample(letters[1:3], 10, replace = TRUE)),
  date_col = seq(as.Date("2023-01-01"), by = "month", length.out = 10),
  POSIXct_col = seq(as.POSIXct("2023-01-01"), by = "week", length.out = 10),
  complex_col = complex(real = runif(10), imaginary = runif(10)),
  stringsAsFactors = FALSE
)
df[sample(nrow(df), 2), ] <- NA

test_that("impute_missing_data returns error when passed not a data frame", {
  expect_error(impute_missing_data("df"))
})

test_that("impute_missing_data returns error when variables is not a list of character vectors", {
  expect_error(impute_missing_data(df, variables = "not a list"))
})

test_that("impute_missing_data displays message when some chosen variables do not exist in dataframe", {
  expect_message(impute_missing_data(df, variables = list(c("logical_col", "non_existent_col"))))
})

test_that("impute_missing_data validates methods parameter", {
  expect_error(impute_missing_data(df, methods = "invalid"))
  expect_error(impute_missing_data(df, methods = c("mean", "invalid")))
})

test_that("impute_missing_data returns an empty data frame when passed an empty data frame", {
  df_empty <- data.frame()
  df_imputed <- impute_missing_data(df_empty)
  expect_equal(nrow(df_imputed), 0)
  expect_equal(ncol(df_imputed), 0)
})

test_that("impute_missing_data imputes missing data using multiple methods for different columns", {
  cols_to_impute <- list(c("integer_col", "numeric_col"), "factor_col")
  methods_to_use <- c("mean", "mode")
  df_imputed <- impute_missing_data(df, cols_to_impute, methods_to_use)
  expect_false(any(is.na(df_imputed[unlist(cols_to_impute)])))
})

test_that("impute_missing_data imputes NA values with median in numeric columns", {
  df2 <- impute_missing_data(df, list(c("integer_col", "numeric_col")), c("median"))
  expect_true(all(!is.na(df2$integer_col)))
  expect_true(all(!is.na(df2$numeric_col)))
})

test_that("impute_missing_data imputes NA values with mode in factor and character columns", {
  df2 <- impute_missing_data(df, list(c("factor_col", "character_col")), c("mode"))
  expect_true(all(!is.na(df2$factor_col)))
  expect_true(all(!is.na(df2$character_col)))
})

test_that("impute_missing_data imputes NA values with mean in all columns except character and factor type", {
  df2 <- impute_missing_data(df, list(colnames(df)), c("mean"))
  non_char_factor_cols <- sapply(df2, function(x) !is.character(x) && !is.factor(x))
  expect_true(all(!is.na(df2[non_char_factor_cols])))
})

test_that("impute_missing_data imputes NA values with median in all columns except character, factor and complex type", {
  df2 <- impute_missing_data(df, list(colnames(df)), c("median"))
  non_char_factor_complex_cols <- sapply(df2, function(x) !is.character(x) && !is.factor(x) && !is.complex(x))
  expect_true(all(!is.na(df2[non_char_factor_complex_cols])))
})

test_that("impute_missing_data imputes NA values with mode in all columns", {
  df2 <- impute_missing_data(df, list(colnames(df)), c("mode"))
  expect_true(all(!is.na(df2)))
})
