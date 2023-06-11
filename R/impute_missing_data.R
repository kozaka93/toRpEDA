#' Impute missing data in a data frame
#'
#' This function imputes missing data in a data frame using one or more specified methods.
#' The function can impute missing data for all columns or for a selected set of columns.
#'
#' @param df A data frame containing the data to impute.
#' @param variables A list of character vectors specifying the columns to impute for each method.
#'   Each element of the list should be a character vector containing the names of the columns to impute
#'   using the corresponding method in the `methods` parameter. The default value is a list containing all column names of `df`.
#' @param methods A character vector specifying the methods to use for imputing missing data.
#'  The available methods are `"mean"`, `"median"` and `"mode"`. The default value is `"mean"`.
#'
#' @return A data frame with the same dimensions as `df` where missing values have been imputed using the specified methods.
#'
#' @examples
#' df <- data.frame(x <- c(1, NA, 3, NA),y <- c(NA, 4, 6, 8))
#' impute_missing_data(df)
#'
#' @export
#'
impute_missing_data <- function(df, variables = list(colnames(df)), methods=c("mean")) {

  if(!is.data.frame(df))
    stop("df is not a data frame!")

  if(!is.list(variables) || !all(sapply(variables, is.character)))
    stop("variables is not a list of character vectors!")

  if(!all(unlist(lapply(variables, function(x) all(x %in% colnames(df)))))) {
    message("Some chosen variables do not exist in dataframe, all variables will be taken")
    variables <- rep(list(colnames(df)), length(methods))
  }

  if (!is.character(methods) || !all(methods %in% c("mean", "median", "mode")))
    stop("methods must be a character vector containing only 'mean', 'median' or 'mode'!")
                        
  impute_data <- function(i) {
    cols <- variables[[i]]
    method <- methods[i]
    if (method == "mean") {
      df <<- impute_mean(df, cols)
    } else if (method == "median") {
      df <<- impute_median(df, cols)
    } else if (method == "mode") {
      df <<- impute_mode(df, cols)
    }
  }
  lapply(seq_along(variables), impute_data)
  return(df)
}
#' Impute mean
#'
#' This function imputes mean
#' The function can impute mean for all columns or for a selected set of columns.
#'
#' @param df A data frame containing the data to impute.
#' @param variables A character vector containing the names of the columns to impute
#'   using mean. The default value is a list containing all column names of `df`.
#'
#' @return A data frame with the same dimensions as `df` where missing values have been imputed using the mean.
#'
#' @examples
#' df <- data.frame(x <- c(1, NA, 3, NA),y <- c(NA, 4, 6, 8))
#' impute_mean(df)
#'
#' @export
#'
impute_mean <- function(df, variables = colnames(df)) {
  impute_col_mean <- function(col) {
    if (!is.character(col) && !is.factor(col)) {
      col[is.na(col)] <- mean(col, na.rm = TRUE)
    }
    if (inherits(col, "Date")) {
      mean_date <- mean(col, na.rm = TRUE)
      col[is.na(col)] <- mean_date
      col <- as.Date(as.double(col), origin = "1970-01-01")
    }
    return(col)
  }
  df[variables] <- lapply(df[variables], impute_col_mean)
  return(df)
}
#' Impute mode
#'
#' This function imputes mode
#' The function can impute mode for all columns or for a selected set of columns.
#'
#' @param df A data frame containing the data to impute.
#' @param variables A character vector containing the names of the columns to impute
#'   using mode. The default value is a list containing all column names of `df`.
#'
#' @return A data frame with the same dimensions as `df` where missing values have been imputed using the mode.
#'
#' @examples
#' df <- data.frame(x <- c(1, NA, 3, NA),y <- c(NA, 4, 6, 8))
#' impute_mode(df)
#'
#' @export
#'
impute_mode <- function(df, variables = colnames(df)) {
  calc_mode <- function(x) {
    if (is.character(x) | is.logical(x)) {
      names(which.max(table(x)))
    } else if (is.factor(x)) {
      levels(x)[which.max(table(x))]
    } else {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
  }
  df[variables] <- lapply(df[variables], function(x) {
    mode_val <- calc_mode(na.omit(x))
    x[is.na(x)] <- mode_val
    x
  })
  return(df)
  }
#' Impute median
#'
#' This function imputes median
#' The function can impute median for all columns or for a selected set of columns.
#'
#' @param df A data frame containing the data to impute.
#' @param variables A character vector containing the names of the columns to impute
#'   using median. The default value is a list containing all column names of `df`.
#'
#' @return A data frame with the same dimensions as `df` where missing values have been imputed using the median.
#'
#' @examples
#' df <- data.frame(x <- c(1, NA, 3, NA),y <- c(NA, 4, 6, 8))
#' impute_median(df)
#'
#' @export
#'
impute_median <- function(df, variables = colnames(df)) {
  impute_col_median <- function(col) {
    if (!is.character(col) && !is.factor(col) && !is.complex(col)) {
      col[is.na(col)] <- median(col, na.rm = TRUE)
    }
    if (inherits(col, "Date")) {
      median_date <- median(col, na.rm = TRUE)
      col[is.na(col)] <- median_date
      col <- as.Date(as.double(col), origin = "1970-01-01")
    }
    return(col)
  }
  df[variables] <- lapply(df[variables], impute_col_median)
  return(df)
}
