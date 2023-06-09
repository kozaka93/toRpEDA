#' Correlation matrix
#'
#'
#' 'cor_matrix' computing correlation matrix for numeric variables from data.
#'
#' @param data A numeric matrix, a data frame or a vector.
#' @param variables An optional character string giving a variables for computing correlation matrix. This must be colnames from data.
#' @param method A character string specifying the correlation coefficient to be calculated. Possible values are: 'pearson' - computes the Pearson correlation coefficient (default), 'spearman' - computes the Spearman rank correlation coefficient.
#'
#'
#' @return A correlation matrix is returned as a numeric matrix, which is a symmetric matrix with correlation coefficients between -1 and 1. The diagonal elements are always equal to 1.
#'
#' @examples
#'
#' library(toRpEDA)
#'
#' #computing correlation matrix only from numeric variables
#' cor_matrix(data = iris)
#'
#' #computing correlation matrix from selected numeric variables
#' cor_matrix(data = mtcars, variables = c("cyl", "vs", "carb"))
#'
#' #computing correlation matrix from numeric variables using 'spearman' method
#' data <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
#' cor_matrix(data, method = 'spearman')
#'
#'
#' @export

cor_matrix <- function(data, variables = NULL, method = 'pearson') {

  # checking if all parameters are appropriate
  if (!is.matrix(data) & !is.data.frame(data) & !is.vector(data)) {
    stop("Argument 'data' must be a data frame, a matrix or a vector.")
  }

  if (!method %in% c('pearson', 'spearman')) {
    stop("Invalid argument used in function. Argument 'method' must be set to either 'pearson' or 'spearman'.")
  }

  if (is.vector(data) & is.character(variables) ) {
    stop("Argument 'data' is a vector and contains only one variable.")
  }

  if (is.vector(data)) {
    names <- 'v'
  } else {
    names <- colnames(data)
  }

  if (!is.vector(data) & !is.null(variables) & is.character(variables)) {
    if (all(variables %in% colnames(data))) {
      data <- data[, variables]
      names <- variables
    } else {
      stop("The dataset does not contain the required columns. Please check that the specified column names are spelled correctly and exist in the dataset.")
    }
  } else if (!is.null(variables) & !is.character(variables)) {
    stop("Invalid variable type used in function. Argument 'variables' must be NULL (deafult) or a character vector.")
  }

  if (is.matrix(data)){
    data <- as.data.frame(data)
  }

  # selecting only numeric variables
  if (!all(sapply(data, is.numeric))) {
    if (all(!sapply(data, is.numeric))) {
      stop("All variables are non-numeric.")
    } else{
      names <- colnames(data)[sapply(data, is.numeric)]
      data <- data[, sapply(data, is.numeric)]
      warning("Only numeric variables were used to calculate the correlation matrix.")
    }
  }

  if (is.vector(data)) {
    data <- as.data.frame(data)
  }

  # checking method selection
  if (method == 'spearman') {
    data <- data.frame(apply(data, 2, rank))
  }

  # computing correlation matrix
  p <- ncol(data)
  lnames <- list(names, names)
  cros <- expand.grid(first = names, second = names)
  for (i in 1:nrow(cros)) {
    x <- data[, cros[i, 1]]
    y <- data[, cros[i, 2]]
    cros$third[i] <-
      sum((x - mean(x)) * (y - mean(y))) / sqrt(sum((x - mean(x)) ^ 2) * sum((y - mean(y)) ^ 2))
  }

  cormatrix <- matrix(cros$third, nrow = p, ncol = p, dimnames = lnames)
  return(cormatrix)

}
