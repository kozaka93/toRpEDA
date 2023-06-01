#' QQ Plot
#'
#'
#' The qqplot function generates a QQ plot for a given dataset or a vector. By default, it plots the QQ plot against the standard normal distribution. However, if a vector y is provided, it will plot the QQ plot against a different specified distribution.
#'
#' @param data A matrix, data frame or numeric vector, which is sample for qqplot.
#' @param y An optional vector specifying the values of a custom distribution to compare against the dataset's quantiles. If provided, the QQ plot will be generated using this custom distribution instead of the standard normal distribution.
#' @param variables An optional character string specifying the variables of dataset to include in the QQ plot. If not provided, all numeric variables from the dataset will be used.
#'
#' @examples
#'
#' library(toRpEDA)
#'
#' qqplot(iris)
#'
#' # QQ plot with selected variables
#' qqplot(mtcars, variables = c("cyl", "carb", "mpg"))
#'
#' # QQ plot with custom distribution
#' custom_distribution <- rt(150, df=4)
#' qqplot(iris$Sepal.Length, y = custom_distribution)
#'
#' @export



qqplot <- function(data, y = NULL, variables = NULL) {

    # checking if all parameters are appropriate
    if (!is.matrix(data) & !is.data.frame(data) & !is.vector(data)) {
      stop("Argument 'data' must be a data frame, a matrix or a vector.")
      }

    if (!is.vector(data) & !is.null(variables) & is.character(variables)) {
      if (all(variables %in% colnames(data))) {
        data <- data[, variables]
        } else {
          stop("The dataset does not contain the required columns. Please check that the specified column names are spelled correctly and exist in the dataset.")
          }
      }
  else if (!is.null(variables) & !is.character(variables)) {
    stop("Invalid variable type used in function. Argument 'variables' must be NULL (deafult) or a character vector.")
  }

  if (!is.null(y) & !is.numeric(y) & !is.vector(y)) {
    stop("Argument 'y' must be NULL (deafult) or a numeric vector.")
  }

    # selecting only numeric variables
    if (!is.vector(data) & !all(sapply(data, is.numeric))) {
      if (all(!sapply(data, is.numeric))) {
        stop("All variables are non-numeric.")
        } else{
          data <- data[, sapply(data, is.numeric)]
          warning("Only numeric variables were used to qq-plot.")
          }
    }


    if (is.vector(data)) {
      len <- length(data)
      cvar <- 1
      } else {
        len <- dim(data)[1]
        cvar <- dim(data)[2]
      }

  if (cvar > 9) {
    stop("Too many variables are selected.")
  }


    if (is.null(y)) {
      y <- rnorm(len)
    } else{
      if(length(y) != len) {
        stop("The vector 'y' must be of the same length as the vector 'data' or of a length equal to the number of rows in the 'data' argument")
      }
    }

    if (cvar > 1){
      k <- ceiling(sqrt(cvar))
      par(mfrow=c(k,k))
      for (i in 1:cvar){
        plot(sort(data[,i]),sort(y),xlab="Sample",ylab="Theoretical",main=colnames(data)[i])
        }
    } else {
        plot(sort(data),sort(y),xlab="Sample",ylab="Theoretical")
    }

  par(mfrow=c(1,1))
}
