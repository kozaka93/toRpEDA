#' Function for a given data frame or chosen variables either scales them to
#' interval [0,1], or transforms them by subtracting mean and dividing by
#' standard deviation.
#'
#' @param x data frame
#' @param variables vector of names of variables to consider; all numeric
#' variables are taken by default.
#' @param mode either "standard" (the deafult) to perform standardisation or
#' "interval" for scaling variables to unit interval.
#' @param na.om logical. Should whole row be removed when missing value
#' is present?
#'
#' @return Data frame inputed with normalised chosen columns.
#'
#' @examples
#' library('toRpEDA')
#'
#' # Standardising variables of data frame
#' normalise(iris)
#'
#' # scaling variables of data frame to unit integral
#' normalise(iris, mode = "interval")
#'
#' # Choosing variables for normalisation
#' normalise(iris, variables = "Sepal.Width")
#'
#' @export

normalise <- function(x, variables = colnames(x[, which(sapply(x, is.numeric))]),
                      mode = "standard", na.om = FALSE){

  if(!is.data.frame(x)){
    stop("Given data type is not supported.")
  }

  # Taken from szuvarska, ty <3
  if (!is.character(variables))
    stop("'variables' is not a character vector.")
  else if (!all(variables %in% colnames(x))) {
    warning("Not all chosen variables do exist in dataframe, all variables that
            do will be taken.")
    variables <- subset(colnames(x), colnames(x) %in% variables)
  }
  if (length(variables) == 0){
    stop("None variables were chosen.")
  }

  x <- as.data.frame(x[, variables])

  if(!all(sapply(x, is.numeric))){
    stop("Given variables are not of numeric type.")
  } else if(!any(mode == "standard", mode == "interval")){
    stop("Mode should be either 'standard' or 'interval'.")
  }

  if(na.om){
    x <- na.omit(x)
  }

  which_const <- NULL
  if(min(apply(x, 2, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
     < 0.00000001 & mode == "interval"){
    warning("Some of given data columns are constant; Output are vectors filled
            with values 0.5.")
    which_const <- which(apply(x, 2, function(x)
      max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) < 0.00000001)

    x[, which_const] <- 0.5
  }

  if(min(apply(x, 2, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
     < 0.00000001 & mode == "standard"){
    warning("Some of given data columns are constant.")
    which_const <- which(apply(x, 2, function(x)
      max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) < 0.00000001)
  }

  if(any(0 == apply(apply(x, 2, function(x) x%%1), 2, function(x)
    sum(x, na.rm = TRUE)))){
    warning("Some of given data columns are categorical, whereas normalisation
            should be used for continuous data.")
    # which_cat <- which(apply(x, 2, function(x) x%%1) == 0)
  }


  if(is.null(which_const)){
    if(mode == "standard"){
      x <- apply(x, 2, function(x) (na.omit(x) - mean(x, na.rm = TRUE))/
                   sqrt(sum((na.omit(x) - mean(x, na.rm = TRUE))^2)/
                          (length(na.omit(x)) - 1)))
    } else {
      x <- apply(x, 2, function(x) (na.omit(x) - min(x, na.rm = TRUE))/
                   (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    }
  }
  else if(mode == "standard"){
    x[, -which_const] <- apply(as.data.frame(x[, -which_const]), 2, function(x)
      (na.omit(x) - mean(x, na.rm = TRUE))/
        sqrt(sum((na.omit(x) - mean(x, na.rm = TRUE))^2)/(length(na.omit(x)) - 1)))
  } else {
    x[, -which_const] <- apply(as.data.frame(x[, -which_const]), 2, function(x)
      (na.omit(x) - min(x, na.rm = TRUE))/
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }

  return(x)
}
