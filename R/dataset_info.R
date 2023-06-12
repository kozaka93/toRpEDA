#' Dataset Information
#'
#' This function provides information about a given dataframe, including the
#' number of rows and columns, as well as the data types of the columns.
#'
#' @param df A dataframe or matrix to analyze
#' @param variables An optional character string giving a variables for computing correlation matrix. This must be colnames from df.
#' @param info Logical; if TRUE (default), prints information about the dataframe.
#' If FALSE, only returns the information as a list.
#' @return If \code{info} is TRUE, the function prints the information about the
#' dataframe and returns an invisible list containing the number of rows and
#' columns, and classes of the columns. If \code{info} is FALSE, the
#' function only returns the list.
#' @examples
#' library(toRpEDA)
#'
#' #Getting informations about dataframe
#' dataset_info(mtcars)
#'
#' #Getting number of rows in a dataframe or matrix
#' dataset_info(mtcars,info = FALSE)$nrow
#'
#' #Selecting columns
#' dataset_info(mtcars, variables = c("cyl", "vs", "carb"))
#'
#'
#' @export
dataset_info <- function(df,variables =NULL,info=TRUE){

  if (!is.matrix(df) && !is.data.frame(df)) {
    stop("\ndf needs to be a matrix or data.frame")
  }

  if (!is.null(variables) & is.character(variables)) {

    if (all(variables %in% colnames(df))) {
      df <- as.data.frame(df[, variables])
      colnames(df) <- variables
    } else {
      stop("The dataset does not contain the required columns. Please check that the specified column names are spelled correctly and exist in the dataset.")
    }
  }
  else if (!is.null(variables) & !is.character(variables)) {
    stop("Invalid variable type used in function. Argument 'variables' must be NULL (deafult) or a character vector.")
  }

  if (info != TRUE && info != FALSE){
    stop("\n info needs to be TRUE or FALSE")
  }
  if (is.matrix(df)){
    df <-as.data.frame(df)
    warning("\nMatrix converted to data frame")
  }

  rown <- nrow(df)
  coln <- ncol(df)
  classes <- sapply(df,class)
  if (info){
    cat("\n Informations about your dataframe:\n",
        "Number of rows: ",rown,"\n",
        "number of columns: ",coln,"\n\n",
        "Classes of columns:\n ")
    for(i in 1:coln){
      cat("-",names(classes[i])," - ",classes[i],"\n ")
    }
  }
  res<-list(nrow=rown,ncol=coln,classes=classes)
  return(invisible(res))
}

