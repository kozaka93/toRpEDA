#' Dataset Information
#'
#' This function provides information about a given dataframe, including the
#' number of rows and columns, as well as the data types of the columns.
#'
#' @param df A dataframe or matrix to analyze
#' @param info Logical; if TRUE (default), prints information about the dataframe.
#' If FALSE, only returns the information as a list.
#' @return If \code{info} is TRUE, the function prints the information about the
#' dataframe and returns an invisible list containing the number of rows and
#' columns, and the data types of the columns. If \code{info} is FALSE, the
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
#'
#' @export
dataset_info <- function(df,info=TRUE){

  if (!is.matrix(df) && !is.data.frame(df)) {
    stop("\ndf needs to be a matrix or data.frame")
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
  types <- sapply(df,typeof)
  if (info){
    cat("\n Informations about your dataframe:\n",
        "Number of rows: ",rown,"\n",
        "number of columns: ",coln,"\n\n",
        "Types of columns:\n ")
    for(i in 1:coln){
      cat("-",names(types[i])," - ",types[i],"\n ")
    }
  }
  res<-list(nrow=rown,ncol=coln,types=types)
  return(invisible(res))
}

