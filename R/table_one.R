#' table_one
#'
#'
#' Function generates a data frame of the most common statistics
#' such as mean, standard deviation, minimum and maximum
#' grouped by selected argument for numerical variables
#'
#' @param data Data frame
#' @param numeric_vars column names of numeric columns of dataframe which we want to use to have table one
#' arguments should be typed as character vector
#' @param groupby element which we want to group by the rest numeric variables but only one grupby element
#'
#' @return returns a list of the most commonly used statistical measures
#' such as mean, standard deviation, minimum and maximum
#' of desired numerical variables
#' @import stats
#' @import dplyr
#' @examples
#' library("toRpEDA")
#' library(dplyr)
#' library(stats)
#' table_one(numeric_vars = c("Petal.Length","Petal.Width"), groupby = "Species", data= iris)
#' table_one(groupby = 'Species', data= iris)
#' @export

table_one <-function(numeric_vars = NULL, groupby, data){
  data[data == ''] <- NA
  if (missing(data)){
    stop("Please enter valid data")
  }
  if (missing(groupby)) {
    stop("Please enter groupby argument")
  }
  if (!all(numeric_vars %in% colnames(data))){
    stop("Please enter valid column names")
  }
  if(length(groupby)>1){
    stop("Please enter only one groupby element")
  }
  if (!all(groupby %in% colnames(data))){
    stop("Please enter valid column name for groupby argument")
  }
  if (!is.data.frame(data)) {
    stop("Please type data frame in data argument")
  }
  if(!all(unlist(lapply(data[,numeric_vars], is.numeric), use.names = FALSE))){
    warning("One of variables is non-numeric, function will take
            only numeric variables")
    wh <- unlist(lapply(data[,numeric_vars], is.numeric), use.names = FALSE)
    numeric_vars <- c(colnames(data[,numeric_vars])[wh])
  }
  if(is.null(numeric_vars)){
    numeric_vars <- colnames(data %>% select(-all_of(c(groupby))))
    }
  functions<- function(x){
    c(
      "mean (SD)" = paste(format(round(mean(x, na.rm = TRUE),3),nsmall=3),
                          paste0("(",format(round(sd(x, na.rm = TRUE),3),nsmall=3),")"),sep = " "),
      "min (max)" =  paste(format(round(min(x, na.rm = TRUE),3),nsmall=3),
                           paste0("(",format(round(max(x, na.rm = TRUE),3),nsmall=3),")"),sep = " "))
  }
  output <- aggregate(data[,numeric_vars],by=list(data[,groupby]), functions)
  output <- t(output)
  colnames(output) <- output[1,]
  output <- output[-1, ]
  n <-  data %>% group_by(data[,groupby]) %>% summarise(
    n = n())
  n <- t(n)
  colnames(n) <- n[1,]
  n <- n[-1, ]
  out <- as.data.frame(rbind(n,output))
  return(out)
}
