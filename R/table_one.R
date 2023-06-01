#' Dataset Information
#' Function generates a list of the most common statistics
#' such as mean, standard deviation, minimum and maximum
#' grouped by selected argument
#'
#' @param data Data frame
#' @param numeric_vars column names of numeric columns of dataframe which we want to use to have table one
#' arguments should be typed as character vector
#' @param groupby element which we want to group by the rest numeric variables
#'

#' @examples
#' library("toRpEDA")
#' library(dplyr)
#' library(stats)
#' table_one(numeric_vars = c("Petal.Length","Petal.Width"), groupby = "Species", data= iris)
#'
#' @return returns a list of the most commonly used statistical measures
#' such as mean, standard deviation, minimum and maximum
#' of desired numerical variables
#' @import stats
#' @import dplyr
#' @export
table_one <-function(numeric_vars, groupby, data){
  data[data == ''] <- NA
  if (!all(numeric_vars %in% colnames(data))){
    stop("Please enter valid column names")
  }
  if (missing(data)){
    stop("Please enter valid data")
  }
  if (!is.data.frame(data)) {
    stop("Please type data frame in data argument")
  }
  if (missing(groupby)) {
    stop("Please enter groupby argument")
  }
  ## Give a class
  #class(TableOneObject) <- "TableOne"
  if(!is.character(groupby)){
    stop("Please enter character groupby argument")
  }
  if(missing(numeric_vars)){
    stop("Please enter numeric column names argument")
  }
  if(!all(unlist(lapply(data[,numeric_vars], is.numeric), use.names = FALSE))){
    warning("One of variables is non-numeric, function will take
            only numeric variables")
    wh <- unlist(lapply(data[,numeric_vars], is.numeric), use.names = FALSE)
    numeric_vars <- c(colnames(data[,numeric_vars])[wh])
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
  out <- rbind(n,output)
  out <- as.data.frame(out)
  return(out)
}
