#' The aim of this function is to check how many missing values has our data.
#'
#'@param df dataframe
#'@param variable selected column (default value = all columns)
#'
#'
#'@return A table and a diagram representing the number of missing values in each/selected colums.
#'@import ggplot2
#'@examples
#'library(ggplot2)
#' df <- data.frame(data.frame(a = c(2,5,4,NA, 5,7,NA), b =  c(NA,NA,2,NA, 5,7,3),
#' c = c(23,5,4,1, 5,7,NA), d =  c(NA,NA,2,NA, 5,7,5),
#' e = c(4,5,4,NA, 0,7,NA), f =  c(1,123,2,87, 5,7,6),
#' g = c(253,NA,NA,NA, NA,NA,NA), h =  c(3,34,2,5, 5,7,56)))
#'
#'find_missing_values(df)
#'find_missing_values(df, variable = c("a", "e"))
#'find_missing_values(df, variable = "b")
#'
#'@export



find_missing_values <- function(df, variable = NULL) {
  if(!is.data.frame(df)){
    stop("Dataframe required!")
  }
  if(is.null(variable)){
    p <- ncol(df)
    tmp <- rep(0, p)
    for (i in 1:p) {
      if(is.na(as.integer(table(is.na(df[, i]))[2])))
      {
        tmp[i] <- as.integer(table(is.na(df[, i]))[1])
      }
      else{
      tmp[i] <- as.integer(table(is.na(df[, i]))[2])
      }
      }
    data <-
      as.data.frame(data.frame(Column = colnames(df), Number = tmp))
    diagram <- ggplot(data, aes(x = colnames(df), y = tmp)) +
      geom_col() +
      xlab("Colnames") +
      ylab("Missing values") +
      theme_torpeda() +
      scale_x_discrete(guide = guide_axis(angle = 90))
    tab_values <- data.frame(colnames(df), tmp)
  }
  else if(length(variable) == 1) {
    if(! variable %in% colnames(df))
    {
      stop("You have to write correct column name!")
    }
    if(is.na(as.integer(table(is.na(df[, variable]))[2])))
    {
      tmp <- as.integer(table(is.na(df[, variable]))[1])
    }
    else{
      tmp <- as.integer(table(is.na(df[, variable]))[2])
    }
    data <-
      as.data.frame(data.frame(Column = variable, Number = tmp))
    diagram <- ggplot(data, aes(x = variable, y = tmp)) +
      geom_col() +
      xlab(variable) +
      ylab("Missing values")+
      theme_torpeda()
    tab_values <- data.frame(variable, tmp)
  }
  else if(length(variable) > ncol(df))
  {
    stop("You selected too many variables!")
  }
  else{
    for(j in variable){
    if(any(! j %in% colnames(df))) {
      stop("You have to write correct columns names!")
    }}
    new_df = df[,variable]
    p <- ncol(new_df)
    tmp <- rep(0, p)
    for (i in 1:p) {
      if(is.na(as.integer(table(is.na(df[, variable[i]]))[2])))
      {
        tmp[i] <- as.integer(table(is.na(df[, variable[i]]))[1])
      }
      else{
        tmp[i] <- as.integer(table(is.na(df[, variable[i]]))[2])
      }
    }
    data <-
      as.data.frame(data.frame(Column = variable, Number = tmp))
    diagram <- ggplot(data, aes(x = variable, y = tmp)) +
      geom_col() +
      xlab(variable) +
      ylab("Missing values") +
      theme_torpeda()
    tab_values <- data.frame(variable, tmp)
  }
  names(tab_values) <- c("Column name" ,"Number of missing values")
  tab_values[is.na(tab_values)] <- 0
  return(list("Table" = tab_values, "Diagram" = diagram))
}


