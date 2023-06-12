library(isotree)
library(glue)

#' Function that finds potential outlier observations
#' @param df A dataframe
#' @param variables column names of given dataframe to process. By default all colnames of dataframe.
#' @return An information about outliers as a message
#' It informed about outliers in individual columns and in all dataset.
#'
#' @examples
#' library(isotree)
#' library(toRpEDA)
#' library(glue)
#'
#' outliers(iris)
#'
#' a <- c(1,2,3,42,1,1,0)
#' outliers(a)
outliers <- function(df, variables = NULL) {
  if(!is.null(variables) && is.data.frame(df)){
    df <- df[, variables]
  }
  df <- na.omit(df)
  mess <- ""
  if(is.vector(df)){
    if(is.numeric(df)){
      col <- df
      out <- sort(c(seq(1, length(df))[df > quantile(col, 0.95)],
                         seq(1, length(df))[df < quantile(col, 0.05)]))
      if(!length(out) > 0){
        mess <- glue(mess, 'Vector has no outliers. ')
      }else{
        mess <- (glue(mess, "Potential outlier indexes: {glue_collapse(out,  sep = ', ')}. "))
      }
    }
  } else if (is.data.frame(df)) {
    for(i in 1:ncol(df)){
      col <- df[[i]]
      if(is.numeric(col)){
        out <- sort(c(seq(1, length(col))[col > quantile(col, 0.975)],
               seq(1, length(col))[col < quantile(col, 0.025)]))
        colname <- colnames(df)[i]
        if(!length(out) > 0){
          mess <- glue(mess, 'For {colname}, no outliers were detected. ')
        }else{
          mess <- glue(mess, "For column {colname} potential outliers indexes: {glue_collapse(out,  sep = ', ')}. ")
        }
      }
    }
    if(length(df)>1){
      model <- isolation.forest(df, nthreads=1)
      pred <- predict(model, df)
      out <- seq(1, nrow(df))[pred > 0.75]
      mess <- glue(mess, 'Based on all columns: ')
      if(is.na(out) || length(out) == 0){
        mess <- glue(mess, 'no outliers were found. ')
      }else{
        mess <- glue(mess, 'potential outliers indexes: {glue_collapse(out,  sep = ', ')}. ')
      }
    }
  } else {
    mess <- glue(mess, 'Input data is not a vector or a data frame. ')
  }
  message(mess)
}
