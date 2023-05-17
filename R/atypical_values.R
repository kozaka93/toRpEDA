#' Description
#'
#' @param df - A data frame
#' @param variables - A char vector containing names of columns in
#' a data frame, for which the analysis will be performed
#' @param analyses - A char vector containing names of analyses which
#' will be performed on data frame
#'
#' @return
#'
#' @examples
#'
#' @export

atypical_values <- function(df, variables, analyses){
  # could part of char can be numeric
  # can char be True and False with different capitalization
  # in float there is ',' instead of '.'

  if(!is.data.frame(df)){
    stop("Argument 'df' has to be a data frame")
  }

  analyses_list <- c('char', 'boolean', 'numeric')

  if(missing(variables)){
    variables <- colnames(df)
  }

  if(missing(analyses)){
    analyses <- analyses_list
  }

  if(!all(variables %in% colnames(df))){
    columns <- variables[!(variables %in% colnames(df))]
    message <- paste(c(
      'Columns:',
      paste(columns, collapse=", "),
      'are not a part of given data frame.'
    ), sep = " ")
    stop(message)
  }

  if(!all(analyses %in% analyses_list)){
    message <- paste(c(
      'Analyses:',
      paste((analyses[!(analyses %in% analyses_list)]), collapse=", "),
      'are not a part of this function and would not be performed.'
    ), sep = " ")
    analyses <- analyses[(analyses %in% analyses_list)]
    stop(message)
  }

  columns <- colnames(df)

  char_fun <- function(x){
    if (is.character(x)){
      suppressWarnings(result <- as.numeric(x))
      all(!is.na(result))
    } else{ FALSE }

  }

  numeric_fun <- function(x){
    if (is.character(x) & !char_fun(x)){
      suppressWarnings(result <- as.numeric(gsub(",",".",x)))
      all(!is.na(result))
    } else{ FALSE }
  }

  boolean_fun <- function(x){
    if (is.character(x)){
      #t, f, true, false
      true_false <- all(tolower(x) %in% c('t','f','true','false'))
      #n, y, no, yes
      yes_no <- all(tolower(x) %in% c('y','n','yes','no'))

      if(true_false){
        1
      }else if(yes_no){
        2
      }else { FALSE }
    }else{ 0 }
  }

  result <- list()
  if ('char' %in% analyses_list){
    result[['char']] <- sapply(df[variables], char_fun)
  }
  if ('boolean' %in% analyses_list){
    result[['boolean']] <- sapply(df[variables], boolean_fun)
  }
  if ('numeric' %in% analyses_list){
    result[['numeric']] <- sapply(df[variables], numeric_fun)
  }

  result
}










