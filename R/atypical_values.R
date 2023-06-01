#' Atypilac values analyses columns with character data and check if they can
#' be different data type: if it is int vector transformed to string - "integer",
#' vector containing 'false' and 'true'  or 'yes'
#' and 'no' with different abbreviations and capitalizations that may indicate that
#' vector could be transform to boolean - "boolean", and if numeric values were
#' written with coma instead of dot.
#' Results are presented in a list with performed
#' analyses that contain column names.
#'
#' @param df - A data frame
#' @param variables - A char vector containing names of columns in
#' a data frame, for which the analysis will be performed
#' @param analyses - A char vector containing names of analyses which
#' will be performed on data frame
#'
#' @return List with boolean vectors for types int or numeric and numeric vector
#' with 1 indicating 'true' and 'false', 2 for 'yes' and 'no'.
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

  analyses_list <- c('integer', 'boolean', 'numeric')

  if(missing(variables)){
    variables <- colnames(df)
  }

  if(missing(analyses)){
    analyses <- analyses_list
  }

  if(!all(variables %in% colnames(df))){
    columns <- variables[!(variables %in% colnames(df))]
    message <- paste(c(
      'Columns:   ',
      paste(columns, collapse=", "),
      '   are not a part of given data frame.'
    ), sep = " ")
    stop(message)
  }

  if(!all(analyses %in% analyses_list)){
    message <- paste(c(
      'Analyses:   ',
      paste((analyses[!(analyses %in% analyses_list)]), collapse=", "),
      '   are not a part of this function and would not be performed.'
    ), sep = " ")
    analyses <- analyses[(analyses %in% analyses_list)]
    stop(message)
  }

  columns <- colnames(df)

  int_fun <- function(x){
    if (is.character(x)){
      suppressWarnings(result <- as.numeric(x))
      all(!is.na(result))
    } else{ FALSE }

  }

  numeric_fun <- function(x){
    if (is.character(x) & !int_fun(x)){
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
  if ('integer' %in% analyses){
    result[['integer']] <- sapply(df[variables], int_fun)
  }
  if ('boolean' %in% analyses){
    result[['boolean']] <- sapply(df[variables], boolean_fun)
  }
  if ('numeric' %in% analyses){
    result[['numeric']] <- sapply(df[variables], numeric_fun)
  }

  result
}










