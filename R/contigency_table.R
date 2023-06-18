#' Contigency table
#'
#' This function creates a contingency table to summarize the frequencies or
#' counts of each combination. The resulting table provides a clear visual
#' representation of the relationship between the variables.
#'
#' @param df A data frame to analyze.
#' @param var1 Name of the first categorical variable.
#' @param var2 Name of the second categorical variable.
#'
#' @returns The function returns a contingency table object that displays the
#' frequencies or counts of different combinations of the categorical variables
#' `var1` and `var2`.
#'
#' @examples
#' # Load the package
#' library(toRpEDA)
#' # Create sample categorical variables
#' data <- data.frame(
#'   gender = c("Male", "Female", "Male", "Male", "Female"),
#'   color = c("Blue", "Red", "Blue", "Green", "Green")
#' )
#' # Generate the contingency table
#' contingencyTable <- contigency_table(data, "gender", "color")
#' # Print the table
#' print(contingencyTable)
#'
#' @export


contigency_table <- function(df, var1=NULL, var2=NULL) {
  result <- NULL
  if(!is.data.frame(df)){
    stop("`df` should be a data frame!")
  }
  if(is.null(var1) | is.null(var2)){
    warning("Some variables were not chosen! Returning NULL")
    return(NULL)
  }
  if (!is.null(var1) & !is.null(var2) & is.character(var1) & is.character(var2)) {
    if ((var1 %in% colnames(df)) & (var2 %in% colnames(df))) {
      result <- table(df[[var1]], df[[var2]])
    } else {
      stop("`df` does not contain required columns!")
    }
  } else if (is.data.frame(df) & !is.null(var1) & !is.null(var2) & (!is.character(var1) | !is.character(var2))) {
    warning("Invalid type used in function: `var1` and `var2` must be character vectors! Returning NULL")
    return(NULL)
  }

  return(result)
}




