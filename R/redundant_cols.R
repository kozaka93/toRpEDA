#' redundant_cols`()` suggests redundant columns.
#'
#' @param df Data frame.
#' @param variables Vector of names of variables to consider. All of variables
#' are taken as default.
#' @param delete If `TRUE`, function returns data frame with deleted redundant
#' columns. `FALSE` as default.
#' @param  correlated If `TRUE`, function treats highly correlated variables as redundant columns and also deletes
#' them if `delete` is `TRUE`. `FALSE` as default.
#'
#' @return If `delete` = `FALSE`, vector of names of redundant columns. Otherwise
#' data frame with deleted redundant columns. In case there are not any redundant
#' columnm, `character(0)` is returned.
#'
#' @examples
#' library("toRpEDA")
#' redundant_cols(iris)
#' redundant_cols(iris, c("Sepal.Length","Sepal.Width","Species"), delete = TRUE)
#'
#' @export

redundant_cols <- function(df, variables = colnames(df), correlated = FALSE, delete = FALSE) {
  if(!is.data.frame(df))
    stop("df is not a data frame!")

  if(!is.character(variables))
    stop("variables is not a character vector!")
  else if(!all(variables %in% colnames(df))) {
    message("chosen variables do not exist in dataframe, all variables will be taken")
    variables = colnames(df)
  }

  if(!is.logical(delete))
    stop("delete is not logical")
  if(!is.logical(correlated))
    stop("correlated is not logical")

  if(NROW(df) == 0)
    message("df is empty")

  df <- dplyr::select(df, dplyr::all_of(variables))

  duplicated_cols <- duplicated(as.list(df))
  variables <- variables[!duplicated_cols]
  cols <- colnames(df[duplicated_cols])

  df <- dplyr::select(df, dplyr::all_of(variables))

  n <- NROW(df)

  for(col in variables) {
    n_unique <- dplyr::n_distinct(df[,col], na.rm = TRUE)
    if(n_unique == n | n_unique == 1 ) {
      cols <- c(cols, col)
      df <- select(df, -col)
    }
  }
  if(correlated) {
    cor_matrix <- stats::cor(dplyr::select_if(df, is.numeric))
    highly_correlated <- which(abs(cor_matrix) > 0.9 & upper.tri(cor_matrix), arr.ind = TRUE)
    highly_correlated <- unique(unlist(dimnames(highly_correlated)[1]))
    cols <- c(cols, highly_correlated)
    df <- dplyr::select(df, -one_of(highly_correlated))
  }
  if(delete)
    return(df)
  return(cols)
}
