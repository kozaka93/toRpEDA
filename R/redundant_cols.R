#' redundant_cols`()` suggests redundant columns and deletes them if requested.
#'
#' @param df Data frame.
#' @param variables Vector of names of variables to consider. All of variables
#' are taken as default.
#' @param  correlated If `TRUE`, function treats highly correlated variables as
#' redundant columns and also deletes them if `delete` is `TRUE`. `FALSE` as default.
#' @param corr_treshold Number between 0 and 1 that defines high correlation.
#' `0.9` as default, meaning columns with correlation above `0.9` will be treated
#' as redundant.
#' @param delete If `TRUE`, function returns data frame with deleted redundant
#' columns. It also includes columns skipped in `variables`. `FALSE` as default.
#'
#' @return If `delete` = `FALSE`, vector of names of redundant columns. Otherwise
#' data frame with deleted redundant columns. In case there are not any redundant
#' columns, `character(0)` is returned.
#'
#' @examples
#' library("toRpEDA")
#'
#' # finding index and static columns
#' df <- iris
#' df$index <- 1:NROW(df)
#' df$static <- 5
#' redundant_cols(df)
#'
#' # deleting redundant columns
#' df <- redundant_cols(df, delete = TRUE)
#'
#' # finding highly correlated columns
#' df <- mtcars
#' df$variable <- df$hp * 5
#' redundant_cols(df, correlated = TRUE)
#' redundant_cols(df, corr_treshold = 0.8)
#'
#' # finding duplicated columns
#' df <- iris
#' df$something <- df$Sepal.Length
#' redundant_cols(df)
#'
#' @export

redundant_cols <-
  function(df,
           variables = colnames(df),
           correlated = FALSE,
           corr_treshold = NULL,
           delete = FALSE) {
    # checking if all parameters are appropriate
    if (!is.data.frame(df))
      stop("df is not a data frame!")

    if (!is.character(variables))
      stop("variables is not a character vector!")
    else if (!all(variables %in% colnames(df))) {
      message("chosen variables do not exist in dataframe, all variables will be taken")
      variables <- colnames(df)
    }

    if (!is.logical(delete))
      stop("delete is not logical")

    if (!is.logical(correlated))
      stop("correlated is not logical")

    if (NROW(df) == 0)
      stop("df is empty")

    if (all(!is.null(corr_treshold),!correlated))
      correlated <- TRUE
    if (all(
      !is.null(corr_treshold),
      any(
        !is.numeric(corr_treshold),
        length(corr_treshold) != 1,
        corr_treshold > 1,
        corr_treshold < 0
      )
    )) {
      message("corr_treshold is not a proper number between 0 and 1; changing it to 0.9")
      corr_treshold <- 0.9
    }
    if (correlated)
      corr_treshold <- 0.9

    # skipping columns not provided in variables parameter
    original_df <- df
    df <- dplyr::select(df, dplyr::all_of(variables))

    # checking for duplicated columns
    duplicated_cols <- duplicated(as.list(df))
    variables <- variables[!duplicated_cols]
    cols <- colnames(df[duplicated_cols])
    df <- dplyr::select(df, dplyr::all_of(variables))

    # checking for index and static columns
    n <- NROW(df)
    for (col in variables) {
      n_unique <- dplyr::n_distinct(df[, col], na.rm = TRUE)
      if (any(all(n_unique == n,
                  !(is.numeric(df[, col]) &&
                       any(df[, col] != round(df[, col])))),
              n_unique == 1)) {
        cols <- c(cols, col)
        df <- dplyr::select(df,-col)
      }
    }

    # checking for strongly correlated columns among the numeric ones if requested
    if (correlated) {
      # creating correlation matrix
      cor_matrix <- stats::cor(dplyr::select_if(df, is.numeric))

      # finding columns with correlation above 0.9
      highly_correlated <-
        which(abs(cor_matrix) > corr_treshold &
                upper.tri(cor_matrix),
              arr.ind = TRUE)
      highly_correlated <-
        unique(unlist(dimnames(highly_correlated)[1]))
      cols <- c(cols, highly_correlated)
      df <- dplyr::select(df,-dplyr::one_of(highly_correlated))
    }

    # informing about lack of redundant columns
    if (length(cols) == 0)
      message("There are no redundant columns")

    # deleting columns if requested
    if (delete)
      return(dplyr::select(original_df,-dplyr::one_of(cols)))

    return(cols)
  }
