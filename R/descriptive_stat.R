library(dplyr)

#' Generates descriptive statistics for a given data frame.
#' Continuous variables stats are grouped in a single data frame while each
#' categorical variable has its stats stored in a separate data frame.
#'
#' @param df A data.frame object to analyse
#' @param vars Variables to compute stats for. NULL means all variables
#' @param include_long_catg Whether or not to include stats for categorical
#' variables with more than 50 unique values
#' @returns a list of data frames with descriptive statistics
#' @examples
#' get_descriptive_stat(iris)
#' get_descriptive_stat(iris, vars = c('Petal.Length', 'Species'))
#' get_descriptive_stat(iris, include_long_catg = TRUE)
get_descriptive_stat <- function(df, vars = c(), include_long_catg = FALSE) {
  if (class(df) != 'data.frame') {
    stop('df parameter should be a data.frame')
  }
  # use only specified variables
  if (length(vars) > 0) {
    df <- df %>% select(vars)
  }

  # continuous stats names
  stats_continuous_names <- c(
    'min', 'max', 'mean', 'median', 'std', 'variance', 'Q1', 'Q3'
  )
  # a data frame storing continuous stats values
  stats_continuous <- data.frame(
    matrix(nrow = 0, ncol = length(stats_continuous_names))
  )
  colnames(stats_continuous) <- stats_continuous_names

  # returned list with all statistics
  all_stats = list()

  for (var in colnames(df)) {
    values <- df[,var]
    # continuous
    if (class(values) %in% c('numeric', 'integer')) {
      stats_for_var_vector <- c(
        min(values, na.rm = TRUE),
        max(values, na.rm = TRUE),
        mean(values, na.rm = TRUE),
        median(values, na.rm = TRUE),
        sd(values, na.rm = TRUE),
        var(values, na.rm = TRUE),
        quantile(values, probs = 0.25, names = FALSE, na.rm = TRUE),
        quantile(values, probs = 0.75, names = FALSE, na.rm = TRUE)
      )
      stats_continuous[var,] = stats_for_var_vector
    }
    # categorical
    else if (length(unique(values)) <= 50 || include_long_catg) {
      stats_for_var <- data.frame(table(values))
      # set category name as an data frame index
      rownames(stats_for_var) <- stats_for_var$Var1
      stats_for_var$Var1 <- NULL
      # rename table's Freq to count
      stats_for_var$count <- stats_for_var$Freq
      stats_for_var$Freq <- NULL
      # add more stats
      stats_for_var$percentage <- 100 * stats_for_var$count / length(values)

      all_stats[[var]] <- stats_for_var
    }
  }
  # add continuous variables stats to the list (if there are any)
  if (NROW(stats_continuous) > 0) {
    all_stats$ContinuousVariables = stats_continuous
  }
  return(all_stats)
}
