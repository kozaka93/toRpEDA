#' Plot scatter plots for continuous variables
#'
#' Function for displaying scatter plots of contninuous variables in data frame.
#' Automatically picks columns of data frame and displays unique variables pairs
#' scatter plots on one multi-faceted graphic. Column names can be specified. Color grouping by
#' target can be added in case of classification task.
#'
#' @param df Data frame
#' @param vars (optonal) Vector with names of columns to include on plots.
#' @param target (optional) Name of target column to perform grouping on scatter plot.
#'
#' @return patchwork's list of ggplot objects
#'
#' @export
#'
#' @import patchwork ggplot2
#'
#'
#' @examples
#' df <- attitude
#' df['target'] <- as.factor(rbinom(30, 1, 0.3))
#'
#' plot_scatter(df, vars=c('rating', 'complaints', 'privileges', 'learning'))
#' plot_scatter(df, target='target')
plot_scatter <- function(df, vars, target = NULL) {
  if (!('data.frame' %in% class(df))) {
    stop("df must be data.frame class object")
  }
  if (!is.null(target)) {
    if (length(target) > 1) {
      stop("You can specify only one target column.")
    }
    if (!(target %in% colnames(df))) {
      stop("Target column not present in dataframe.")
    }
    if (length(unique(df[[target]])) > 6) {
      stop("Target column has too many unique values. Maximum: 6")
    }
  }
  if (missing(vars)) {
    names <- colnames(df)
    vars <- c()
    for (name in names) {
      if (is.numeric(df[[name]]) && length(unique(df[[name]])) > 5) {
        vars <- c(vars, name)
      }
    }
  }
  else {
    if (!all(vars %in% colnames(df))) {
      stop("There are columns not present in dataframe passed through
            vars parameter.")
    }
    non_numeric <- c()
    for (name in vars) {
      if (!is.numeric(df[[name]])) {
        non_numeric <- c(non_numeric, name)
      }
    }
    if (length(non_numeric) > 0) {
      war <- sprintf(
        "There are columns passed through vars parameter that
          are not numeric. Namely: %s. Ignoring them.",
          paste(non_numeric, collapse = " ")
      )
      warning(war)
      vars = vars[!vars %in% non_numeric]
    }
    non_continuous <- c()
    for (name in vars) {
      if (length(unique(df[[name]])) <= 5) {
        non_continuous <- c(non_continuous, name)
      }
    }
    if (length(non_continuous) > 0) {
      war <-
        sprintf(
          "There are columns passed through vares that have very low
            cardinality (below 6 unique values). Namely: %s . It is adviced to choose
            columns with higher cardinality for scatter plots.",
            paste(non_continuous, collapse = " ")
        )
      warning(war)
    }
  }
  if (length(vars) == 0) {
    stop(
      "There are no continuous variables found in data frame. Consider
        passing names of necassary columns through vars parameter."
    )
  }
  if (length(vars) == 1) {
    stop(
      "Only one continuous column found in vars parameter. There must be at least 2
        to show scatter plots."
    )
  }
  if (length(vars) > 8) {
    warning(
      "There are too many continuous variables to display them on one
        plot. Displaying maximum: 8. Consider changing number of columns in dataset or
        passing names of necassary columns through vars parameter."
    )
    vars <- vars[1:8]
  }
  plots_num <-
    factorial(length(vars)) / (factorial(2) * factorial(length(vars) - 2))
  plot_list <- list()
  indeks = 1
  for (i in 1:(length(vars) - 1)) {
    for (j in (i + 1):length(vars)) {
      plot <-
        ggplot(df, aes_string(
          x = vars[i],
          y = vars[j],
          color = target
        )) +
        geom_point() +
        theme_torpeda()
      if (!is.null(target)) {
        plot <- plot + scale_color_manual(
          values = colors_discrete_torpeda(n = length(unique(df[[target]]))))
      }
      plot_list[[indeks]] <- plot
      indeks = indeks + 1
    }
  }
  nr <- 4
  if (plots_num < 4)
    nr <- plots_num
  combined_plots <- wrap_plots(plot_list, ncol = nr)
  combined_plots
}
