#'plot_hist_vars
#'@description
#'The plot_hist_vars function creates a series of histogram plots for
#'the numeric variables of a data frame. The function allows users to
#'specify which variables to include in the plots and how many plots to display
#'per page. The function makes use of the ggplot2 and patchwork packages to
#'create the plots and combine them into a single page.
#'
#'@param data a data frame
#'@param vars a character vector of variable names to include in the plots.
#'If NULL (default), all numeric variables are used.
#'@param plots_per_page an integer indicating the number of plots to display per
#'page. Default is 6.
#'@returns series of histogram plots to the R console.
#'
#'@import ggplot2 patchwork
#'@export
#'
#'@examples
#'library(ggplot2)
#'library(patchwork)
#'
#'data(mtcars)
#'plot_hist_vars(mtcars, c("mpg", "cyl"))
#'
#'data(mtcars)
#'plot_hist_vars(mtcars)
#'
#'data(iris)
#'plot_hist_vars(iris, c("Sepal.Length", "Petal.Length", "Species"))
#'
#'data(iris)
#'plot_hist_vars(iris)



plot_hist_vars <- function(data, vars, plots_per_page = 6) {
  all_numeric_vars_idx <- sapply(data, is.numeric)

  if (!any(all_numeric_vars_idx)) {
    warning("No numeric variables found.")
    return(NULL)
  }

  if (missing(vars)) {
    data_numeric <- data[, all_numeric_vars_idx, drop=FALSE]

  } else {

    all_data_by_user_idx <- names(data) %in% vars

    data_by_user <- data[, all_data_by_user_idx, drop=FALSE]

    numeric_vars_by_user_idx <- all_numeric_vars_idx & all_data_by_user_idx

    check_if_numeric_user_data <- sapply(data_by_user, is.numeric)

    if (!any(check_if_numeric_user_data)) {
      warning("None of specified columns are numeric")
      return(NULL)
    }

    if (!all(check_if_numeric_user_data)) {
      warning(paste("Following columns are not numeric:", paste(names(data_by_user)[!check_if_numeric_user_data], collapse = ", ")))
    }

    data_numeric <- data[, numeric_vars_by_user_idx, drop=FALSE]
  }

  plots <- vector("list", ncol(data_numeric))

  for (i in 1:ncol(data_numeric)) {
    plot <-  ggplot(data = data_numeric, aes_string(x = colnames(data_numeric)[i])) +
      geom_histogram(fill = colors_discrete_torpeda(1), bins=30) +
      labs(title = colnames(data_numeric)[i], x = "Value", y = "Frequency") +
      ggtitle(paste("Histogram of", colnames(data_numeric)[i])) +
      xlab(colnames(data_numeric)[i]) +
      theme_torpeda()
    plots[[i]] <- plot
  }

  n_pages <- ceiling(length(plots) / plots_per_page)

  for (i in seq_len(n_pages)) {
    start_idx <- (i - 1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, length(plots))
    page_plots <- plots[start_idx:end_idx]
    page <- patchwork::wrap_plots(page_plots, nrow = 2, ncol = 3)
    print(page)
  }
}
