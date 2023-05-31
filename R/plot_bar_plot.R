#' Bar plots by target
#'
#' Creates a plot of categorical columns by target
#'
#'
#' @param df data frame
#' @param target character, name of target column
#' @param variables vector of column names for which charts are to be made
#' @param col_number numeric the number of columns in which the charts will be displayed
#'
#' @return None
#' @import ggplot2 patchwork
#' @export
#'
#' @examples
#' my_data <- data.frame(
#'       target = c("R", "Python", "R", "R", "Python", "Python"),
#'       a = c(1, 2, 3, 4, 2, 2),
#'       b = c("a", "b", "c", "b", "c", "c"),
#'       c = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
#'       d = c("dog ", "cat", "dog", "dog", "dog", "cat"),
#'       e = c("PL", "PL", "PL", "USA", "USA", "USA"),
#'       f = c("Europa", "Europa", "America", "Europa", "America", "Europa")
#'       )
#' plot_bar_plot(my_data, "target")
#' plot_bar_plot(my_data, "target", variables = c("b", "d", "e"))
#' plot_bar_plot(my_data, "target", col_number = 3)

plot_bar_plot <- function(df, target, variables = NULL, col_number = 2) {

  if (!is.data.frame(df))
    stop("df must be data frame")
  if (!is.character(target))
    stop("target must be character")
  if (!is.numeric(col_number))
    stop("Number of column must be numeric")
  if (!(target %in% colnames(df)))
    stop("Incorrect target column")
  if (!all(variables %in% colnames(df)) & !is.null(variables)){
    stop("Incorrect column in variables arguement")
  }
  if (!is.character(df[[target]])){
    stop("Target column is not categorical")
  }
  if(!is.null(variables)){
    for (col_name in variables) {
      if (!is.character(df[[col_name]])) {
        stop("One of variable column is not categorical")
      }
    }
  }

  if (is.null(variables)) {
    for (col_name in colnames(df)) {
      if (is.character(df[[col_name]]) & col_name != target) {
        variables <- c(variables, col_name)
      }
    }
    if(is.null(variables)){
      stop("Dataframe doesn't contain any categorical colum")
    }
  }


  plot_list <- list()
  for (i in 1:length(variables)) {
    plot_list[[i]] <-
      ggplot(df, aes(x = .data[[variables[i]]], fill =  .data[[target]])) +
      geom_bar(width = 0.6,
               position = position_dodge2(width = 0.7, preserve = "single")) +
      scale_x_discrete(drop = FALSE)+
      theme_torpeda()+
      scale_fill_manual(
        values = colors_discrete_torpeda(n = length(unique(df[[target]]))))
  }

  combined_plots <- wrap_plots(plot_list, ncol = col_number)
  combined_plots
}
