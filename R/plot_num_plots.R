#' Draws numeric plots (boxplots and violin plots) by target
#' for selected columns.
#'
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#'
#'
#'@param df data frame with numeric values
#'@param variables column names to be included
#'@param target name of target column
#'@param plot_type type of plots, 'boxplot' or 'violin'
#'
#'@return list of plots
#'
#'@examples
#'library("toRpEDA")
#'
#'# drawing 4 plots
#'plot_num_plots(iris, target = "Species", plot_type="violin")
#'plot_num_plots(iris, target = "Species", plot_type="boxplot")
#'
#'plot_num_plots(mtcars, target = "gear", plot_type="boxplot")
#'plot_num_plots(mtcars, variables = c("wt", "mpg"), target = "gear", plot_type="boxplot")
#'
#'@export



plot_num_plots <- function(df, variables = colnames(df),
                           target, plot_type = "boxplot") {
  if (!is.data.frame(df))
    stop("df is not a data frame!")
  if (!(target %in% colnames(df)))
    stop("Incorrect target column")
  if (length(unique(df[,target]))  > 6)
    stop("Function only for classification task")
  if (!is.character(variables))
    stop("variables is not a character vector!")
  else if (!all(variables %in% colnames(df))) {
    message("chosen variables do not exist in dataframe, all variables will be taken")
    variables <- colnames(df)
  }

  if (!(plot_type %in% c("boxplot", "violin")) ){
    message("Only boxplot and violin possible. Drawing boxplot.")
    plot_type <- "boxplot"
  }
  #select only numeric columns (and chosen by user)
  df_user <- df[,variables]
  num_cols <- unlist(lapply(df_user, is.numeric))
  df_num <- df_user[ , num_cols]


  create_one_plot <- function(colname) {



    p <- ggplot(data = NULL, aes(x = as.factor(df[,target]),
                                 y =df[,colname] ) )

    if(plot_type == "boxplot"){
      p <- p + geom_boxplot()
    }else if (plot_type == "violin"){
      p <- p + geom_violin()
    }
    one_plot <- p + labs(x = target,
           y = colname,
           title = paste0(colname, " by ", target)) +
            theme_torpeda()
    one_plot
    }

  #creating numerical plots for each num. column
  plots <- lapply(colnames(df_num), create_one_plot)
  #display four plots on each page (using patchwork)

  #number of variables to plot
  n_variables <- length(plots)

  #number of pages
  n_plots <- floor(n_variables / 4) + sign(n_variables %% 4)

  #empty list for pages
  plots_list <- vector(mode = "list", length = n_plots)
  j <- 1

  if (n_variables > 3) { # if not, we go to the last page
    for (i in seq(1, n_variables - n_variables %% 4, by = 4)) {
      plots_list[[j]] <-
        (plots[[i]] + plots[[i + 1]]) / (plots[[i + 2]] + plots[[i + 3]])
      j <- j + 1
    }
  }
  #last page (1, 2, or 3 plots)
  if (n_variables %% 4 == 1) {
    plots_list[[j]] <- plots[[n_variables]]
  } else if (n_variables %% 4 == 2) {
    plots_list[[j]] <- plots[[n_variables]] + plots[[n_variables - 1]]
  } else if (n_variables %% 4 == 3) {
    plots_list[[j]] <-
      plots[[n_variables]] + plots[[n_variables - 1]] + plots[[n_variables -
                                                                 2]]
  }


  plots_list
}
