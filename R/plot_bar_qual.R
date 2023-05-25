#' This function returns bar plots of quality columns of a dataframe
#'
#' @param df A data frame
#' @param variables A vector of names (or numbers) of columns of which the user wants to create bar plots
#'
#' @return Bar plots in k rows and length(variables)/k columns where k is the largest
#' divisor of the number of columns fewer than the number of chosen columns
#'
#' @examples plot_bar_qual(iris, 5)
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#' @export
plot_bar_qual <- function(df, variables = NULL){
  if(is.null(variables)){
    return(NULL)
  }
  for(i in variables){
    if (class(df[,i]) == 'numeric'){
      stop("One of the variables is not a quality variable.")
    }
  }
  k <- 1
  dl <- length(variables)
  if(dl == 1){
    k <- 1
  } else {
  for(i in 1:(dl-1)){
    if(dl%% i == 0){
      k <- i
    } else{
      k <- k
    }
  }
  }
  frame <- as.data.frame(df %>%
    select(variables))
  cols <- colnames(frame)
  p<- list()
  for(i in 1:dl){
    p[[i]] <- ggplot(data = frame,
                     aes_string(x = names(frame %>% select(get('i'))))) +
      geom_bar(fill = 'royalblue1') +
      labs(x = cols[i],
           y = 'Count',
           title = paste("Distribution of a column", cols[i]))+
           theme_torpeda()
  }
  wrap_plots(p, nrow = k, ncol = dl/k)
}
