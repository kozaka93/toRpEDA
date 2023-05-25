#' This function returns principal components and their associeted eigenvectors - PCA
#'
#' @param df A data frame
#' @param standardize - boolean - whether you want to automatically standardize your data
#' @param plot_explained_variance - boolean - whether you want to plot the proportion of explained variance by next vectors
#'
#' @returns The function returns eigenvectors with their according principal components in PCA. Additionally displays a plot of explained variance in data.
#' @examples
#' library(ggplot2)
#' PCA(mtcars, standardize = TRUE, plot_explained_variance = TRUE)
#' @import ggplot2
#' @export


PCA <- function(df, standardize = TRUE, plot_explained_variance = TRUE){
  n <- nrow(df)
  p <- ncol(df)
  # standardize and plot_explained_variance are boolean type
  if(!is.logical(standardize) | !is.logical(plot_explained_variance)){
    stop("'standardize' and 'plot_explained_variance' must be of logical type!")
  }
  # Is df a data frame
  if(!is.data.frame(df)){
    stop("Argument 'df' is not a data frame!")
  }
  # Are all columns numeric?
  for(i in 1:p){
    if(!is.numeric(df[,i])){
      stop("All columns of your data frame must be numeric!")
    }
  }
  # Are there Nulls/NaNs?
  if(sum(is.na(df)) | sum(is.null(df))){
    stop("Your dataframe contains NA/Null values!")
  }
  # This part standardizes columns of the data frame
  if(standardize){
    sigmas <- as.numeric(lapply(df,sd))
    means <- as.numeric(lapply(df, mean))
    for (i in 1:p) {
      df[,i] <- (df[,i] - means[i])/sigmas[i]
    }
    message("The results are calculated for a standardized version of your data frame!")
  }

  # Calculation of eigenvalues and eigenvectors
  cov_matrix <- as.matrix(cov(df))
  eig <- eigen(cov_matrix)
  answer <- list(Eigenvalues = eig$values,
                          Eigenvectors = eig$vectors)
  # Calculating percentage of total variance explained + plotting
  if(plot_explained_variance){
    tr <- sum(diag(cov_matrix))
    numerki <- 1:p
    ev <- data.frame(explained_variance = answer$Eigenvalues/tr,index = numerki)
    plot <-ggplot(ev, aes(x = numerki,y = explained_variance)) + geom_col() +
      ggtitle("Fraction of total variance explained by following eigenvectors:") +
      xlab("Eigenvector index")
    print(plot)
  }
  answer
}
