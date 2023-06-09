% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_scatter.R
\name{plot_scatter}
\alias{plot_scatter}
\title{Plot scatter plots for continuous variables}
\usage{
plot_scatter(df, vars, target = NULL)
}
\arguments{
\item{df}{Data frame}

\item{vars}{(optonal) Vector with names of columns to include on plots.}

\item{target}{(optional) Name of target column to perform grouping on scatter plot.}
}
\value{
patchwork's list of ggplot objects
}
\description{
Function for displaying scatter plots of contninuous variables in data frame.
Automatically picks columns of data frame and displays unique variables pairs
scatter plots on one multi-faceted graphic. Column names can be specified. Color grouping by
target can be added in case of classification task.
}
\examples{
df <- attitude
df['target'] <- as.factor(rbinom(30, 1, 0.3))

plot_scatter(df, vars=c('rating', 'complaints', 'privileges', 'learning'))
plot_scatter(df, target='target')
}
