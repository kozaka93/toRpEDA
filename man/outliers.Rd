% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers.R
\name{outliers}
\alias{outliers}
\title{Function that finds potential outlier observations}
\usage{
outliers(df, variables = NULL)
}
\arguments{
\item{df}{A dataframe}

\item{variables}{column names of given dataframe to process. By default all colnames of dataframe.}
}
\value{
An information about outliers as a message
It informed about outliers in individual columns and in all dataset.
}
\description{
Function that finds potential outlier observations
}
\examples{
library(isotree)
library(toRpEDA)
library(glue)

outliers(iris)

a <- c(1,2,3,42,1,1,0)
outliers(a)
}
