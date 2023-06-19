#' Generate a report with statistics on the data that includes basic descriptive 
#' statistics, a histogram and correlation matrix for quantitative variables, 
#' and barplots for qualitative variables.
#'
#' @param df A data frame for which the report is to be generated.
#' @param variables Variables for which the report is to be generated.
#' @param output_file A string giving the name of the output file. Default is NULL.
#' @param output_dir A string giving the directory where the output file will be saved. Default is the current working directory.
#'
#' @return Report is saved as local file. It contains basic descriptive statistics, 
#' a histogram and correlation matrix for quantitative variables, and barplots 
#' for qualitative variables.
#' @export
#' @import dplyr
#' 
#' @examples
#' \dontrun{
#' report_statistics(iris, output_file = "iris_statistics_report.html")
#' }

report_statistics <-
  function(df,
           variables = NULL,
           output_file = NULL, 
           output_dir = getwd()) {
    
    if (is.null(variables))
      
      input_file_path <- system.file('rmd/report_statistics', 'report_statistics.Rmd', package = 'toRpEDA')
    
    rmarkdown::render(
      input         = input_file_path,
      output_format = "html_document",
      output_file   = output_file,
      output_dir    = output_dir,
      params        = list(
        df          = df,
        variables   = variables
      )
    )
  }