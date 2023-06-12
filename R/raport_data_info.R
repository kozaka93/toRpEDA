#' Generate report_data_info
#'
#' @param df A data frame.
#' @param variables A vector includes names of columns .
#' @param output_file A string giving the name of the output file. Default is NULL.
#' @param output_dir A string giving the directory where the output file will be saved. Default is the current working directory.
#'
#' @return Report generated to the local file. It contains information about variables types, data dimensions,
#' missing values, potential ID/redundant columns, outliers and mismatched value types
#' metrics for
#'
#' @export
#'
#' @examples
#' \dontrun{
#' report_data_info(iris, output_file = "iris_report.html")
#' }
#'
#'

raport_data_info <-
  function(df,
           variables = NULL,
           output_file = NULL,
           output_dir = getwd()) {

    if (is.null(variables)){
      variables = colnames(df)
    }


    input_file_path <- system.file("rmd/raport_data_info", "report_data_info.Rmd", package = "toRpEDA")

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
