test_that("report_statistics generates a report", {
  output_file <- "Test_report_statistics.html"
  output_dir = getwd()
  report_statistics(iris, output_file = output_file)
  expect_true(file.exists(output_file))
  expect_true(file.size(output_file) > 0)
  unlink(output_file)
})