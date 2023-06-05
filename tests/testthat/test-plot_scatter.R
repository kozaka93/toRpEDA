test_that("parameters correctness", {
  df <- infert
  df['target'] <- as.character(rbinom(dim(df)[1], 1, 0.3))
  df2 <- data.frame(
    target=rbinom(5, 1, 0.5),
    country=c("Poland", "Germany", "Germany", "Poland", "Italy"),
    humidity=rnorm(5, 20, 10),
    temperature=rbinom(5, 20, 10),
    city=c("Warsaw", "Berlin", "Munich", "Lublin", "Rome"),
    season=c("Summer", "Fall", "Winter", "Fall", "Spring")
  )
  df3 <- data.frame(
    col1=rnorm(20),
    col2=rnorm(20),
    col3=rnorm(20),
    col4=rnorm(20),
    col5=rnorm(20),
    col6=rnorm(20),
    col7=rnorm(20),
    col8=rnorm(20),
    col9=rnorm(20)
  )
  expect_error(plot_scatter(c(1,2,3)))
  expect_error(plot_scatter())
  expect_error(plot_scatter(df, target=12))
  expect_error(plot_scatter(df, target=c('target', 'case')))
  expect_error(plot_scatter(df, target='age'))
  expect_error(plot_scatter(df, c('age', 'parity', "temp")))
  expect_warning(plot_scatter(df, c('education', 'age', 'stratum', 'pooled.stratum')))
  expect_warning(plot_scatter(df, c('case', 'age', 'stratum', 'pooled.stratum')))
  expect_error(plot_scatter(df2))
  expect_error(plot_scatter(df, c('stratum', 'target', 'education')))
  expect_error(plot_scatter(df2, c('country', 'season', 'city')))
  expect_warning(plot_scatter(df3))
})

test_that('Return type', {
  df <- infert
  df['target'] <- as.character(rbinom(dim(df)[1], 1, 0.3))
  expect_type(plot_scatter(df, target='target'), 'list')
})
