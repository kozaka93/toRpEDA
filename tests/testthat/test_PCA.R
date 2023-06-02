test_that("Czy wykrywa, że obiekt nie jest dataframe", {expect_error(PCA(c(1,2,3)))})
test_that("Czy wykrywa NULL/NA 1", {expect_error(PCA(data.frame(x1 = c(1,2,NULL))))})
test_that("Czy wykrywa NULL/NA 2", {expect_error(PCA(data.frame(x1 = c(1,2,NA))))})
test_that("Numeryczne", {expect_error(data.frame(x1 = c("xdxd",2,4)))})
