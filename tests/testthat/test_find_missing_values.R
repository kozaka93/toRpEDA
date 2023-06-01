
df <- data.frame(data.frame(a = c(2,5,4,NA, 5,7,NA), b =  c(NA,NA,2,NA, 5,7,3),
                             c = c(23,5,4,1, 5,7,NA), d =  c(NA,NA,2,NA, 5,7,5),
                             e = c(4,5,4,NA, 0,7,NA), f =  c(1,123,2,87, 5,7,6),
                             g = c(253,NA,NA,NA, NA,NA,NA), h =  c(3,34,2,5, 5,7,56)))
library(ggplot2)
library(testthat)
test_that("Nie podano ramki danych",
          {expect_error(find_missing_values(c(1,2,3)))})

test_that("Bledna nazwa kolumny",
          {expect_error(find_missing_values(df,variable = c("a", "e", "r") ))})

test_that("Wybrano zbyt duza liczba kolumn",
          {expect_error(find_missing_values(df,variable = c("a", "e", "b", "c", "d", "f", "g", "h", "i" ,"l")))})


test_that("Podano poprawne dane",
         {df_true <- data.frame("Column_name" = c("a", "b", "c", "d", "e", "f", "g", "h"),
                                "Number_of_missing_values" = c(2,3,1,3,2,0,6,0))
         expect_equal(find_missing_values(df)$Table$`Column name`, df_true$Column_name)
         expect_equal(find_missing_values(df)$Table$`Number of missing values`, df_true$Number_of_missing_values)
         })

test_that("Podano poprawne dane",
          {df_true2 <- data.frame("Column_name" = c("a"),
                                  "Number_of_missing_values" = c(2))
          expect_equal(find_missing_values(df, variable = "a")$Table$`Column name`, df_true2$Column_name)
          expect_equal(find_missing_values(df, variable = "a")$Table$`Number of missing values`, df_true2$Number_of_missing_values)
          })

test_that("Podano poprawne dane",
          {df_true3 <- data.frame("Column_name" = c("a", "c"),
                                  "Number_of_missing_values" = c(2,1))
          expect_equal(find_missing_values(df, variable = c("a", "c"))$Table$`Column name`, df_true3$Column_name)
          expect_equal(find_missing_values(df, variable = c("a", "c"))$Table$`Number of missing values`, df_true3$Number_of_missing_values)
          })

