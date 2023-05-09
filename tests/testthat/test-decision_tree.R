


test_that("czy działa wszystko szmrra", {
  expect_equal(corr_pearson(c(1, 2, 3), c(1, 2, 3)), 1)
  expect_equal(corr_pearson(c(1, 2, 3), c(1, 2, 3)), 1)
  expect_equal(corr_spearman(c(1, 2, 3), c(3, 2, 1)),-1)
  expect_equal(corr_spearman(c(1, 2, 3), c(3, 2, 1)),-1)
})
