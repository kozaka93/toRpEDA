

test_that("errors work", {
  expect_error(decision_tree("a"))
  expect_error(decision_tree(iris, "potato"))
  expect_error(decision_tree(iris, target = "Species"))
  expect_error(decision_tree(iris, categorical = "tomato"))
  expect_error(decision_tree(iris, showplot="cucumber"))
  expect_error(decision_tree(iris, maxdepth="onion"))
  expect_error(decision_tree(iris, minsplit="carrot"))
  expect_error(decision_tree(iris, seed="pickle"))
  expect_error(decision_tree(iris[c("Species")]))
})

test_that("messages work", {
  expect_message(decision_tree(iris))
  expect_message(decision_tree(iris, minsplit = 1.5))
})

test_that("accuracy measure works", {
  expect_type(decision_tree(iris), "list")
  expect_equal(decision_tree(iris)$accuracy, 0.977777, tolerance = 0.001)
})
