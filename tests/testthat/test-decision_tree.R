

test_that("errors work", {
  expect_error(decision_tree("a"))
  expect_error(decision_tree(iris, "potato"))
  expect_error(decision_tree(iris, target = "garlic"))
  expect_error(decision_tree(iris, classification = "tomato"))
  expect_error(decision_tree(iris, showplot="cucumber"))
  expect_error(decision_tree(iris, maxdepth="onion"))
  expect_error(decision_tree(iris, minsplit="carrot"))
  expect_error(decision_tree(iris, seed="pickle"))
  expect_error(decision_tree(iris, cp="pumpkin"))
  expect_error(decision_tree(iris, xval="beetroot"))
  expect_error(decision_tree(iris[c("Species")]))
})

test_that("messages work", {
  expect_message(decision_tree(iris))
  expect_message(decision_tree(iris, minsplit = 2.7))
  expect_message(decision_tree(iris, xval = 1.7))
  expect_message(decision_tree(iris, maxdepth = 3.7))


})

test_that("accuracy measure works", {
  expect_type(decision_tree(iris), "list")
  expect_equal(decision_tree(iris)$accuracy, 0.977777, tolerance = 0.001)
  expect_equal(decision_tree(iris)$balanced_accuracy, 0.9814815, tolerance = 0.001)

})

test_that("regression task", {
  d <- USArrests
  d$Rape <- as.character(d$Rape)

  expect_message(decision_tree(d, classification=FALSE))
  expect_equal(decision_tree(USArrests, classification=FALSE)$RMSE, 52.99113, tolerance = 0.001)
})
