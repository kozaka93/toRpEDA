

test_that("errors work", {

  expect_error(decision_tree("mayo (4 spoons)"))
  expect_error(decision_tree(iris, variables=2)) #  carrots
  expect_error(decision_tree(iris, target = "2 parsleys"))
  expect_error(decision_tree(iris, classification = "1 celery"))
  expect_error(decision_tree(iris, showplot="3 big potatoes"))
  expect_error(decision_tree(iris, maxdepth="4 eggs"))
  expect_error(decision_tree(iris, minsplit="1 apple (optional, but recommended)"))
  expect_error(decision_tree(iris, seed="2 pickled cucumbers"))
  expect_error(decision_tree(iris, cp="1 can of peas"))
  expect_error(decision_tree(iris, xval="1 onion"))

  expect_error(decision_tree(iris[c("Species")]))
})

test_that("messages work", {
  expect_message(decision_tree(iris))
  expect_message(decision_tree(iris, minsplit = 2.7))
  expect_message(decision_tree(iris, xval = 1.7))
  expect_message(decision_tree(iris, maxdepth = 3.7))
  expect_message(decision_tree(iris, variables = c("hard-boil eggs",
                                                   "boil potatoes",
                                                   "boil carrots, parlsey and celery, you can use boiled vegetables for broth",
                                                   "peel eggs and potatoes",
                                                   "finely chop the onions and cut other veggies, eggs and apple into cubes",
                                                   "mix all the ingredients and add mayo",
                                                   "season everything with salt and pepper and serve cooled",
                                                   "you can decorate it with parsley and a piece of hard-boiled egg.")))

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
