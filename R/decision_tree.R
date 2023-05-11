#' decision_tree`()` Builds decision tree on given data. Returns metrics for the tree and optionally plots the tree.
#'
#' @param df Data frame.
#' @param target Column name which values function predicts. By default the last column
#' @param categorical If `TRUE`, function builds categorical tree, builds regression tree otherwise.
#' If building regression tree and target column is not numeric, casting the column to numeric values.
#' `TRUE` by default.
#'
#' @param showplot If `TRUE` displays decision tree with plot function from rpart.plot package. By default `TRUE`
#'
#' @param maxdepth Maximum depth of decision tree. Default value is `10`
#' @param minsplit The minimum number of observations that must exist in a node in order for a split to be attempted. Default value is `20`
#' @param cp Complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
#' The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile.
#' Essentially,the user informs the program that any split which does not improve the fit by cp will likely be pruned off by cross-validation, and that hence the program need not pursue it.
#' Default value is `0.01`
#' @param xval Number of cross-validation. Default value is `5`
#'
#'
#' @param seed Random seed, provides repeatability for the tree output. By default `44`
#'
#' @examples
#' library("toRpEDA")
#' decision_tree(iris, "Species")
#' decision_tree(USArrests, categorical = FALSE)
#'
#' @export

decision_tree <- function(df, target = NULL, categorical = TRUE, showplot=TRUE, maxdepth = 10, minsplit = 20, cp = 0.01, xval = 5, seed=44) {
  if(!is.data.frame(df))
    stop("df is not a data frame!")

  if(ncol(df) < 2)
    stop("number of columns is not sufficient")


  if(is.null(target)) {# check if target exists, if not assign the last colname
    target = tail(colnames(df), n=1)
    message(paste0("set target value to ", target))
  }

  if(!target %in% colnames(df))
    stop("target is not one of df columns")
  if(!is.logical(categorical))
    stop("categorical is not logical")

  if(!is.logical(showplot))
    stop("showplot is not logical")

  if(!is.numeric(maxdepth)) # i'm not sure if I should check all the possibilites - the rpart function should handle the errors
    stop("maxdepth is not numeric")
  if(!is.numeric(minsplit))
    stop("minsplit is not numeric")
  if(!is.numeric(xval))
    stop("xval is not integer")

  if(!is.numeric(cp))
    stop("xval is not numeric")

  if(!is.numeric(seed) || seed != round(seed))
    stop("seed is not integer")

  if(!categorical && !is.numeric(df[, target])) { # if regression model
    message("target is not numeric, converting to numeric")

    df[, target] <- as.numeric(df[ , target])
  }

  if(minsplit != round(minsplit)){
    minsplit = floor(minsplit)
    message("minsplit is not integer, rounded the value")
  }

  if(maxdepth != round(maxdepth)){
    maxdepth = floor(maxdepth)
    message("maxdepth is not integer, rounded the value")
  }

  if(xval != round(xval)){
    xval = floor(xval)
    message("xval is not integer, rounded the value")
  }


  set.seed(seed) # for repeatability

  target_formula <- paste(target) # maybe here I can add some more options, like adding more columns or sth
  target_formula <- as.formula(paste(target_formula, "~ ."))

  # splitting the data

  sample_size <- 0.7

  sample <- sample.int(n = nrow(df), size = floor(sample_size*nrow(df)), replace = F)
  train <- df[sample, ]
  test  <- df[-sample, ]

  method <- ifelse(categorical, "class", "anova")

  # grow the decision tree
  tree <- rpart::rpart(target_formula,
                      data = train,
                      method = method,
                      control = rpart::rpart.control(maxdepth=maxdepth, minsplit=minsplit, cp=cp, xval=xval))




  # check if tree exists
  if(nrow(tree$frame) > 1)  {
    if(showplot){
      rpart.plot::rpart.plot(tree,
                             main = paste0("Decision tree for target column ", target),
                             prefix="target = ",       # prefix text in first line in node
                             type=2,                   # 2: split variable name under box, 5: split variable name in the interior nodes
                             yesno=2,                  # show yes/no at each node
                             #extra=107,                # 106 = % observations + target
                             branch=0,                 # 0 = V shaped, 1 = squared
                             branch.type=1,            # 5 = proportional width
                             box.palette = 'Greens',    # colors for nodes
                             shadow.col = 0,           # color of shadow, 0 = none
                             cex = 0.7,
                             left = TRUE)
    } # plot tree

    # calculate metrics for decision tree

    metrics <- list()

    # predictions
    pred_method <- ifelse(categorical, "class", "vector")

    y_real <- test[, target]
    y_pred <- predict(tree, test, type=pred_method)

    # calculating support
    metrics$train_support <- nrow(train)
    metrics$test_support <- nrow(test)


    if(categorical == TRUE) {
      t <- table(y_real,y_pred)
      metrics$accuracy <- sum(diag(t))/sum(t)

      # balanced accuracy

      per_class = diag(t) / colSums(t)
      if(any(is.na(per_class)))
        message("y_pred contains classes not in y_true")
      per_class <-  per_class[!is.na(per_class)]
      metrics$balanced_accuracy = mean(per_class)

      return(metrics)

    }
    else { # if categorical == FALSE
      # calculate metrics for regression model


      # RMSE
      metrics$RMSE <- mean((y_real - y_pred)^2)


      return(metrics)
    } # endif categorical

  } else {
    stop("unable to grow decision tree")
  }

}
