#' decision_tree`()` suggests redundant columns.
#'
#' @param df Data frame.
#' @param target Column name which values function predicts. By default the last column
#' @param categorical If `TRUE`, function builds categorical tree, builds regression tree otherwise
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
#' @examples
#' library("toRpEDA")
#' decision_tree(iris, "Species")
#' decision_tree(iris, delete = TRUE)
#'
#' @export

decision_tree <- function(df, target = NULL, categorical = TRUE, showplot=TRUE, maxdepth = 10, minsplit = 20, cp = 0.01, xval = 5, seed=44) {
  if(!is.data.frame(df))
    stop("df is not a data frame!")
  if(is.null(target)) # check if target exists, if not assign the last colname
    target = tail(colnames(df), n=1)
  if(!target %in% colnames(df))
    stop("target is not one of df columns")
  if(!is.logical(categorical))
    stop("categorical is not logical")

  if(!is.logical(showplot))
    stop("showplot is not logical")

  if(!is.numeric(maxdepth) || maxdepth != round(maxdepth)) # i'm not sure if I should check all the possibilites - the rpart function should handle the errors
    stop("maxdepth is not integer")
  if(!is.numeric(minsplit) || minsplit != round(minsplit)) # i'm not sure if I should check all the possibilites - the rpart function should handle the errors
    stop("minsplit is not integer")
  if(!is.numeric(xval) || xval != round(xval)) # i'm not sure if I should check all the possibilites - the rpart function should handle the errors
    stop("xval is not integer")

  if(!is.numeric(cp)) # i'm not sure if I should check all the possibilites - the rpart function should handle the errors
    stop("xval is not numeric")

  if(!is.numeric(seed) || seed != round(seed)) # i'm not sure if I should check all the possibilites - the rpart function should handle the errors
    stop("seed is not integer")

  set.seed(seed)

  target_formula <- paste(target) # TODO maybe add some more options, like adding more columns or sth
  target_formula <- as.formula(paste(target_formula, "~ ."))


  # perhaps here we should here split the data

  sample_size <- 0.7

  sample <- sample.int(n = nrow(df), size = floor(sample_size*nrow(df)), replace = F)
  train <- df[sample, ]
  test  <- df[-sample, ]

  #

  # grow the decision tree
  mod <- rpart::rpart(target_formula,
                      data = train,
                      method = "class",
                      control = rpart::rpart.control(maxdepth=maxdepth, minsplit=minsplit, cp=cp, xval=xval))




  # check if tree exists
  if(nrow(mod$frame) > 1)  {
    if(showplot){
      rpart.plot::rpart.plot(mod,
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

    # calculate indexes for decision trees

    tmp <- mod$cptable
    r_square <- as.vector(1-tmp[,c(4)])
    r_square <- tail(r_square, 1)

    t_val <- test[, target]

    t_pred <- predict(mod, test, type="class")

    confMat <- table(t_val,t_pred)

    accuracy <- sum(diag(confMat))/sum(confMat)



    return(r_square) # for now returns only R^2 value


  } else {
    plot_text("can't grow decision tree")
    stop("unable to frow decision tree")
  }

}
