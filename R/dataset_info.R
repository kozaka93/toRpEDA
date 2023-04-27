dataset_info <- function(df,info=TRUE){
  rown <- nrow(df)
  coln <- ncol(df)
  types <- sapply(df,typeof)
  #text<-
  if (info){
    cat("\n Informations about your dataframe:\n", 
    "Number of rows: ",rown,"\n",
    "number of columns: ",coln,"\n\n",
    "Types of columns:\n ")
    for(i in 1:coln){
      cat("-",names(types[i])," - ",types[i],"\n ")
    }
    }
  res<-list(nrow=rown,ncol=coln,types=types)
  return(invisible(res))
}
