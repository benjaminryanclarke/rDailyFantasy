#' mlb pitcher model training function quickCaret
#'
#' @param algo model to use
#' @param dat the data
#'
#' @return caret model
#' @export
#'
#' @examples mlbArmsTrain(algo='blackboost',dat=train.Actual.Arms)
mlbArmsTrain<-function(algo='blackboost',dat=train.Actual.Arms){
  set.seed(414)
  model_Arms = train(ActualPoints ~ ., data=dat, method=algo, preProc=c('knnImpute','YeoJohnson'),na.action = na.pass)
  return(model_Arms)
}
