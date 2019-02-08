#' weighted Predictions
#'
#' @param weights c(...,...,...)
#' @param predictions list(...,...,...)
#'
#' @return weighted predictions
#' @export
#'
#' @examples weightedPredctions(weights=c(0.58,0.39),predictions=list(preds,preds1))
weightedPredictions<-function(weights=c(0.58,0.39),predictions=list(...)){

weightsT<-weights
predsT<-c(predictions)

predsList<-list(w=weightsT,p=predsT)

w<-weightsT
p<-predsT

weightedPredictions<-foreach(i=1:length(predsList),.combine = '+') %do% {
  w[i] * sapply(p[i],as.numeric)
}
weightedPreds<-sapply(weightedPredictions,as.numeric)
return(weightedPreds)
}
