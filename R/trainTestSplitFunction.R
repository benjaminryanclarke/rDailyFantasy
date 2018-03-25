#' train Test Data Split for Machine Learning
#'
#' @param x dataFrame 
#' @param sp training split percent
#' @param targetVar target Variable
#'
#' @return list...train,test,trainTarget,testTarget
#' @export
#'
#' @examples trainTestSplit(pgaPointsAllData,sp=0.75,targetVar="ActualPoints")
trainTestSplit <- function(x,sp=0.75,targetVar=NULL){
  trainingRowInd<-caret::createDataPartition(y=x[,1],p=sp,list = FALSE)
  tra<-x[trainingRowInd,]
  tes<-x[-trainingRowInd,]
  ret<-list(train=data.frame(tra),test=data.frame(tes))
  
  if(!is.null(targetVar)){
  tra.target<-data.frame(tra,stringsAsFactors = F)
  tes.target<-data.frame(tes,stringsAsFactors = F)
  tra.target<-tra.target[,targetVar]
  tes.target<-tes.target[,targetVar]
  ret<-list(train=data.frame(tra),test=data.frame(tes),trainTarget=tra.target,testTarget=tes.target)
  }
  return(ret)
}