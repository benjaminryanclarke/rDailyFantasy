#' caret model fit function
#'
#' @param X data
#' @param Y target Variable
#' @param algo caret method to use
#' @param pre preProcess params
#' @param seedNum seed number
#' @param para parallel? T/F
#'
#' @return trainModel
#' @export
#'
#' @examples noControlModel(X=mlbAllBats,Y=ActualPoints,algo="lmStepAIC",seedNum=14,para=TRUE)
noControlModel<-function(X=trainX,Y=train$ActualPoints,algo="lm",pre=c("center","scale","YeoJohnson"),trMethod=NULL,seedNum=14){

  set.seed(seedNum)

fitControl<-trainControl(method="none", verboseIter = TRUE, allowParallel = TRUE)
if(!is.null(trMethod)){
  fitControl<-trainControl(verboseIter = TRUE, allowParallel = TRUE)
}

suppressWarnings(if(!is.null(pre)){
trainModel<-train(x=X,y=Y,
                  method=algo,
                  trControl=fitControl,
                  preProcess=pre,
                  )
}else{
  trainModel<-train(x=X,y=Y,
                    method=algo,
                    trControl=fitControl,
                    )
})
return(trainModel)
}


