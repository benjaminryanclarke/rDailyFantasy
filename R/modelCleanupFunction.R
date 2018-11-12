#' ressearch Moddel Cleanup Function
#'
#' @param mod dataframe to evaluate
#'
#' @return preProcessed Data for mLearn
#' @export
#'
#' @examples researchModelCleanup(qbAll)
researchModelCleanup <- function(mod = rbAll){
  #fullModelCleanup
  modNums <- numericOnly(mod) %>%  dplyr::select(-X,-PlayerId,-ContestGroupId,-GameCount) %>% nzv.filter()
  modNums$ActualPoints <- modNums$ActualPoints %>% na.What()
  #createTrainTestSamples
  inTrain <- caret::createDataPartition(modNums$ActualPoints,p=0.75,list=FALSE)
  train <- modNums[inTrain,]
  test <- modNums[-inTrain,]
  #get column umber of targetVar
  targetCol <- which(colnames(train)=="ActualPoints")
  #define preProc for preProcessing models
  preProc <- caret::preProcess(train[,-targetCol], method = c("YeoJohnson", "center", "scale", "knnImpute"))
  #tranform,knnImpute,center,scale missing values in train/test
  training <- predict(preProc, train[,-targetCol])
  training$ActualPoints <- train$ActualPoints
  testing <- predict(preProc, test[,-targetCol])
  testing$ActualPoints <- test$ActualPoints
  return(list(train=training,test=testing,preProc=preProc))
}
