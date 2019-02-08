#' testPredsFunction
#'
#' @param nbaModel model
#' @param wts c(...,...,...)
#'
#' @return stats for predictons
#' @export
#'
#' @examples testPreds(nabModel=nba19,wts=c(0.3,0.6,0.1))
testPreds<-function(nbaModel=nba19,wts=c(0.3,0.6,0.1)){
  ##nbaCubist
  cbMod<-predict(preProcNBA,na.What(nbaModel))
  cbPreds<-predict(nbaCubist,cbMod)

  ##xgb.actualPoints.big.full.final
  xgbMod<-suppressWarnings(vtreat::prepare(treatplanBig, nbaModel, varRestriction = new_varsBig) %>% as.matrix())
  xgbPreds<-predict(xgb.actualPoints.big.full.final,xgbMod)

  ##glmnet
  nbaModel$p_own <- as.factor(nbaModel$p_own)
  nbaModel$Days_Between_games <- as.factor(nbaModel$Days_Between_games)
  nbaModel$Depth <- as.factor(nbaModel$Depth)
  nbaModel$ProTrends_DK <- as.factor(nbaModel$ProTrends_DK)
  nbaModel$ProTrends_FD <- as.factor(nbaModel$ProTrends_FD)
  nbaModel$Salary <- as.factor(nbaModel$Salary)
  nbaModel$Salary_DK <- as.factor(nbaModel$Salary_DK)
  nbaModel$Salary_FD <- as.factor(nbaModel$Salary_FD)
  nbaModel$Trend <- as.factor(nbaModel$Trend)
  nbaModel$PlayerId <- as.factor(nbaModel$PlayerId)
  glmMod <- suppressWarnings(vtreat::prepare(treatplanActualPoints, nbaModel, varRestriction = new_varsActualPoints) %>% as.matrix())
  glmPreds<-predict(glmnetnbafull2_3,glmMod)
  glmPreds<-sapply(glmPreds,as.numeric)




  weightsT<-wts
  predsT<-c(list(cbPreds,xgbPreds,glmPreds))

  predsList<-list(weights=weightsT,preds=predsT)

  weightedPs<-foreach(i=1:length(predsList),.combine = '+') %do% {
    weightsT[i] * sapply(predsT[i],as.numeric)
  }

  rmse<-RMSE(na.What(nbaModel$ActualPoints),weightedPs)
  rmse1<-RMSE(na.What(nbaModel$ActualPoints),cbPreds)
  rmse2<-RMSE(na.What(nbaModel$ActualPoints),xgbPreds)
  rmse3<-RMSE(na.What(nbaModel$ActualPoints),glmPreds)
  cor<-cor(na.What(nbaModel$ActualPoints),weightedPs)
  cor1<-cor(na.What(nbaModel$ActualPoints),cbPreds)
  cor2<-cor(na.What(nbaModel$ActualPoints),xgbPreds)
  cor3<-cor(na.What(nbaModel$ActualPoints),glmPreds)
  nbaModelRMSLE<-nbaModel
  nbaModelRMSLE$ActualPoints[which(nbaModel$ActualPoints<0)]<-0
  rmsle<-mlr::measureRMSLE(na.What(nbaModelRMSLE$ActualPoints),weightedPs)
  rmsle1<-mlr::measureRMSLE(na.What(nbaModelRMSLE$ActualPoints),cbPreds)
  rmsle2<-mlr::measureRMSLE(na.What(nbaModelRMSLE$ActualPoints),xgbPreds)
  rmsle3<-mlr::measureRMSLE(na.What(nbaModelRMSLE$ActualPoints),glmPreds)

  model<-c('optimal','nbaCubist','xgb','glmnet')
  RMSE<-c(rmse,rmse1,rmse2,rmse3)
  COR<-c(cor,cor1,cor2,cor3)
  RMSLE<-c(rmsle,rmsle1,rmsle2,rmsle3)

  out<-data.frame("model"=model[1:4],"RMSE"=RMSE[1:4],"COR"=COR[1:4],"RMSLE"=RMSLE[1:4])


  return(out)
}
