#' nfl positionModel Correlations for Algos testing/machineLearning Models Function
#'
#' @param position QB/RB/WR/TE/DST
#' @param modelYear 2015-2018
#' @param modelWeek 1-17
#'
#' @return list of correls for each model
#' @export
#'
#' @examples nflPositionModelCorrels(position="QB",modelYear=2018,modelWeek=3)
nflPositionModelCorrels<-function(position="QB",modelYear=2018,modelWeek=3){

modelList<-list("Main_gbmtuneUniq2.csv","Main_treebagtuneUniq2.csv","Main_ridgetuneUniq2.csv","Main_icrtuneUniq2.csv","Main_cubisttune.csv")

correls<-foreach(j=1:5) %do% {
  projections<-read.csv(paste0("~/NFL_Daily/dfsProjections/",position,"/",modelYear,"/",modelWeek,"/",modelList[j]))
  corrr<-cor(na.What(projections$ActualPoints),projections$projection)
}
#returnNames<-c(paste0(position,modelWeek,modelList[1]),paste0(position,modelWeek,modelList[2]),paste0(position,modelWeek,modelList[3]),paste0(position,modelWeek,modelList[4]),paste0(position,modelWeek,modelList[5]))
returnNames<-c(paste0(position,modelList[1]),paste0(position,modelList[2]),paste0(position,modelList[3]),paste0(position,modelList[4]),paste0(position,modelList[5]))


returnDf<-data.frame(correls[1],correls[2],correls[3],correls[4],correls[5])
names(returnDf)<-returnNames
return(returnDf)

}
