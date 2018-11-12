#' nfl teamStats dataSet Cleanup Function
#'
#' @param modelYear 2018
#' @param modelWeek 1-17
#'
#' @return NULL
#' @export
#'
#' @examples nflTeamStatsCleanup(modelYear=2018,modelWeek=1)
nflTeamStatsCleanup<-function(modelYear=2018,modelWeek=4){
origDir<-getwd()
setwd(paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/"))
teamN<-list.files()
foreach(i=1:length(teamN)) %do% if(file.exists(paste0(teamN[i],"/teamStats.csv"))){
  teamDir<-getwd()
  setwd(paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",teamN[i],"/"))
  system(command = "rm teamStats.csv")
  setwd(teamDir)
}
setwd(origDir)
}
