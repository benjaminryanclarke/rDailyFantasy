#' Nfl Model Import Function
#'
#' @param pastEvent TRUE/FALSE
#' @param cookie labsCookies
#' @param labModel name of fantasyLab model to use
#' @param postRefresh reRun for misc postTourney stats(1/0)[T/F]
#' @param parallelRun running with doSNOW in parallel TRUE/FALSE
#' @param modelWeek 1:17
#' @param modelYear 2015:2018
#' @param qbOnly T/F
#' @param rbOnly T/F
#' @param wrOnly T/F
#' @param teOnly T/F
#' @param dstOnly T/F
#' @param systemRefresh refresh lab models data(1/0)[T/F]
#'
#' @return nfl Models
#' @export
#'
#' @examples nflModelSetup(modelWeek=1,modelYear=2015,cookie=nflCookies)
nflModelSetup<-function(modelWeek=1,modelYear=2018,pastEvent=TRUE,labModel="2018",postRefresh=1,systemRefresh=1,parallelRun=FALSE,qbOnly=FALSE,rbOnly=FALSE,wrOnly=FALSE,teOnly=FALSE,dstOnly=FALSE,cookie=nflCookies){

  require(foreach,quietly = TRUE,warn.conflicts = F)
  require(doSNOW,quietly = TRUE,warn.conflicts = F)
  require(qdapRegex,quietly = TRUE,warn.conflicts = F)
  require(caret,quietly = TRUE,warn.conflicts = F)
  require(dplyr,quietly = TRUE,warn.conflicts = F)
  require(tidyr,quietly = TRUE,warn.conflicts = F)
  require(lubridate,quietly = TRUE,warn.conflicts = F)

  nfl2014Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2014/09/03", weeks = 17)
  nfl2015Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2015/09/09", weeks = 17)
  nfl2016Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2016/09/07", weeks = 17)
  nfl2017Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2017/09/07", weeks = 17)
  nfl2018Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2018/09/05", weeks = 17)
  nflDates <- data.frame("2014"=nfl2014Dates,"2015"=nfl2015Dates,"2016"=nfl2016Dates,"2017"=nfl2017Dates,"2018"=nfl2018Dates)
  nflDateColumn <- dplyr::if_else(modelYear==2014,1,dplyr::if_else(modelYear==2015,2,dplyr::if_else(modelYear==2016,3,dplyr::if_else(modelYear==2017,4,dplyr::if_else(modelYear==2018,5,0)))))
  modelDate <- nflDates[,nflDateColumn][modelWeek]



  setwd("~/NFL_Daily")
  #removeOldDataFiles
  suppressMessages(system(command = paste0("cd ~/NFL_Daily/ && rm -f NFL",modelDate,".csv")))
  suppressMessages(system(command = "cd ~/NFL_Daily/json/ && rm -rf *"))

  if(parallelRun==FALSE){

    #check for cookies data
    if(!exists("nflCookies")){
      readline("missing Cookies, attempting to get them now!")
      nflCookies<-rDailyFantasy::labsCookies()$labs
      cookie<-nflCookies

      assign("nflCookies",nflCookies,envir = .GlobalEnv)
    }
  }
  #Search for sourceData
  sourceDataByDate<-list.files(path = paste0("~/NFL_Daily/sourceData/",modelYear,"/"))
  sourceData<-dplyr::if_else(modelWeek %in% sourceDataByDate,TRUE,FALSE)
  if(postRefresh==TRUE && sourceData==TRUE){
    suppressMessages(system(command = paste0("cd ~/NFL_Daily/sourceData/",modelYear,"/"," && rm -R ",modelWeek)))
  }
  sourceDataByDate<-list.files(path = paste0("~/NFL_Daily/sourceData/",modelYear,"/"))
  #if no sourceData Exists...
  sourceData<-dplyr::if_else(modelWeek %in% sourceDataByDate,TRUE,FALSE)
  if(sourceData==FALSE){
    dir.create(paste0("~/NFL_Daily/sourceData/",modelYear,"/",modelWeek),showWarnings = FALSE)
    sourceUrl<- paste0("https://www.fantasylabs.com/api/sourcedata/1/",modelDate)
    setwd("~/NFL_Daily/json/")
    y3 <- paste0("sourceData",modelDate,".json")
    pmCurlHandles3 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y3," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",sourceUrl," ",pmCurlHandles3)
    system(curlAddress,ignore.stderr = TRUE)
    sourceData <- jsonlite::fromJSON(y3, flatten = TRUE)
    setwd("~/NFL_Daily")
    #### ### ## #
    #sourcedataParsingEditing
    #eventsData <- data.frame(dplyr::bind_rows(sourceData$ContestGroups$Events),stringsAsFactors=FALSE)# %>% dplyr::select(-IsPrimary)
    #if("IsPrimary" %in% names(eventsData)){
    #  eventsData <- eventsData %>% dplyr::select(-IsPrimary)
    #}
    sourceData2<- data.frame(sourceData$ContestGroups,stringsAsFactors = FALSE) %>% dplyr::select(-Events) %>% dplyr::filter(SourceId==4)
    #fullSourceDataWithEvents<-left_join(sourceData2,eventsData,by="ContestGroupId") %>% dplyr::select(-AdminEdit,-IsProjected,-IsPrimary,-IsOpen)
    fullSourceData<-sourceData2 %>% dplyr::select(-AdminEdit,-IsProjected,-IsPrimary,-IsOpen)
    #fullSourceDataWithEvents$ContestSuffix<-dplyr::if_else(fullSourceDataWithEvents$ContestSuffix=="","Main",unlist(qdapRegex::ex_bracket(fullSourceDataWithEvents$ContestSuffix)))
    fullSourceData$ContestSuffix<-dplyr::if_else(fullSourceData$ContestSuffix=="","Main",unlist(qdapRegex::ex_bracket(fullSourceData$ContestSuffix)))
    write.csv(fullSourceData,file=paste0("~/NFL_Daily/sourceData/",modelYear,"/",modelWeek,"/","fullSourceData.csv"))
    #write.csv(fullSourceDataWithEvents,file=paste0("~/NFL_Daily/sourceData/",modelDate,"/fullSourceDataWithEvents.csv"))
    #create all contest directories
    sourceGroupList<-c(unique(fullSourceData$ContestSuffix))
    nothing<-suppressMessages(foreach::foreach(i=1:length(sourceGroupList)) %do% {
      dir.create(paste0("~/NFL_Daily/sourceData/",modelYear,"/",modelWeek,"/",sourceGroupList[i]),showWarnings = FALSE)
      sourceD <- fullSourceData %>% dplyr::filter(ContestSuffix==sourceGroupList[i])
      #sourceD <- data.frame(fullSourceData[i,],stringsAsFactors=FALSE)
      write.csv(sourceD,file=paste0("~/NFL_Daily/sourceData/",modelYear,"/",modelWeek,"/",sourceGroupList[i],"/source.csv"))
    })
  }

  suppressMessages(system(command = "cd ~/NFL_Daily/json/ && rm -rf *"))

  #importSourceData
  sourceDataIn<-read.csv(file=paste0("~/NFL_Daily/sourceData/",modelYear,"/",modelWeek,"/","fullSourceData.csv"),stringsAsFactors = FALSE) #%>% dplyr::select(ContestGroupId,ContestSuffix,HomeTeam,StadiumName,)
  #sourceDataInForLogs<-read.csv(file=paste0("~/NFL_Daily/sourceData/",modelDate,"/fullSourceData.csv"),stringsAsFactors = FALSE) %>% dplyr::select(StadiumName,SportEventId,ContestSuffix) %>% unique()
  #names(sourceDataInForLogs)<-c("StadiumName","EventId","ContestSuffix")

  #### ### ## #
  #modelId setup__SYSTEMS*
  if(systemRefresh==TRUE || "systemsQb.csv" %!in% list.files(path="~/NFL_Daily")){
    systemsURL <- paste0("https://www.fantasylabs.com/api/systems/models/1/")
    setwd("~/NFL_Daily/json/")
    y4 <- paste0("systems.json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y4," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",systemsURL," ",pmCurlHandles)
    #download systems json to json folder
    system(curlAddress,ignore.stderr = TRUE)
    systemsDf <- jsonlite::fromJSON(paste0("~/NFL_Daily/json/",y4),simplifyDataFrame = TRUE)
    setwd("~/NFL_Daily")
    systemIds <- systemsDf$Models$SystemId
    systemNames <- systemsDf$Models$SystemName
    allSystems<- data.frame(sysNames=systemNames,SystemId=systemIds)
    modelId<-allSystems[which(allSystems$sysNames==labModel),][2][[1]]
    systemsResultsQb <- data.frame(systemsDf$Models$SystemResults$`4101`)
    systemsResultsRb <- data.frame(systemsDf$Models$SystemResults$`4102`)
    systemsResultsWr <- data.frame(systemsDf$Models$SystemResults$`4103`)
    systemsResultsTe <- data.frame(systemsDf$Models$SystemResults$`4104`)
    systemsResultsDst <- data.frame(systemsDf$Models$SystemResults$`4105`)
    systemsCompleteQb <- dplyr::inner_join(allSystems,systemsResultsQb,by="SystemId")
    systemsCompleteRb <- dplyr::inner_join(allSystems,systemsResultsRb,by="SystemId")
    systemsCompleteWr <- dplyr::inner_join(allSystems,systemsResultsWr,by="SystemId")
    systemsCompleteTe <- dplyr::inner_join(allSystems,systemsResultsTe,by="SystemId")
    systemsCompleteDst <- dplyr::inner_join(allSystems,systemsResultsDst,by="SystemId")
    write.csv(systemsCompleteQb,file = "~/NFL_Daily/systemsQb.csv")
    write.csv(systemsCompleteRb,file = "~/NFL_Daily/systemsRb.csv")
    write.csv(systemsCompleteWr,file = "~/NFL_Daily/systemsWr.csv")
    write.csv(systemsCompleteTe,file = "~/NFL_Daily/systemsTe.csv")
    write.csv(systemsCompleteDst,file = "~/NFL_Daily/systemsDst.csv")

    suppressMessages(system(command = "cd ~/NFL_Daily/json/ && rm -rf *"))
  }
  systemssss<-read.csv(file = "~/NFL_Daily/systemsQb.csv",stringsAsFactors = F)
  allSystems<- data.frame(sysNames=systemssss$sysNames,SystemId=systemssss$SystemId)
  modelId<-allSystems[which(allSystems$sysNames==labModel),][2][[1]]


  #downloadMatchupData
  nflMatchupTeams<-read.csv(file="~/NFL_Daily/nflMatchupTeams.csv") %>% dplyr::select(-X)
  scheduleGrid<-read.csv(file=paste0("~/NFL_Daily/dataSets/",modelYear,"/scheduleGrid.csv"))
  onBye<- rownames(scheduleGrid[scheduleGrid[,modelWeek+1]=="BYE",])
  onBye2<- if(purrr::is_empty(rownames(scheduleGrid[scheduleGrid[,modelWeek+1]=="BYE",]))){
    scheduleGrid
  }else{
    scheduleGrid[-c(as.numeric(onBye)),]
  }
  mad<-modelWeek+1
  onBye3<-onBye2[,mad]
  onBye3<-data.frame("abr"=onBye3)
  onBye3$abr <-gsub('@',"",onBye3$abr)
  nflMatchupTeams<-dplyr::inner_join(onBye3,nflMatchupTeams,by="abr")


  foreach(m=1:nrow(nflMatchupTeams)) %do% {
    teamAPIUrl<- paste0("https://www.fantasylabs.com/api/matchups/1/team/",nflMatchupTeams[m,2],"/",modelDate)
    setwd("~/NFL_Daily/json/")
    saveN<-nflMatchupTeams[m,3]
    y3 <- paste0(saveN,modelDate,".json")
    pmCurlHandles3 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y3," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",teamAPIUrl," ",pmCurlHandles3)
    system(curlAddress,ignore.stderr = TRUE)
    y4<-paste0("NA",modelDate,".json")
    teamAPIData <- jsonlite::fromJSON(y3)
    setwd("~/rDFs/rDailyFantasy")
    matchupProperties<-teamAPIData$TeamMatchups$Properties
    playerMatchupProperties<-teamAPIData$PlayerMatchups$Properties %>% data.frame()
    fixx <- grep("News",x = names(playerMatchupProperties),fixed=TRUE)
    playerMatchupProperties <- playerMatchupProperties[,-fixx]
    teamStatsProperties<-teamAPIData$TeamStats$Properties
    dir.create(path = paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek),showWarnings = FALSE)

    dir.create(path = paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData"),showWarnings = FALSE)
    dir.create(path = paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",saveN),showWarnings = FALSE)
    write.csv(matchupProperties,file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",saveN,"/teamMatchup.csv"))
    write.csv(playerMatchupProperties,file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",saveN,"/playerMatchup.csv"))
    #write.csv(teamStatsProperties,file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",saveN,"/teamStats.csv"))
    if(!file.exists(paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/teamStats.csv"))){
    write.csv(teamStatsProperties,file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/teamStats.csv"))
    }
  }

  matchUpTeamsList<-nflMatchupTeams$saveN

  #matchupData import
  playerMatchupIn<-foreach(j=1:length(matchUpTeamsList),.combine = 'rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",matchUpTeamsList[j],"/playerMatchup.csv")) %>% dplyr::select(-X)
  teamMatchupIn<-foreach::foreach(j=1:length(matchUpTeamsList),.combine='rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",matchUpTeamsList[j],"/teamMatchup.csv")) %>% dplyr::select(-X)
  #teamStatsIn<-foreach::foreach(j=1:length(matchUpTeamsList),.combine='rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/",matchUpTeamsList[j],"/teamStats.csv")) %>% dplyr::select(-X)
  teamStatsIn<-read.csv(file=paste0("~/NFL_Daily/dataSets/",modelYear,"/",modelWeek,"/matchupData/teamStats.csv")) %>% dplyr::select(-X)

  qbVariableNames <- rDailyFantasy::loadRData(fileName = "~/NFL_Daily/machineLearning/QB/modelVariables/qbVariables.rda")
  rbVariableNames <- rDailyFantasy::loadRData(fileName = "~/NFL_Daily/machineLearning/RB/modelVariables/rbVariables.rda")
  wrVariableNames <- rDailyFantasy::loadRData(fileName = "~/NFL_Daily/machineLearning/WR/modelVariables/wrVariables.rda")
  teVariableNames <- rDailyFantasy::loadRData(fileName = "~/NFL_Daily/machineLearning/TE/modelVariables/teVariables.rda")
  dstVariableNames <- rDailyFantasy::loadRData(fileName = "~/NFL_Daily/machineLearning/DST/modelVariables/dstVariables.rda")


  #duplicateVariables for each position in projection model already
  qbVarsDupl<-qbVariableNames[qbVariableNames %in% names(playerMatchupIn)]
  rbVarsDupl<-rbVariableNames[rbVariableNames %in% names(playerMatchupIn)]
  wrVarsDupl<-wrVariableNames[wrVariableNames %in% names(playerMatchupIn)]
  teVarsDupl<-teVariableNames[teVariableNames %in% names(playerMatchupIn)]
  dstVarsDupl<-dstVariableNames[dstVariableNames %in% names(playerMatchupIn)]


  qbMatchupModelBig<-playerMatchupIn %>% dplyr::filter(PositionType=="qb-1") %>% select(-qbVarsDupl) %>% mutate(ActualPoints_DK_RnkPct=NA,ActualPoints_DK_Rnk=NA,TourneyOwnership_DK_RnkPct=NA,TourneyOwnership_DK_Rnk=NA,t_own_DK_RnkPct=NA,t_own_DK_Rnk=NA)
  rbMatchupModelBig<-playerMatchupIn %>% dplyr::filter(PositionType=="rb-1" | PositionType=="rb-2") %>% select(-rbVarsDupl) %>% mutate(ActualPoints_DK_RnkPct=NA,ActualPoints_DK_Rnk=NA,TourneyOwnership_DK_RnkPct=NA,TourneyOwnership_DK_Rnk=NA,t_own_DK_RnkPct=NA,t_own_DK_Rnk=NA)
  wrMatchupModelBig<-playerMatchupIn %>% dplyr::filter(PositionType=="wr-1" | PositionType=="wr-2" | PositionType=="wr-3") %>% select(-wrVarsDupl) %>% mutate(ActualPoints_DK_RnkPct=NA,ActualPoints_DK_Rnk=NA,TourneyOwnership_DK_RnkPct=NA,TourneyOwnership_DK_Rnk=NA,t_own_DK_RnkPct=NA,t_own_DK_Rnk=NA)
  teMatchupModelBig<-playerMatchupIn %>% dplyr::filter(PositionType=="te-1") %>% select(-teVarsDupl) %>% mutate(ActualPoints_DK_RnkPct=NA,ActualPoints_DK_Rnk=NA,TourneyOwnership_DK_RnkPct=NA,TourneyOwnership_DK_Rnk=NA,t_own_DK_RnkPct=NA,t_own_DK_Rnk=NA)
  dstMatchupModelBig<-playerMatchupIn %>% dplyr::filter(is.na(PositionType)) %>% select(-dstVarsDupl) %>% mutate(ActualPoints_DK_RnkPct=NA,ActualPoints_DK_Rnk=NA,TourneyOwnership_DK_RnkPct=NA,TourneyOwnership_DK_Rnk=NA,t_own_DK_RnkPct=NA,t_own_DK_Rnk=NA)





  #setup and run webdriver to get full fantasylab data models and import into environment
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/1/",modelDate,"/?modelid=",modelId)
  #change directory to json folder
  setwd("~/NFL_Daily/json/")
  y <- paste0("NFL",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",modelURL," ",pmCurlHandles)
  #download Model json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  #read in json Model to parse
  nflModelAll <- jsonlite::fromJSON(y, flatten = FALSE)
  setwd("~/NFL_Daily")

  suppressMessages(system(command = "cd ~/NFL_Daily/json/ && rm -rf *"))

  #cgResults <- nflModelAll$ContestGroupFantasyResults
  cgResults <- nflModelAll$ContestGroupFantasyResults #%>% dplyr::select(-PlayerId)
  cgResults <- dplyr::inner_join(sourceDataIn,cgResults,by="ContestGroupId") %>% dplyr::select(-SourceId,-X)
  allModelQb<- nflModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4,PositionId=="101")
  allModelRb<- nflModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4,PositionId=="102")
  allModelWr<- nflModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4,PositionId=="103")
  allModelTe<- nflModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4,PositionId=="104")
  allModelDst<- nflModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4,PositionId=="105")
  cgAddQb<- dplyr::inner_join(allModelQb,cgResults,by=c("FantasyResultId","PlayerId"))
  cgAddRb<- dplyr::inner_join(allModelRb,cgResults,by=c("FantasyResultId","PlayerId"))
  cgAddWr<- dplyr::inner_join(allModelWr,cgResults,by=c("FantasyResultId","PlayerId"))
  cgAddTe<- dplyr::inner_join(allModelTe,cgResults,by=c("FantasyResultId","PlayerId"))
  cgAddDst<- dplyr::inner_join(allModelDst,cgResults,by=c("FantasyResultId","PlayerId"))
  #make final join to original sourceData for complete Data
  #completeModelHitters<-left_join(cgAddHitters,sourceDataIn,by="ContestGroupId") %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-") %>% dplyr::select(-maxOwn)
  #completeModelPitchers<-left_join(cgAddPitchers,sourceDataIn,by="ContestGroupId") %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-") %>% dplyr::select(-maxOwn)
  #completeModel$p_own<-unlist(lapply(completeModel$p_own,as.numeric))
  completeModelQb<-cgAddQb %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)
  completeModelRb<-cgAddRb %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)
  completeModelWr<-cgAddWr %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)
  completeModelTe<-cgAddTe %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)
  completeModelDst<-cgAddDst %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)

  completeModelQbOwnCheck<-dplyr::if_else(length(dplyr::contains("+",vars = completeModelQb$p_own))>0,1,0)
  completeModelRbOwnCheck<-dplyr::if_else(length(dplyr::contains("+",vars = completeModelRb$p_own))>0,1,0)
  completeModelWrOwnCheck<-dplyr::if_else(length(dplyr::contains("+",vars = completeModelWr$p_own))>0,1,0)
  completeModelTeOwnCheck<-dplyr::if_else(length(dplyr::contains("+",vars = completeModelTe$p_own))>0,1,0)
  completeModelDstOwnCheck<-dplyr::if_else(length(dplyr::contains("+",vars = completeModelDst$p_own))>0,1,0)

  if(completeModelQbOwnCheck==1){
    completeModelQb<-completeModelQb %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }
  if(completeModelRbOwnCheck==1){
    completeModelRb<-completeModelRb %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }
  if(completeModelWrOwnCheck==1){
    completeModelWr<-completeModelWr %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }
  if(completeModelTeOwnCheck==1){
    completeModelTe<-completeModelTe %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }
  if(completeModelDstOwnCheck==1){
    completeModelDst<-completeModelDst %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }

  completeModelQb$p_own<-as.numeric(completeModelQb$p_own)
  completeModelRb$p_own<-as.numeric(completeModelRb$p_own)
  completeModelWr$p_own<-as.numeric(completeModelWr$p_own)
  completeModelTe$p_own<-as.numeric(completeModelTe$p_own)
  completeModelDst$p_own<-as.numeric(completeModelDst$p_own)

  #completeModelHitters$p_own<-suppressWarnings(unlist(lapply(completeModelHitters$p_own,as.numeric)))
  #completeModelPitchers$p_own<-suppressWarnings(unlist(lapply(completeModelPitchers$p_own,as.numeric)))


  #cleanUpData
  completeModelNumsQb<-completeModelQb[sapply(completeModelQb,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmQbNumsLinCombs<-caret::findLinearCombos(completeModelNumsQb)
  #cleanedNumsQb<-completeModelNumsQb[,-cmQbNumsLinCombs$remove]
  cleanedNumsQb<-completeModelNumsQb
  if("PlayerId" %in% names(cleanedNumsQb)==FALSE){
    cleanedNumsQb<-cleanedNumsQb %>% dplyr::mutate(PlayerId=cgAddQb$PlayerId)
  }
  #cleanedNumsQb<-cleanedNumsQb[apply(cleanedNumsQb,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNumsQb)==FALSE){
    cleanedNumsQb<-cleanedNumsQb %>% dplyr::mutate(ContestGroupId=cgAddQb$ContestGroupId)
  }
  cleanNamesQb<- list(c("Player_Name"),names(completeModelQb[,names(completeModelQb) %in% names(cleanedNumsQb)]))
  cleanedQb<-completeModelQb %>% dplyr::select(unlist(cleanNamesQb)) %>% dplyr::select(-GameCount,-DraftGroupId)
  ###FinalQb_noPost
  #numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  ##suppressMessages(allModelQb$p_own<-as.numeric(allModelQb$p_own))

  allModelQb<-allModelQb %>% dplyr::select(-p_own)





  allFinalQb<-suppressMessages(dplyr::left_join(allModelQb,completeModelQb))
  #____________________________
  allFinalQb<-allFinalQb %>% dplyr::select(Score,Ceiling,ProjectedSacks,LeveragePct,OppPlusMinus,OppPts,SpreadPct,RushingSuccessfulAllowedPct,TouchdownAllowedPct,ReceivingYardsMarketShare,OffensiveSnapsMarketShare,PassYards,AdjYPA,PassingLong,PassingAttemptsTEPct,PassSackPct,RushYards,RushingSuccessfulPct,ReceivingYardsPerReception,OpportunitiesRedZone10,RushingSuccessfulPctRedZone,RedZoneTouchdownPct,Season_PPG,Season_X1,ContestGroupId,Salary,ProjPlusMinus,ActualPoints,Trend,OffensiveSnapsPlayed,Run_Change,InterceptionPct,SackPct,YardsPerPassingAttempt,RushingTouchdownsMarketShare,PassingCompletions,PassingCompletionPercentage,PassingTouchdowns,PassingAttemptsRBPct,PassTDPct,RushingAttempts,RushingYardsPerAttempt,ReceivingYards,RedZoneSnaps,OpportunitiesRedZone5,RedZoneSnaps10,Temperature,Season_Plus_Minus,Season_X2,AvgPts,PtVal,ImpPts,Site_Salary,Pts,Total,PassingSuccessfulPct,TakeawayPct,ReceivingTargetsMarketShare,RushingYardsMarketShare,PassingAttempts,PassingYardsPerAttempt,PassingRating,PassingAttemptsWRPct,PassINTPct,RushingLong,RushingTouchdowns,ReceivingLong,OpportunitiesRedZone,RushingTouchdownsRedZonePct,RedZoneSnaps5,Wind_Speed,Season_FPSnap,Season_X0,GameCount,Player_Name,ContestSuffix,PlayerId,p_own,PositionId,Team,LeveragePct,FantasyResultId)

  allFinalQB2 <- dplyr::left_join(allFinalQb,qbMatchupModelBig,by="PlayerId") %>% dplyr::select(-FullName)
  qbFDRemove<-grep(pattern = "_FD",names(allFinalQB2),fixed = TRUE)
  qbFDRemove2<-grep(pattern = "_FD_",names(allFinalQB2),fixed = TRUE)
  qbFDRemove3<-c(qbFDRemove,qbFDRemove2)
  allFinalQb<-allFinalQB2[,-qbFDRemove3]
  #allFinalQb<-left_join(cleanedQb,cgResults,by = c("PlayerId", "FantasyResultId", "ContestGroupId","SportId"))
  #suppressWarnings(allFinalQb<-allFinalQb %>% tidyr::separate(Player_Name,into=c("Player_Name","L/R"),sep = " \\(") %>% tidyr::separate(`L/R`,into=c("L/R"),sep = "\\)"))
  allDataQb<-allFinalQb %>% dplyr::filter(!is.na(ContestGroupId)) %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  #left_join(allModelQb,completeModelQb)
  #hittersCSV <- allFinalQb %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #allDataQb <- hittersCSV

  #cleanUpData
  completeModelNumsRb<-completeModelRb[sapply(completeModelRb,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmRbNumsLinCombs<-caret::findLinearCombos(completeModelNumsRb)
  #cleanedNumsRb<-completeModelNumsRb[,-cmRbNumsLinCombs$remove]
  cleanedNumsRb<-completeModelNumsRb
  if("PlayerId" %in% names(cleanedNumsRb)==FALSE){
    cleanedNumsRb<-cleanedNumsRb %>% dplyr::mutate(PlayerId=cgAddRb$PlayerId)
  }
  #cleanedNumsRb<-cleanedNumsRb[apply(cleanedNumsRb,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNumsRb)==FALSE){
    cleanedNumsRb<-cleanedNumsRb %>% dplyr::mutate(ContestGroupId=cgAddRb$ContestGroupId)
  }
  cleanNamesRb<- list(c("Player_Name"),names(completeModelRb[,names(completeModelRb) %in% names(cleanedNumsRb)]))
  cleanedRb<-completeModelRb %>% dplyr::select(unlist(cleanNamesRb)) %>% dplyr::select(-GameCount,-DraftGroupId)
  ###FinalRb_noPost
  #numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  ##suppressMessages(allModelRb$p_own<-as.numeric(allModelRb$p_own))

  allModelRb<-allModelRb %>% dplyr::select(-p_own)





  allFinalRb<-suppressMessages(dplyr::left_join(allModelRb,completeModelRb))
  allFinalRb<-allFinalRb %>% dplyr::select(Score,Salary,AvgPts,Ceiling,ProjPlusMinus,PtVal,ProjectedSacks,ActualPoints,ImpPts,LeveragePct,Trend,Site_Salary,OppPlusMinus,OffensiveSnapsPlayed,Pts,OppPts,Run_Change,Total,SpreadPct,InterceptionPct,PassingSuccessfulPct,RushingSuccessfulAllowedPct,SackPct,TakeawayPct,TouchdownAllowedPct,YardsPerPassingAttempt,ReceivingTargetsMarketShare,ReceivingTouchdownsMarketShare,ReceivingYardsMarketShare,RushingTouchdownsMarketShare,RushingYardsMarketShare,OffensiveSnapsMarketShare,PassingAttempts,PassingRating,PassingAttemptsWRPct,RushingAttempts,RushingLong,RushYards,RushingYardsPerAttempt,RushingTouchdowns,RushingSuccessfulPct,ReceiveTgts,Receptions,ReceivingYards,ReceivingLong,ReceivingYardsPerReception,ReceivingTouchdowns,ReceivingYardsPerTarget,RedZoneSnaps,OpportunitiesRedZone,OpportunitiesRedZone10,OpportunitiesRedZone5,RushingTouchdownsRedZonePct,ReceivingTouchdownsRedZonePct,RushingSuccessfulPctRedZone,RedZoneSnaps10,RedZoneSnaps5,RedZoneTouchdownPct,Temperature,Wind_Speed,Humidity,PrecipProb,Season_PPG,Season_Plus_Minus,Season_FPSnap,Season_X1,Season_X2,Season_X0,Season_Count,ProjPlaysPct,OppPlusMinusPct,MktShrPct,ProjPct,PtPerDPct,FloorPct,ProjPlusMinusPct,CeilingPct,Upside,Consistency,Season_PPG_Percentile,Pro_Pct,Plus_Minus,PlayerId,ContestGroupId,GameCount,Player_Name,ContestSuffix,p_own,PositionId,Team,LeveragePct,FantasyResultId)
  #allFinalRb<-left_join(cleanedRb,cgResults,by = c("PlayerId", "FantasyResultId", "ContestGroupId","SportId"))
  #suppressWarnings(allFinalRb<-allFinalRb %>% tidyr::separate(Player_Name,into=c("Player_Name","L/R"),sep = " \\(") %>% tidyr::separate(`L/R`,into=c("L/R"),sep = "\\)"))

  allFinalRB2 <- dplyr::left_join(allFinalRb,rbMatchupModelBig,by="PlayerId") %>% dplyr::select(-FullName)
  rbFDRemove<-grep(pattern = "_FD",names(allFinalRB2),fixed = TRUE)
  rbFDRemove2<-grep(pattern = "_FD_",names(allFinalRB2),fixed = TRUE)
  rbFDRemove3<-c(rbFDRemove,rbFDRemove2)
  allFinalRb<-allFinalRB2[,-rbFDRemove3]

  allDataRb<-allFinalRb %>% dplyr::filter(!is.na(ContestGroupId)) %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  #left_join(allModelRb,completeModelRb)
  #hittersCSV <- allFinalRb %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #allDataRb <- hittersCSV

  #cleanUpData
  completeModelNumsWr<-completeModelWr[sapply(completeModelWr,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmWrNumsLinCombs<-caret::findLinearCombos(completeModelNumsWr)
  #cleanedNumsWr<-completeModelNumsWr[,-cmWrNumsLinCombs$remove]
  cleanedNumsWr<-completeModelNumsWr
  if("PlayerId" %in% names(cleanedNumsWr)==FALSE){
    cleanedNumsWr<-cleanedNumsWr %>% dplyr::mutate(PlayerId=cgAddWr$PlayerId)
  }
  #cleanedNumsWr<-cleanedNumsWr[apply(cleanedNumsWr,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNumsWr)==FALSE){
    cleanedNumsWr<-cleanedNumsWr %>% dplyr::mutate(ContestGroupId=cgAddWr$ContestGroupId)
  }
  cleanNamesWr<- list(c("Player_Name"),names(completeModelWr[,names(completeModelWr) %in% names(cleanedNumsWr)]))
  cleanedWr<-completeModelWr %>% dplyr::select(unlist(cleanNamesWr)) %>% dplyr::select(-GameCount,-DraftGroupId)
  ###FinalWr_noPost
  ##numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  ##suppressMessages(allModelWr$p_own<-as.numeric(allModelWr$p_own))

  allModelWr<-allModelWr %>% dplyr::select(-p_own)





  allFinalWr<-suppressMessages(dplyr::left_join(allModelWr,completeModelWr))
  allFinalWr<-allFinalWr %>% dplyr::select(Score,Salary,AvgPts,Ceiling,Floor,ProjPlusMinus,PtVal,ProjectedSacks,ActualPoints,ImpPts,LeveragePct,Trend,Site_Salary,OppPlusMinus,OffensiveSnapsPlayed,Pts,OppPts,Run_Change,Total,SpreadPct,InterceptionPct,PassingSuccessfulPct,RushingSuccessfulAllowedPct,SackPct,TakeawayPct,TouchdownAllowedPct,YardsPerPassingAttempt,ReceivingTargetsMarketShare,ReceivingTouchdownsMarketShare,ReceivingYardsMarketShare,OffensiveSnapsMarketShare,PassingCompletions,PassingAttempts,PassYards,PassingCompletionPercentage,PassingYardsPerAttempt,AdjYPA,PassingTouchdowns,PassingRating,PassingLong,PassingAttemptsRBPct,PassingAttemptsWRPct,PassTDPct,PassSackPct,RushingLong,RushYards,RushingYardsPerAttempt,RushingSuccessfulPct,ReceiveTgts,Receptions,ReceivingYards,ReceivingLong,ReceivingYardsPerReception,ReceivingTouchdowns,ReceivingYardsPerTarget,RedZoneSnaps,OpportunitiesRedZone,OpportunitiesRedZone10,OpportunitiesRedZone5,RushingTouchdownsRedZonePct,ReceivingTouchdownsRedZonePct,RushingSuccessfulPctRedZone,RedZoneSnaps10,RedZoneSnaps5,RedZoneTouchdownPct,Temperature,Wind_Speed,Humidity,PrecipProb,Season_PPG,Season_Plus_Minus,Season_FPSnap,Season_X1,Season_X2,Season_X0,Season_Count,ProjPlaysPct,OppPlusMinusPct,MktShrPct,ProjPct,PtPerDPct,FloorPct,ProjPlusMinusPct,CeilingPct,Vegas,Upside,Consistency,Season_PPG_Percentile,Plus_Minus,PlayerId,ContestGroupId,GameCount,Player_Name,ContestSuffix,p_own,PositionId,Team,LeveragePct,FantasyResultId)
  #allFinalWr<-left_join(cleanedWr,cgResults,by = c("PlayerId", "FantasyResultId", "ContestGroupId","SportId"))
  #suppressWarnings(allFinalWr<-allFinalWr %>% tidyr::separate(Player_Name,into=c("Player_Name","L/R"),sep = " \\(") %>% tidyr::separate(`L/R`,into=c("L/R"),sep = "\\)"))

  allFinalWR2 <- dplyr::left_join(allFinalWr,wrMatchupModelBig,by="PlayerId") %>% dplyr::select(-FullName)
  wrFDRemove<-grep(pattern = "_FD",names(allFinalWR2),fixed = TRUE)
  wrFDRemove2<-grep(pattern = "_FD_",names(allFinalWR2),fixed = TRUE)
  wrFDRemove3<-c(wrFDRemove,wrFDRemove2)
  allFinalWr<-allFinalWR2[,-wrFDRemove3]

  allDataWr<-allFinalWr %>% dplyr::filter(!is.na(ContestGroupId)) %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  #left_join(allModelWr,completeModelWr)
  #hittersCSV <- allFinalWr %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #allDataWr <- hittersCSV

  #cleanUpData
  completeModelNumsTe<-completeModelTe[sapply(completeModelTe,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmTeNumsLinCombs<-caret::findLinearCombos(completeModelNumsTe)
  #cleanedNumsTe<-completeModelNumsTe[,-cmTeNumsLinCombs$remove]
  cleanedNumsTe<-completeModelNumsTe
  if("PlayerId" %in% names(cleanedNumsTe)==FALSE){
    cleanedNumsTe<-cleanedNumsTe %>% dplyr::mutate(PlayerId=cgAddTe$PlayerId)
  }
  #cleanedNumsTe<-cleanedNumsTe[apply(cleanedNumsTe,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNumsTe)==FALSE){
    cleanedNumsTe<-cleanedNumsTe %>% dplyr::mutate(ContestGroupId=cgAddTe$ContestGroupId)
  }
  cleanNamesTe<- list(c("Player_Name"),names(completeModelTe[,names(completeModelTe) %in% names(cleanedNumsTe)]))
  cleanedTe<-completeModelTe %>% dplyr::select(unlist(cleanNamesTe)) %>% dplyr::select(-GameCount,-DraftGroupId)
  ###FinalTe_noPost

  ##suppressMessages(allModelTe$p_own<-as.numeric(allModelTe$p_own))

  allModelTe<-allModelTe %>% dplyr::select(-p_own)





  allFinalTe<-suppressMessages(dplyr::left_join(allModelTe,completeModelTe))
  allFinalTe<-allFinalTe %>% dplyr::select(Score,Salary,AvgPts,Ceiling,PtVal,ProjectedSacks,ActualPoints,ImpPts,LeveragePct,Trend,Site_Salary,OppPlusMinus,OffensiveSnapsPlayed,Pts,OppPts,Run_Change,Total,SpreadPct,InterceptionPct,PassingSuccessfulPct,RushingSuccessfulAllowedPct,SackPct,TakeawayPct,TouchdownAllowedPct,YardsPerPassingAttempt,ReceivingTargetsMarketShare,ReceivingTouchdownsMarketShare,ReceivingYardsMarketShare,RushingYardsMarketShare,OffensiveSnapsMarketShare,RushingYardsPerAttempt,RushingSuccessfulPct,ReceiveTgts,Receptions,ReceivingYards,ReceivingLong,ReceivingYardsPerReception,ReceivingTouchdowns,ReceivingYardsPerTarget,RedZoneSnaps,OpportunitiesRedZone,OpportunitiesRedZone10,OpportunitiesRedZone5,ReceivingTouchdownsRedZonePct,RedZoneSnaps10,RedZoneSnaps5,RedZoneTouchdownPct,Temperature,Wind_Speed,Humidity,PrecipProb,Season_PPG,Season_Plus_Minus,Season_FPSnap,Season_X1,Season_X2,Season_X0,Season_Count,ProjPlaysPct,OppPlusMinusPct,MktShrPct,ProjPct,PtPerDPct,FloorPct,ProjPlusMinusPct,CeilingPct,Vegas,Upside,Consistency,Season_PPG_Percentile,Pro_Pct,PlayerId,ContestGroupId,GameCount,Player_Name,ContestSuffix,p_own,PositionId,Team,LeveragePct,FantasyResultId)

  allFinalTE2 <- dplyr::left_join(allFinalTe,teMatchupModelBig,by="PlayerId") %>% dplyr::select(-FullName)
  teFDRemove<-grep(pattern = "_FD",names(allFinalTE2),fixed = TRUE)
  teFDRemove2<-grep(pattern = "_FD_",names(allFinalTE2),fixed = TRUE)
  teFDRemove3<-c(teFDRemove,teFDRemove2)
  allFinalTe<-allFinalTE2[,-teFDRemove3]
  #allFinalTe<-left_join(cleanedTe,cgResults,by = c("PlayerId", "FantasyResultId", "ContestGroupId","SportId"))
  #suppressWarnings(allFinalTe<-allFinalTe %>% tidyr::separate(Player_Name,into=c("Player_Name","L/R"),sep = " \\(") %>% tidyr::separate(`L/R`,into=c("L/R"),sep = "\\)"))
  allDataTe<-allFinalTe %>% dplyr::filter(!is.na(ContestGroupId)) %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  #left_join(allModelTe,completeModelTe)
  #hittersCSV <- allFinalTe %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #allDataTe <- hittersCSV


  #cleanUpData
  completeModelNumsDst<-completeModelDst[sapply(completeModelDst,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmDstNumsLinCombs<-caret::findLinearCombos(completeModelNumsDst)
  #cleanedNumsDst<-completeModelNumsDst[,-cmDstNumsLinCombs$remove]
  cleanedNumsDst<-completeModelNumsDst
  if("PlayerId" %in% names(cleanedNumsDst)==FALSE){
    cleanedNumsDst<-cleanedNumsDst %>% dplyr::mutate(PlayerId=cgAddDst$PlayerId)
  }
  #cleanedNumsDst<-cleanedNumsDst[apply(cleanedNumsDst,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNumsDst)==FALSE){
    cleanedNumsDst<-cleanedNumsDst %>% dplyr::mutate(ContestGroupId=cgAddDst$ContestGroupId)
  }
  cleanNamesDst<- list(c("Player_Name"),names(completeModelDst[,names(completeModelDst) %in% names(cleanedNumsDst)]))
  cleanedDst<-completeModelDst %>% dplyr::select(unlist(cleanNamesDst)) %>% dplyr::select(-GameCount,-DraftGroupId)
  ###FinalDst_noPost

  allModelDst<-allModelDst %>% dplyr::select(-p_own)





  allFinalDst<-suppressMessages(dplyr::left_join(allModelDst,completeModelDst))
  allFinalDst<-allFinalDst %>% dplyr::select(Score,Salary,AvgPts,Ceiling,ProjPlusMinus,PtVal,ProjectedSacks,ActualPoints,LeveragePct,Trend,Site_Salary,OppPlusMinus,Pts,OppPts,Run_Change,Total,SpreadPct,InterceptionPct,PassingSuccessfulPct,RushingSuccessfulAllowedPct,SackPct,TakeawayPct,TouchdownAllowedPct,YardsPerPassingAttempt,RedZoneSnaps,RedZoneSnaps10,RedZoneSnaps5,RedZoneTouchdownPct,Temperature,Wind_Speed,Humidity,ContestGroupId,GameCount,Player_Name,ContestSuffix,p_own,PlayerId,PositionId,Team,LeveragePct,FantasyResultId)

  allFinalDST2 <- dplyr::left_join(allFinalDst,dstMatchupModelBig,by="PlayerId") %>% dplyr::select(-FullName)
  dstFDRemove<-grep(pattern = "_FD",names(allFinalDST2),fixed = TRUE)
  dstFDRemove2<-grep(pattern = "_FD_",names(allFinalDST2),fixed = TRUE)
  dstFDRemove3<-c(dstFDRemove,dstFDRemove2)
  allFinalDst<-allFinalDST2[,-dstFDRemove3]

  #allFinalDst<-left_join(cleanedDst,cgResults,by = c("PlayerId", "FantasyResultId", "ContestGroupId","SportId"))
  #suppressWarnings(allFinalDst<-allFinalDst %>% tidyr::separate(Player_Name,into=c("Player_Name","L/R"),sep = " \\(") %>% tidyr::separate(`L/R`,into=c("L/R"),sep = "\\)"))
  allDataDst<-allFinalDst %>% dplyr::filter(!is.na(ContestGroupId)) %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #numericHitterNamesDf<-read.csv(file="~/NFL_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  #left_join(allModelDst,completeModelDst)
  #hittersCSV <- allFinalDst %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #allDataDst <- hittersCSV



  #separate modelDate into alternate form for Player Game Logs
  #mDate<-data.frame(mD=modelDate) %>% tidyr::separate(mD,into=c("mMonth","mDay","mYear"),sep="_")
  #mDate1<- paste0(mDate$mMonth,"-",mDate$mDay,"-",mDate$mYear)
  #mDay2<-as.numeric(mDate$mDay)+1
  #mDate2<- paste0(mDate$mMonth,"-",mDay2,"-",mDate$mYear)

  ###find gamelog functions/scripts that can be used here in the nflMachLearnScripts file in NFL_Daily in ~/Ben


  #datePath<-paste0("~/NFL_Daily/dataSets/",modelDate,"/")
  #systemPath<-paste0("~/NFL_Daily/dataSets/bySystem/",modelId)


nothing2<-foreach::foreach(i=1:nrow(allDataQb)) %do% {
  contestQb<-tidyr::unnest(allDataQb[i,]) %>% dplyr::ungroup() %>% data.frame()
  contestRb<-tidyr::unnest(allDataRb[i,]) %>% dplyr::ungroup() %>% data.frame()
  contestWr<-tidyr::unnest(allDataWr[i,]) %>% dplyr::ungroup() %>% data.frame()
  contestTe<-tidyr::unnest(allDataTe[i,]) %>% dplyr::ungroup() %>% data.frame()
  contestDst<-tidyr::unnest(allDataDst[i,]) %>% dplyr::ungroup() %>% data.frame()



  modelDatePath<-paste0("~/NFL_Daily/dataSets/",modelYear)#,"/",allDataQb[i,]$ContestSuffix)#,"/QB")
  modelDatePath2<-paste0(modelDatePath,"/",modelWeek)
  suffixPathQb<-paste0(modelDatePath2,"/",allDataQb[i,]$ContestSuffix)
  suffixPathRb<-paste0(modelDatePath2,"/",allDataRb[i,]$ContestSuffix)
  suffixPathWr<-paste0(modelDatePath2,"/",allDataWr[i,]$ContestSuffix)
  suffixPathTe<-paste0(modelDatePath2,"/",allDataTe[i,]$ContestSuffix)
  suffixPathDst<-paste0(modelDatePath2,"/",allDataDst[i,]$ContestSuffix)




  qbPath<-paste0(suffixPathQb,"/QB")
  rbPath<-paste0(suffixPathRb,"/RB")
  wrPath<-paste0(suffixPathWr,"/WR")
  tePath<-paste0(suffixPathTe,"/TE")
  dstPath<-paste0(suffixPathDst,"/DST")



  dir.create(path =  modelDatePath,showWarnings = FALSE)
  dir.create(path =  modelDatePath2,showWarnings = FALSE)
  dir.create(path =  suffixPathQb,showWarnings = FALSE)
  dir.create(path =  suffixPathWr,showWarnings = FALSE)
  dir.create(path =  suffixPathTe,showWarnings = FALSE)
  dir.create(path =  suffixPathDst,showWarnings = FALSE)



  dir.create(qbPath,showWarnings = FALSE)
  dir.create(rbPath,showWarnings = FALSE)
  dir.create(wrPath,showWarnings = FALSE)
  dir.create(tePath,showWarnings = FALSE)
  dir.create(dstPath,showWarnings = FALSE)





  pathList<-list(qbPath,rbPath,wrPath,tePath,dstPath)


  foreach::foreach(j=1:length(pathList)) %do% {
    dir.create(path = pathList[[j]],showWarnings = FALSE)
  }
  write.csv(contestQb,file = paste0(pathList[1],"/stats.csv"))
  write.csv(contestRb,file = paste0(pathList[2],"/stats.csv"))
  write.csv(contestWr,file = paste0(pathList[3],"/stats.csv"))
  write.csv(contestTe,file = paste0(pathList[4],"/stats.csv"))
  write.csv(contestDst,file = paste0(pathList[5],"/stats.csv"))
}

suppressMessages(system(command = "cd ~/NFL_Daily/json/ && rm -rf *"))

names(allDataQb)<-c("ContestGroupId","gameSlate","GameCount","data")
names(allDataRb)<-c("ContestGroupId","gameSlate","GameCount","data")
names(allDataWr)<-c("ContestGroupId","gameSlate","GameCount","data")
names(allDataTe)<-c("ContestGroupId","gameSlate","GameCount","data")
names(allDataDst)<-c("ContestGroupId","gameSlate","GameCount","data")

if(qbOnly==TRUE){
  return(allDataQb)
  }
               if(rbOnly==TRUE){
                 return(allDataRb)
                 }
                              if(wrOnly==TRUE){
                                return(allDataWr)
                                }
                                             if(teOnly==TRUE){
                                               return(allDataTe)
                                               }
                                                            if(dstOnly==TRUE){
                                                              return(allDataDst)
                                                              }
return(list(Season=modelYear,Week=modelWeek,QB=allDataQb,RB=allDataRb,WR=allDataWr,TE=allDataTe,DST=allDataDst))

}

