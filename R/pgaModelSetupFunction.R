#' pga Model Import Function
#'
#' @param modelDate "m_d_yyyy"
#' @param pastEvent TRUE/FALSE
#' @param cookie labsCookies
#' @param labModel name of fantasyLab model to use
#' @param postRefresh reRun for misc postTourney stats
#' @param parallelRun running with doSNOW in parallel TRUE/FALSE
#'
#' @return pga Models
#' @export
#'
#' @examples pgaModelSetup(modelDate="4_5_2018",cookie=pgaCookies)
pgaModelSetup<-function(modelDate="3_4_2018",pastEvent=FALSE,labModel="WGCBridgestone",postRefresh=NULL,parallelRun=FALSE,cookie=pgaCookies){

require(doSNOW,quietly = TRUE)
require(qdapRegex,quietly = TRUE)
require(caret,quietly = TRUE)
require(dplyr,quietly = TRUE)
require(tidyr,quietly = TRUE)

setwd("~/PGA_Daily")
#removeOldDataFiles
system(command = paste0("cd ~/PGA_Daily/ && rm -f PGA",modelDate,".csv"))
system(command = "cd ~/PGA_Daily/ && rm -f systems.csv")
system(command = "cd ~/PGA_Daily/json/ && rm -rf *")
if(parallelRun==FALSE){
if(!exists("pgaCookies")){
  readline("missing Cookies, attempting to get them now!")
  pgaCookies<-rDailyFantasy::labsCookies()
  cookie<-pgaCookies

  assign("pgaCookies",pgaCookies,envir = .GlobalEnv)
}
}
  #Search for sourceData
  sourceDataByDate<-list.files(path = "~/PGA_Daily/sourceData/")
  #if no sourceData Exists...
  sourceData<-dplyr::if_else(modelDate %in% sourceDataByDate,TRUE,FALSE)
  if(sourceData==FALSE){
    dir.create(paste0("~/PGA_Daily/sourceData/",modelDate),showWarnings = FALSE)
    sourceUrl<- paste0("https://www.fantasylabs.com/api/sourcedata/5/",modelDate)
    setwd("~/PGA_Daily/json/")
    y3 <- paste0("sourceData",modelDate,".json")
    pmCurlHandles3 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/pga/player-models/' -H 'Cookie: ",cookie,"  -o ",y3," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",sourceUrl," ",pmCurlHandles3)
    system(curlAddress,ignore.stderr = TRUE)
    sourceData <- jsonlite::fromJSON(y3, flatten = TRUE)
    setwd("~/PGA_Daily")
    eventsData <- data.frame(dplyr::bind_rows(sourceData$ContestGroups$Events),stringsAsFactors=FALSE) %>% dplyr::select(-IsPrimary)
    sourceData2<- data.frame(sourceData$ContestGroups,stringsAsFactors = FALSE) %>% dplyr::select(-Events) %>% dplyr::filter(SourceId==4)
    fullSourceData<-left_join(sourceData2,eventsData,by="ContestGroupId")
    fullSourceData$ContestSuffix<-unlist(qdapRegex::ex_bracket(fullSourceData$ContestSuffix))
    write.csv(fullSourceData,file=paste0("~/PGA_Daily/sourceData/",modelDate,"/fullSourceData.csv"))
    #create all contest directories
    nothing<-suppressMessages(foreach::foreach(i=1:nrow(fullSourceData)) %do% {
    dir.create(paste0("~/PGA_Daily/sourceData/",modelDate,"/",fullSourceData$ContestSuffix[i]),showWarnings = FALSE)
      sourceD <- data.frame(fullSourceData[i,],stringsAsFactors=FALSE)
      write.csv(sourceD,file=paste0("~/PGA_Daily/sourceData/",modelDate,"/",fullSourceData$ContestSuffix[i],"/source.csv"))
      })
  }
  system(command = "cd ~/PGA_Daily/json/ && rm -rf *")

  #importSourceData
  sourceDataIn<-read.csv(file=paste0("~/PGA_Daily/sourceData/",modelDate,"/fullSourceData.csv"),stringsAsFactors = FALSE) %>% dplyr::select(ContestGroupId,ContestSuffix,HomeTeam,StadiumName,)
  sourceDataInForLogs<-read.csv(file=paste0("~/PGA_Daily/sourceData/",modelDate,"/fullSourceData.csv"),stringsAsFactors = FALSE) %>% dplyr::select(StadiumName,SportEventId) %>% unique()
  names(sourceDataInForLogs)<-c("StadiumName","EventId")
  #modelId setup__SYSTEMS*
  systemsURL <- paste0("https://www.fantasylabs.com/api/systems/models/5/")
  setwd("~/PGA_Daily/json/")
  y4 <- paste0("systems.json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/pga/player-models/' -H 'Cookie: ",cookie,"  -o ",y4," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",systemsURL," ",pmCurlHandles)
  #download systems json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  systemsDf <- jsonlite::fromJSON(paste0("~/PGA_Daily/json/",y4),simplifyDataFrame = TRUE)
  setwd("~/PGA_Daily")
  systemIds <- systemsDf$Models$SystemResult$SystemId
  systemNames <- systemsDf$Models$SystemName
  allSystems<- data.frame(sysNames=systemNames,SystemId=systemIds)
  modelId<-allSystems[which(allSystems$sysNames==labModel),][2][[1]]
  systemsResults <- data.frame(systemsDf$Models$SystemResults$`4501`)
  systemsComplete <- inner_join(allSystems,systemsResults,by="SystemId")
  write.csv(systemsComplete,file = "~/PGA_Daily/systems.csv")

  system(command = "cd ~/PGA_Daily/json/ && rm -rf *")

  #setup and run webdriver to get full fantasylab data models and import into environment
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/5/",modelDate,"/?modelid=",modelId)
  #change directory to json folder
  setwd("~/PGA_Daily/json/")
  y <- paste0("PGA",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/pga/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",modelURL," ",pmCurlHandles)
  #download Model json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  #read in json Model to parse
  pgaModelAll <- jsonlite::fromJSON(y, flatten = FALSE)
  setwd("~/PGA_Daily")

  system(command = "cd ~/PGA_Daily/json/ && rm -rf *")
  #cgResults <- pgaModelAll$ContestGroupFantasyResults
  cgResults <- pgaModelAll$ContestGroupFantasyResults %>% dplyr::select(-PlayerId)
  allModel<- pgaModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4)
  cgAdd<- inner_join(allModel,cgResults,by="FantasyResultId")
  #make final join to original sourceData for complete Data
  completeModel<-left_join(cgAdd,sourceDataIn,by="ContestGroupId") %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-") %>% dplyr::select(-maxOwn)
  completeModel$p_own<-unlist(lapply(completeModel$p_own,as.numeric))
  #cleanUpData
  completeModelNums<-completeModel[sapply(completeModel,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmNumsLinCombs<-caret::findLinearCombos(completeModelNums)
  cleanedNums<-completeModelNums[,-cmNumsLinCombs$remove]
  if("PlayerId" %in% names(cleanedNums)==FALSE){
  cleanedNums<-cleanedNums %>% dplyr::mutate(PlayerId=cgAdd$PlayerId)
  }
  cleanedNums<-cleanedNums[apply(cleanedNums,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNums)==FALSE){
  cleanedNums<-cleanedNums %>% dplyr::mutate(ContestGroupId=cgAdd$ContestGroupId)
  }
  cleanNames<- list(c("Player_Name"),names(completeModel[,names(completeModel) %in% names(cleanedNums)]))
  cleaned<-completeModel %>% dplyr::select(unlist(cleanNames))
  allFinal<-left_join(cleaned,sourceDataIn,by="ContestGroupId")
  allData<-allFinal %>% dplyr::group_by(ContestGroupId,ContestSuffix,HomeTeam,StadiumName) %>% tidyr::nest()

  #separate modelDate into alternate form for Player Game Logs
  mDate<-data.frame(mD=modelDate) %>% tidyr::separate(mD,into=c("mMonth","mDay","mYear"),sep="_")
  mDate<- paste0(mDate$mMonth,"-",mDate$mDay,"-",mDate$mYear)

  ##For miscellaneous post round stats
  if(pastEvent==TRUE){
    unNestedAllData <- tidyr::unnest(allData) %>% dplyr::ungroup(allData) %>% data.frame()
    logIds<-tidyr::unnest(allData) %>% dplyr::ungroup(allData) %>% data.frame() %>% dplyr::distinct(PlayerId)
    numLogs<-nrow(logIds)
    gameLogsByDateList<-list.files(path = "~/PGA_Daily/gameLogs/byDate/")
    curlLogs<-dplyr::if_else(paste0(modelDate,".csv") %in% gameLogsByDateList,TRUE,FALSE)
    if(curlLogs==FALSE || !is.null(postRefresh)){
    #setup Parallel SNOW cluster with progress bar
      cl <- snow::makeCluster(4)
      registerDoSNOW(cl)
      iterations <- numLogs
      pb <- txtProgressBar(max = iterations, style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)

      playerMisc <- foreach::foreach(k=1:iterations, .combine = 'rbind', .options.snow = opts, .packages = c("dplyr"), .errorhandling = "remove") %dopar% {
        #set curl URL
        modelURL2<- paste0("http://www.fantasylabs.com/api/players/",logIds$PlayerId[k],"/gamelog/4/5/",mDate,"/",mDate,"/All/")
        y2 <- paste0(logIds$PlayerId[2],"_",mDate,".json")
        pmCurlHandles2 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/pga/player-models/' -H 'Cookie: ",cookie,"  -o ",y2," -H 'Connection: keep-alive' --compressed")
        curlAddress2 <- paste0("curl -L ",modelURL2," ",pmCurlHandles2)
        setwd("~/PGA_Daily/json/")
        system(curlAddress2,ignore.stderr = TRUE)
        pgaModelMisc <- jsonlite::fromJSON(y2, simplifyDataFrame = TRUE)
        setwd("~/PGA_Daily")
        pgaModelMisc<- data.frame(pgaModelMisc$Properties) %>% dplyr::filter(SalaryDate != "Total")
        class(pgaModelMisc$Posn)<-"numeric"
        class(pgaModelMisc$TourneyOwnership)<-"numeric"
        class(pgaModelMisc$Rd1)<-"numeric"
        class(pgaModelMisc$Rd2)<-"numeric"
        class(pgaModelMisc$Rd3)<-"numeric"
        class(pgaModelMisc$Rd4)<-"numeric"
        keepNames<- c(4,8,10,13,15,16,17:20,49:57)
        miscClean<-pgaModelMisc[,keepNames]
      }

      close(pb)
      snow::stopCluster(cl)
      rDailyFantasy::unregister()
      playerMisc$Posn<- rDailyFantasy::na.What(playerMisc$Posn,0)
      playerMisc<-playerMisc %>% dplyr::mutate(cut=dplyr::if_else(playerMisc$Posn==0,0,1))
      playerMisc$cut<-factor(playerMisc$cut,levels = c("0","1"),labels = c("Miss","Make"))

      write.csv(playerMisc,file = paste0("~/PGA_Daily/gameLogs/byDate/",modelDate,".csv"))
    }
    postData<-read.csv(file = paste0("~/PGA_Daily/gameLogs/byDate/",modelDate,".csv")) %>% dplyr::select(-FullName,-X)
    postAll<-dplyr::left_join(unNestedAllData,postData,by="PlayerId")
    allData<-postAll %>% dplyr::group_by(ContestGroupId,ContestSuffix,HomeTeam,StadiumName) %>% tidyr::nest()
  }


  datePath<-paste0("~/PGA_Daily/dataSets/byDate/",modelDate)
  systemPath<-paste0("~/PGA_Daily/dataSets/bySystem/",modelId)
  nothing2<-foreach::foreach(i=1:nrow(allData)) %do% {
    contest<-tidyr::unnest(allData[i,]) %>% dplyr::ungroup() %>% data.frame()
    coursePath<-paste0("~/PGA_Daily/dataSets/byCourse/",allData[i,]$StadiumName)
    tournamentPath<-paste0("~/PGA_Daily/dataSets/byTournament/",allData[i,]$HomeTeam)
    contestPath<-paste0("~/PGA_Daily/dataSets/byContest/",allData[i,]$ContestSuffix)
    pathList<-list(coursePath,tournamentPath,contestPath,datePath,systemPath)
    foreach::foreach(j=1:length(pathList)) %do% {
      dir.create(path = pathList[[j]],showWarnings = FALSE)
      write.csv(contest,file = paste0(pathList[j],"/",modelDate,"_",allData[i,]$ContestSuffix,".csv"))
    }
  }

system(command = "cd ~/PGA_Daily/json/ && rm -rf *")

return(allData)
}
