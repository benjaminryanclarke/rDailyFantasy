#' mlb Model Import Function
#'
#' @param modelDate "m_d_yyyy"
#' @param pastEvent TRUE/FALSE
#' @param cookie labsCookies
#' @param labModel name of fantasyLab model to use
#' @param postRefresh reRun for misc postTourney stats(1/0)[T/F]
#' @param parallelRun running with doSNOW in parallel TRUE/FALSE
#' @param systemRefresh refresh lab models data(1/0)[T/F]
#' @param batsOnly only hitters data
#' @param armsOnly only pitcher data
#'
#' @return mlb Models
#' @export
#'
#' @examples mlbModelSetup(modelDate="4_5_2018",cookie=mlbCookies)
mlbModelSetup<-function(modelDate="4_1_2018",pastEvent=TRUE,labModel="mainBae",postRefresh=1,systemRefresh=1,parallelRun=FALSE,batsOnly=FALSE,armsOnly=FALSE,cookie=mlbCookies){


  require(doSNOW,quietly = TRUE)
  require(qdapRegex,quietly = TRUE)
  require(caret,quietly = TRUE)
  require(dplyr,quietly = TRUE)
  require(tidyr,quietly = TRUE)
  require(lubridate,quietly = TRUE)


setwd("~/MLB_Daily")
#removeOldDataFiles
suppressMessages(system(command = paste0("cd ~/MLB_Daily/ && rm -f MLB",modelDate,".csv")))
suppressMessages(system(command = "cd ~/MLB_Daily/json/ && rm -rf *"))

if(parallelRun==FALSE){

  #check for cookies data
  if(!exists("mlbCookies")){
  readline("missing Cookies, attempting to get them now!")
  mlbCookies<-rDailyFantasy::labsCookies()
  cookie<-mlbCookies

  assign("mlbCookies",mlbCookies,envir = .GlobalEnv)
  }
}
  #Search for sourceData
if(postRefresh==TRUE){
  suppressMessages(system(command = paste0("cd ~/MLB_Daily/sourceData && rm -R ",modelDate)))
}
  sourceDataByDate<-list.files(path = "~/MLB_Daily/sourceData/")
  #if no sourceData Exists...
  sourceData<-dplyr::if_else(modelDate %in% sourceDataByDate,TRUE,FALSE)
  if(sourceData==FALSE){
    dir.create(paste0("~/MLB_Daily/sourceData/",modelDate,"/"),showWarnings = FALSE)
    sourceUrl<- paste0("https://www.fantasylabs.com/api/sourcedata/3/",modelDate)
    setwd("~/MLB_Daily/json/")
    y3 <- paste0("sourceData",modelDate,".json")
    pmCurlHandles3 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: ",cookie,"  -o ",y3," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",sourceUrl," ",pmCurlHandles3)
    system(curlAddress,ignore.stderr = TRUE)
    sourceData <- jsonlite::fromJSON(y3, flatten = TRUE)
    setwd("~/MLB_Daily")
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
    write.csv(fullSourceData,file=paste0("~/MLB_Daily/sourceData/",modelDate,"/fullSourceData.csv"))
    #write.csv(fullSourceDataWithEvents,file=paste0("~/MLB_Daily/sourceData/",modelDate,"/fullSourceDataWithEvents.csv"))
    #create all contest directories
    sourceGroupList<-c(unique(fullSourceData$ContestSuffix))
    nothing<-suppressMessages(foreach::foreach(i=1:length(sourceGroupList)) %do% {
    dir.create(paste0("~/MLB_Daily/sourceData/",modelDate,"/",sourceGroupList[i]),showWarnings = FALSE)
      sourceD <- fullSourceData %>% dplyr::filter(ContestSuffix==sourceGroupList[i])
      #sourceD <- data.frame(fullSourceData[i,],stringsAsFactors=FALSE)
      write.csv(sourceD,file=paste0("~/MLB_Daily/sourceData/",modelDate,"/",sourceGroupList[i],"/source.csv"))
      })
  }
  suppressMessages(system(command = "cd ~/MLB_Daily/json/ && rm -rf *"))

  #importSourceData
  sourceDataIn<-read.csv(file=paste0("~/MLB_Daily/sourceData/",modelDate,"/fullSourceData.csv"),stringsAsFactors = FALSE) #%>% dplyr::select(ContestGroupId,ContestSuffix,HomeTeam,StadiumName,)
  #sourceDataInForLogs<-read.csv(file=paste0("~/MLB_Daily/sourceData/",modelDate,"/fullSourceData.csv"),stringsAsFactors = FALSE) %>% dplyr::select(StadiumName,SportEventId,ContestSuffix) %>% unique()
  #names(sourceDataInForLogs)<-c("StadiumName","EventId","ContestSuffix")

  #### ### ## #
  #modelId setup__SYSTEMS*
  if(systemRefresh==TRUE || "systemsHitters.csv" %!in% list.files(path="~/MLB_Daily")){
  systemsURL <- paste0("https://www.fantasylabs.com/api/systems/models/3/")
  setwd("~/MLB_Daily/json/")
  y4 <- paste0("systems.json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: ",cookie,"  -o ",y4," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",systemsURL," ",pmCurlHandles)
  #download systems json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  systemsDf <- jsonlite::fromJSON(paste0("~/MLB_Daily/json/",y4),simplifyDataFrame = TRUE)
  setwd("~/MLB_Daily")
  systemIds <- systemsDf$Models$SystemResult$SystemId
  systemNames <- systemsDf$Models$SystemName
  allSystems<- data.frame(sysNames=systemNames,SystemId=systemIds)
  modelId<-allSystems[which(allSystems$sysNames==labModel),][2][[1]]
  systemsResultsHitters <- data.frame(systemsDf$Models$SystemResults$`4301`)
  systemsResultsPitchers <- data.frame(systemsDf$Models$SystemResults$`4302`)
  systemsCompleteHitters <- inner_join(allSystems,systemsResultsHitters,by="SystemId")
  systemsCompletePitchers <- inner_join(allSystems,systemsResultsPitchers,by="SystemId")
  write.csv(systemsCompleteHitters,file = "~/MLB_Daily/systemsHitters.csv")
  write.csv(systemsCompletePitchers,file = "~/MLB_Daily/systemsPitchers.csv")

  suppressMessages(system(command = "cd ~/MLB_Daily/json/ && rm -rf *"))
  }
  systemssss<-read.csv(file = "~/MLB_Daily/systemsHitters.csv",stringsAsFactors = F)
  allSystems<- data.frame(sysNames=systemssss$sysNames,SystemId=systemssss$SystemId)
  modelId<-allSystems[which(allSystems$sysNames==labModel),][2][[1]]

  #setup and run webdriver to get full fantasylab data models and import into environment
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/3/",modelDate,"/?modelid=",modelId)
  #change directory to json folder
  setwd("~/MLB_Daily/json/")
  y <- paste0("MLB",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",modelURL," ",pmCurlHandles)
  #download Model json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  #read in json Model to parse
  mlbModelAll <- jsonlite::fromJSON(y, flatten = FALSE)
  setwd("~/MLB_Daily")

  suppressMessages(system(command = "cd ~/MLB_Daily/json/ && rm -rf *"))

  #cgResults <- mlbModelAll$ContestGroupFantasyResults
  cgResults <- mlbModelAll$ContestGroupFantasyResults #%>% dplyr::select(-PlayerId)
  cgResults <- inner_join(sourceDataIn,cgResults,by="ContestGroupId") %>% dplyr::select(-SourceId,-X)
  allModelHitters<- mlbModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4,PositionId=="301")
  allModelPitchers<- mlbModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4,PositionId=="302")
  cgAddHitters<- inner_join(allModelHitters,cgResults,by=c("FantasyResultId","PlayerId"))
  cgAddPitchers<- inner_join(allModelPitchers,cgResults,by=c("FantasyResultId","PlayerId"))
  #make final join to original sourceData for complete Data
  #completeModelHitters<-left_join(cgAddHitters,sourceDataIn,by="ContestGroupId") %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-") %>% dplyr::select(-maxOwn)
  #completeModelPitchers<-left_join(cgAddPitchers,sourceDataIn,by="ContestGroupId") %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-") %>% dplyr::select(-maxOwn)
  #completeModel$p_own<-unlist(lapply(completeModel$p_own,as.numeric))
  completeModelHitters<-cgAddHitters %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)
  completeModelPitchers<-cgAddPitchers %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)

  completeModelPitchersOwnCheck<-if_else(length(contains("+",vars = completeModelPitchers$p_own))>0,1,0)
  completeModelHittersOwnCheck<-if_else(length(contains("+",vars = completeModelHitters$p_own))>0,1,0)

  if(completeModelPitchersOwnCheck==1){
    completeModelPitchers<-completeModelPitchers %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }
  if(completeModelHittersOwnCheck==1){
    completeModelHitters<-completeModelHitters %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }

  completeModelPitchers$p_own<-as.numeric(completeModelPitchers$p_own)
  completeModelHitters$p_own<-as.numeric(completeModelHitters$p_own)

  #completeModelHitters$p_own<-suppressWarnings(unlist(lapply(completeModelHitters$p_own,as.numeric)))
  #completeModelPitchers$p_own<-suppressWarnings(unlist(lapply(completeModelPitchers$p_own,as.numeric)))


  #cleanUpData
  completeModelNumsHitters<-completeModelHitters[sapply(completeModelHitters,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmHittersNumsLinCombs<-caret::findLinearCombos(completeModelNumsHitters)
  #cleanedNumsHitters<-completeModelNumsHitters[,-cmHittersNumsLinCombs$remove]
  cleanedNumsHitters<-completeModelNumsHitters
  if("PlayerId" %in% names(cleanedNumsHitters)==FALSE){
    cleanedNumsHitters<-cleanedNumsHitters %>% dplyr::mutate(PlayerId=cgAddHitters$PlayerId)
  }
  #cleanedNumsHitters<-cleanedNumsHitters[apply(cleanedNumsHitters,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNumsHitters)==FALSE){
    cleanedNumsHitters<-cleanedNumsHitters %>% dplyr::mutate(ContestGroupId=cgAddHitters$ContestGroupId)
  }
  cleanNamesHitters<- list(c("Player_Name"),names(completeModelHitters[,names(completeModelHitters) %in% names(cleanedNumsHitters)]))
  cleanedHitters<-completeModelHitters %>% dplyr::select(unlist(cleanNamesHitters)) %>% dplyr::select(-GameCount,-DraftGroupId)
###FinalHitters_noPost
  numericHitterNamesDf<-read.csv(file="~/MLB_Daily/numericHitterNames.csv",stringsAsFactors = F)
  numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  namesList1<-numericHitterNamesList
  ##suppressMessages(allModelHitters$p_own<-as.numeric(allModelHitters$p_own))
  allModelPitchers<-allModelPitchers %>% select(-p_own)
  allModelHitters<-allModelHitters %>% select(-p_own)





  allFinalHitters<-suppressMessages(left_join(allModelHitters,completeModelHitters))
  allFinalHitters<-allFinalHitters[namesList1]
  #allFinalHitters<-left_join(cleanedHitters,cgResults,by = c("PlayerId", "FantasyResultId", "ContestGroupId","SportId"))
  suppressWarnings(allFinalHitters<-allFinalHitters %>% tidyr::separate(Player_Name,into=c("Player_Name","L/R"),sep = " \\(") %>% tidyr::separate(`L/R`,into=c("L/R"),sep = "\\)"))
  allDataHitters<-allFinalHitters %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #numericHitterNamesDf<-read.csv(file="~/MLB_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  #left_join(allModelHitters,completeModelHitters)
  #hittersCSV <- allFinalHitters %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #allDataHitters <- hittersCSV

  completeModelNumsPitchers<-completeModelPitchers[sapply(completeModelPitchers,is.numeric)] %>% rDailyFantasy::na.What(0)
  cmPitchersNumsLinCombs<-caret::findLinearCombos(completeModelNumsPitchers)
  #cleanedNumsPitchers<-completeModelNumsPitchers[,-cmPitchersNumsLinCombs$remove]
  cleanedNumsPitchers<-completeModelNumsPitchers
  if("PlayerId" %in% names(cleanedNumsPitchers)==FALSE){
    cleanedNumsPitchers<-cleanedNumsPitchers %>% dplyr::mutate(PlayerId=cgAddPitchers$PlayerId)
  }
  cleanedNumsPitchers<-cleanedNumsPitchers[apply(cleanedNumsPitchers,2,function(x) mean(x)!=min(x))]
  if("ContestGroupId" %in% names(cleanedNumsPitchers)==FALSE){
    cleanedNumsPitchers<-cleanedNumsPitchers %>% dplyr::mutate(ContestGroupId=cgAddPitchers$ContestGroupId)
  }
  cleanNamesPitchers<- list(c("Player_Name"),names(completeModelPitchers[,names(completeModelPitchers) %in% names(cleanedNumsHitters)]))
 cleanedPitchers<-completeModelPitchers %>% dplyr::select(unlist(cleanNamesPitchers))
 # cleanedPitchers<-completeModelPitchers[namesList1]
  ###FinalPitchers_noPost
 #allModelPitchers$p_own<-suppressMessages(as.numeric(allModelPitchers$p_own))
 allFinalPitchers<-suppressMessages(left_join(allModelPitchers %>% dplyr::select(-PitchCount),completeModelPitchers))
 allFinalPitchers<-allFinalPitchers[namesList1]
  #allFinalPitchers<-left_join(cleanedPitchers,cgResults,by = c("PlayerId", "ContestGroupId","FantasyResultId","GameCount","DraftGroupId","SportId"))
  suppressWarnings(allFinalPitchers<-allFinalPitchers %>% tidyr::separate(Player_Name,into=c("Player_Name","L/R"),sep = " \\(") %>% tidyr::separate(`L/R`,into=c("L/R"),sep = "\\)"))
  allDataPitchers<-allFinalPitchers %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #numericHitterNamesDf<-read.csv(file="~/MLB_Daily/numericHitterNames.csv",stringsAsFactors = F)
  #numericHitterNamesList<-unlist(numericHitterNamesDf$hNames)
  #namesList1<-numericHitterNamesList
  #namesList3<-c(namesList1[1:8],namesList1[11:34],namesList1[36:165])
  #pitchersCSV <- allFinalPitchers[namesList1] %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  #allDataPitchers <- pitchersCSV
  #separate modelDate into alternate form for Player Game Logs
  mDate<-data.frame(mD=modelDate) %>% tidyr::separate(mD,into=c("mMonth","mDay","mYear"),sep="_")
  mDate1<- paste0(mDate$mMonth,"-",mDate$mDay,"-",mDate$mYear)
  mDay2<-as.numeric(mDate$mDay)+1
  mDate2<- paste0(mDate$mMonth,"-",mDay2,"-",mDate$mYear)


##For miscellaneous post game stats[boxScore]
  if(pastEvent==TRUE){
    haveLogs<-dplyr::if_else(modelDate %in% list.files(path.expand("~/MLB_Daily/gameLogs/byDate")),TRUE,FALSE)
    dir.create(paste0("/Users/Ben/MLB_Daily/gameLogs/byDate/",modelDate),showWarnings = FALSE)#,"/hitters/"),showWarnings = FALSE)
    dir.create(paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/pitchers"),showWarnings = FALSE)
    dir.create(paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/hitters"),showWarnings = FALSE)
    #hitters
    unNestedAllDataHitters <- tidyr::unnest(allDataHitters) %>% dplyr::ungroup(allDataHitters) %>% data.frame()
    logIdsHitters<-tidyr::unnest(allDataHitters) %>% dplyr::ungroup(allDataHitters) %>% data.frame() %>% dplyr::distinct(PlayerId)
    #pitchers
    unNestedAllDataPitchers <- tidyr::unnest(allDataPitchers) %>% dplyr::ungroup(allDataPitchers) %>% data.frame()
    logIdsPitchers<-tidyr::unnest(allDataPitchers) %>% dplyr::ungroup(allDataPitchers) %>% data.frame() %>% dplyr::distinct(PlayerId)


    numLogsHitters<-nrow(logIdsHitters)
    numLogsPitchers<-nrow(logIdsPitchers)
    gameLogsByDateList<-list.files(path = "~/MLB_Daily/gameLogs/byDate/")
    curlLogs<-dplyr::if_else(paste0(modelDate,".csv") %in% gameLogsByDateList,TRUE,FALSE)
    if(haveLogs==FALSE || postRefresh==TRUE){
    #setup Parallel SNOW cluster with progress bar
      cl <- snow::makeCluster(4)
      registerDoSNOW(cl)
      iterationsHitters <- numLogsHitters
      iterationsPitchers <- numLogsPitchers
      pbHit <- txtProgressBar(max = iterationsHitters, style = 3)
      progressHit <- function(n) setTxtProgressBar(pbHit, n)
      optsHit <- list(progress = progressHit)
      pbPitch <- txtProgressBar(max = iterationsPitchers, style = 3)
      progressPitch <- function(n) setTxtProgressBar(pbPitch, n)
      optsPitch <- list(progress = progressPitch)

      playerMiscHit <- foreach::foreach(k=1:iterationsHitters, .combine = 'rbind', .options.snow = optsHit, .packages = c("dplyr"), .errorhandling = "remove") %dopar% {
        #set curl URL
        modelURL2<- paste0("http://www.fantasylabs.com/api/players/",logIdsHitters$PlayerId[k],"/gamelog/4/3/",mDate1,"/",mDate2,"/All/")
        y2 <- paste0(logIdsHitters$PlayerId[k],"_",mDate1,".json")
        pmCurlHandles2 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: ",cookie,"  -o ",y2," -H 'Connection: keep-alive' --compressed")
        curlAddress2 <- paste0("curl -L ",modelURL2," ",pmCurlHandles2)
        setwd("~/MLB_Daily/json/")
        system(curlAddress2,ignore.stderr = TRUE)
        mlbModelMisc <- jsonlite::fromJSON(y2, simplifyDataFrame = TRUE)
        setwd("~/MLB_Daily")
        mlbModelMisc<- data.frame(mlbModelMisc$Properties) %>% dplyr::filter(SalaryDate != "Total")
        class(mlbModelMisc$TourneyOwnership)<-"numeric"
        keepNames<- c(8,10,12,19,21:39)
        miscClean<-mlbModelMisc[,keepNames]
      }
      playerMiscPitch <- foreach::foreach(k=1:iterationsPitchers, .combine = 'rbind', .options.snow = optsPitch, .packages = c("dplyr"), .errorhandling = "remove") %dopar% {
        #set curl URL
        modelURL2<- paste0("http://www.fantasylabs.com/api/players/",logIdsPitchers$PlayerId[k],"/gamelog/4/3/",mDate1,"/",mDate2,"/All/")
        y2 <- paste0(logIdsPitchers$PlayerId[k],"_",mDate1,".json")
        pmCurlHandles2 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: ",cookie,"  -o ",y2," -H 'Connection: keep-alive' --compressed")
        curlAddress2 <- paste0("curl -L ",modelURL2," ",pmCurlHandles2)
        setwd("~/MLB_Daily/json/")
        system(curlAddress2,ignore.stderr = TRUE)
        mlbModelMisc <- jsonlite::fromJSON(y2, simplifyDataFrame = TRUE)
        setwd("~/MLB_Daily")
        mlbModelMisc<- data.frame(mlbModelMisc$Properties) %>% dplyr::filter(SalaryDate != "Total") %>% select(-PitchCount)
        class(mlbModelMisc$TourneyOwnership)<-"numeric"
        keepNames<- c(8,10,12,40:62,65,68)
        miscClean<-mlbModelMisc[,keepNames]
      }

      close(pbHit)
      close(pbPitch)
      snow::stopCluster(cl)
      rDailyFantasy::unregister()
      #playerMisc$Posn<- rDailyFantasy::na.What(playerMisc$Posn,0)
      #playerMisc<-playerMisc %>% dplyr::mutate(cut=dplyr::if_else(playerMisc$Posn==0,0,1))
      #playerMisc$cut<-factor(playerMisc$cut,levels = c("0","1"),labels = c("Miss","Make"))

      #dir.create(path = paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/hitters"),showWarnings = FALSE)
      #dir.create(path = paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/pitchers"),showWarnings = FALSE)

      write.csv(playerMiscHit,file = paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/hitters/stats.csv"))
      write.csv(playerMiscPitch,file = paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/pitchers/stats.csv"))
    }
    #Hit
    postDataHitters<-read.csv(file = paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/hitters/stats.csv"),stringsAsFactors = FALSE) %>% dplyr::select(-X)
    postAllHitters<-dplyr::left_join(unNestedAllDataHitters,postDataHitters,by="PlayerId")
    allDataHitters<-postAllHitters %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
    #Pitch
    postDataPitchers<-read.csv(file = paste0("~/MLB_Daily/gameLogs/byDate/",modelDate,"/pitchers/stats.csv"),stringsAsFactors = FALSE) %>% dplyr::select(-X)
    postAllPitchers<-dplyr::left_join(unNestedAllDataPitchers,postDataPitchers,by="PlayerId")
    allDataPitchers<-postAllPitchers %>% dplyr::group_by(ContestGroupId,ContestSuffix,GameCount) %>% tidyr::nest()
  }


  #datePath<-paste0("~/MLB_Daily/dataSets/",modelDate,"/")
  #systemPath<-paste0("~/MLB_Daily/dataSets/bySystem/",modelId)
  nothing2<-foreach::foreach(i=1:nrow(allDataHitters)) %do% {
    contestHitters<-tidyr::unnest(allDataHitters[i,]) %>% dplyr::ungroup() %>% data.frame()
    contestPitchers<-tidyr::unnest(allDataPitchers[i,]) %>% dplyr::ungroup() %>% data.frame()
    modelDatePath<-paste0("~/MLB_Daily/dataSets/",modelDate)#,"/",allDataHitters[i,]$ContestSuffix)#,"/hitters")
    suffixPathH<-paste0(modelDatePath,"/",allDataHitters[i,]$ContestSuffix)
    suffixPathP<-paste0(modelDatePath,"/",allDataPitchers[i,]$ContestSuffix)

    hitterPath<-paste0(suffixPathH,"/hitters")
    pitcherPath<-paste0(suffixPathP,"/pitchers")
    dir.create(modelDatePath,showWarnings = FALSE)
    dir.create(suffixPathH,showWarnings = FALSE)
    dir.create(suffixPathP,showWarnings = FALSE)
    dir.create(hitterPath,showWarnings = FALSE)
    dir.create(pitcherPath,showWarnings = FALSE)
    pathList<-list(hitterPath,pitcherPath)
    foreach::foreach(j=1:length(pathList)) %do% {
      dir.create(path = pathList[[j]],showWarnings = FALSE)
      }
      write.csv(contestHitters,file = paste0(pathList[1],"/stats.csv"))
      write.csv(contestPitchers,file = paste0(pathList[2],"/stats.csv"))
    }

suppressMessages(system(command = "cd ~/MLB_Daily/json/ && rm -rf *"))

names(allDataHitters)<-c("ContestGroupId","gameSlate","GameCount","data")
names(allDataPitchers)<-c("ContestGroupId","gameSlate","GameCount","data")

if(batsOnly==TRUE){
  return(allDataHitters)
}
if(armsOnly==TRUE){
  return(allDataPitchers)
}
return(list(Date=modelDate,Bats=allDataHitters,Arms=allDataPitchers))
}

