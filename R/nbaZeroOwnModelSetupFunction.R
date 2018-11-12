#' NBA Player Model Zero Projected Ownership Retrieval Function
#'
#' @param modelDate "10_16_2018"
#' @param gameSlate "Main"
#' @param labModel  "zeroOwnVarImp"
#' @param systemsRefresh 0/1
#' @param sourceRefresh 0/1
#' @param cookie nbaCookies
#'
#' @return labsPlayerModel
#' @export
#'
#' @examples nbaModelSetup(modelDate="10_16_2018",gameSlate="Main",labModel="2017FirstModel",systemsRefresh=0,sourceRefresh=0,cookie=nbaCookies)
nbaZeroModelSetup<-function(modelDate="10_16_2018",gameSlate="Main",labModel="zeroOwnVarImp",systemsRefresh=0,sourceRefresh=0,cookie=nbaCookies){

  require(foreach,quietly = TRUE,warn.conflicts = F)
  require(doSNOW,quietly = TRUE,warn.conflicts = F)
  require(qdapRegex,quietly = TRUE,warn.conflicts = F)
  require(caret,quietly = TRUE,warn.conflicts = F)
  require(dplyr,quietly = TRUE,warn.conflicts = F)
  require(tidyr,quietly = TRUE,warn.conflicts = F)
  require(lubridate,quietly = TRUE,warn.conflicts = F)

  #set homeDirectory
  hmDir<-getwd()

  #check for cookies data
  if(!exists("nbaCookies")){
    readline("missing Cookies, attempting to get them now!")
    nbaCookies<-rDailyFantasy::labsCookies()
    cookie<-nbaCookies$labs

    assign("nbaCookies",nbaCookies$labs,envir = .GlobalEnv)
    assign("dkCookies",nbaCookies$dk,envir = .GlobalEnv)
  }


  setwd(hmDir)

  #systems
  if(systemsRefresh==TRUE || "systems.csv" %!in% list.files(path="~/NBA_Daily")){
    systemsURL <- paste0("https://www.fantasylabs.com/api/systems/models/2/")
    setwd("~/NBA_Daily/json/")
    y4 <- paste0("systems.json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y4," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",systemsURL," ",pmCurlHandles)
    #download systems json to json folder
    system(curlAddress,ignore.stderr = TRUE)
    systemsDf <- jsonlite::fromJSON(paste0("~/NBA_Daily/json/",y4),simplifyDataFrame = TRUE)
    setwd("~/NBA_Daily")
    allSystems<-systemsDf$Models %>% dplyr::select(SystemName,SystemId) %>% setNames(c("sysNames","SystemId"))
    modelId<-allSystems[which(allSystems$sysNames==labModel),][2][[1]]
    systemsResults <- data.frame(systemsDf$Models$SystemResults$`4201`)
    systemsComplete <- dplyr::inner_join(allSystems,systemsResults,by="SystemId")
    write.csv(systemsComplete,file = "systems.csv")

    suppressMessages(system(command = "cd ~/NBA_Daily/json/ && rm -rf *"))

    setwd(hmDir)
  }

  setwd("~/NBA_Daily")
  allSystems<-read.csv(file = "systems.csv") %>% dplyr::select(-X)
  modelId<-allSystems[which(allSystems$sysNames==labModel),][2][[1]]
  setwd(hmDir)

  #Search for sourceData
  mYear<-tidyr::separate(as.data.frame(modelDate),col=modelDate,into = c("mMonth","mDay","mYear"),by = "_")[,'mYear']
  mDay<-tidyr::separate(as.data.frame(modelDate),col=modelDate,into = c("mMonth","mDay","mYear"),by = "_")[,'mDay']
  mMonth<-tidyr::separate(as.data.frame(modelDate),col=modelDate,into = c("mMonth","mDay","mYear"),by = "_")[,'mMonth']
  sourceDataByDate<-list.files(path = paste0("~/NBA_Daily/sourceData/",mYear,"/"))
  sourceData<-dplyr::if_else(modelDate %in% sourceDataByDate,TRUE,FALSE)
  if(sourceRefresh==TRUE && sourceData==TRUE){
    suppressMessages(system(command = paste0("cd ~/NBA_Daily/sourceData/",mYear,"/"," && rm -R ",modelDate)))
    suppressMessages(system(command = paste0("cd ~/NBA_Daily/dataSets/",mYear,"/"," && rm -R ",modelDate)))

  }
  sourceDataByDate<-list.files(path = paste0("~/NBA_Daily/sourceData/",mYear,"/"))
  #if no sourceData Exists...
  sourceData<-dplyr::if_else(modelDate %in% sourceDataByDate,TRUE,FALSE)
  if(sourceData==FALSE){
    dir.create(paste0("~/NBA_Daily/sourceData/",mYear,"/",modelDate),showWarnings = FALSE)
    sourceUrl<- paste0("https://www.fantasylabs.com/api/sourcedata/2/",modelDate)
    setwd("~/NBA_Daily/json/")
    y3 <- paste0("sourceData",modelDate,".json")
    pmCurlHandles3 <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y3," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",sourceUrl," ",pmCurlHandles3)
    system(curlAddress,ignore.stderr = TRUE)
    sourceDataIn <- jsonlite::fromJSON(y3, flatten = TRUE)
    setwd(paste0("~/NBA_Daily/sourceData/",mYear,"/",modelDate))
    fullSourceData<- data.frame(sourceDataIn$ContestGroups,stringsAsFactors = FALSE) %>% dplyr::select(-Events) %>% dplyr::filter(SourceId==4) %>% dplyr::select(-AdminEdit,-IsProjected,-IsPrimary,-IsOpen)
    fullSourceData$ContestSuffix<-dplyr::if_else(fullSourceData$ContestSuffix=="","Main",unlist(qdapRegex::ex_bracket(fullSourceData$ContestSuffix)))
    fullSourceData<-fullSourceData %>% dplyr::filter(!is.na(ContestSuffix))
    write.csv(fullSourceData,file="fullSourceData.csv")
    #create all contest directories
    sourceGroupList<-c(unique(fullSourceData$ContestSuffix))
    nothing<-suppressMessages(foreach::foreach(i=1:length(sourceGroupList)) %do% {
      dir.create(paste0(sourceGroupList[i]),showWarnings = FALSE)
      sourceD <- fullSourceData %>% dplyr::filter(ContestSuffix==sourceGroupList[i])
      write.csv(sourceD,file=paste0(sourceGroupList[i],"/source.csv"))
    })

    suppressMessages(system(command = "cd ~/NBA_Daily/json/ && rm -rf *"))

    setwd(hmDir)
  }

  #importSourceDate
  setwd(paste0("~/NBA_Daily/sourceData/",mYear,"/",modelDate))
  fullSourceData<-read.csv(file="fullSourceData.csv") %>% dplyr::select(-X) %>% data.frame()
  setwd(hmDir)


  #playerModelData
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=",modelId)
  setwd("~/NBA_Daily/json/")
  y <- paste0("NBA",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",modelURL," ",pmCurlHandles)
  #download Model json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  #read in json Model to parse
  nbaModelAll <- jsonlite::fromJSON(y, flatten = FALSE)

  suppressMessages(system(command = "cd ~/NBA_Daily/json/ && rm -rf *"))

  contestGroups <- nbaModelAll$ContestGroupFantasyResults
  allContestGroupsDf <- dplyr::inner_join(fullSourceData,contestGroups,by="ContestGroupId") %>% dplyr::select(ContestGroupId,ContestSuffix,PlayerId)
  dkPlayerModel<- nbaModelAll$PlayerModels$Properties %>% dplyr::filter(SourceId==4)
  #dkPlayerModel<- dplyr::left_join(dkPlayerModel,allContestGroupsDf,by=c("PlayerId")) %>% dplyr::select(-FantasyResultId,-ContestGroupId,-EventId,-EventTeamId,-SourceId,-IsProjected,-IsLocked,-InLineup,-InjuryStatus,-Watch,-Confirmed,-PositionType,-FullName,-SlateSize,-B2B,-Refs,-Exp_min,-Exp,-MyTrends,-OwnRank_Slate,-ProTrendsPercentile_Slate,-SalaryChangePercentile_Slate,-SiteDiffPercentile_Slate,-VegasPercentile_Slate) %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)
  dkPlayerModel<- dplyr::inner_join(dkPlayerModel,allContestGroupsDf,by=c("PlayerId")) %>% dplyr::select(-SourceId,-IsProjected,-IsLocked,-InLineup,-InjuryStatus,-Watch,-Confirmed,-PositionType,-FullName,-SlateSize,-B2B,-Refs,-Exp_min,-Exp,-MyTrends,-OwnRank_Slate,-ProTrendsPercentile_Slate,-SalaryChangePercentile_Slate,-SiteDiffPercentile_Slate,-VegasPercentile_Slate) %>% tidyr::separate(p_own,into=c("p_own","maxOwn"),sep="-",fill="right") %>% dplyr::select(-maxOwn)
  if("MyTrends|custom" %in% names(dkPlayerModel)){
    dkPlayerModel <- dkPlayerModel %>% select(-`MyTrends|custom`)
  }
  dkPlayerModelOwnCheck<-dplyr::if_else(length(dplyr::contains("+",vars = dkPlayerModel$p_own))>0,1,0)
  if(dkPlayerModelOwnCheck==1){
    dkPlayerModel<-dkPlayerModel %>% tidyr::separate(p_own,into=c("p_own","drop"),sep="\\+",fill="right") %>% dplyr::select(-drop)
  }
  dkPlayerModel$p_own<-as.numeric(dkPlayerModel$p_own)
  dkPlayerModel<-dkPlayerModel %>% unique() %>% dplyr::filter(p_own==0)

  #playerMatchupData
  dkPlayerModelMatchupTeamsUrlOrig<-dkPlayerModel$TeamLong %>% unique()
  dkPlayerModelMatchupTeams<-str_replace_all(string=dkPlayerModelMatchupTeamsUrlOrig, pattern=" ", repl="%20")

  playerMatchupLoop<-foreach(j=1:length(dkPlayerModelMatchupTeams)) %do% {
    matchupURL <- paste0("https://www.fantasylabs.com/api/matchups/2/team/",dkPlayerModelMatchupTeams[j],"/",mMonth,"-",mDay,"-",mYear)
    setwd("~/NBA_Daily/json/")
    y5 <- paste0(j,"matchup",modelDate,".json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y5," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl -L ",matchupURL," ",pmCurlHandles)
    #download Model json to json folder
    system(curlAddress,ignore.stderr = TRUE)
    #read in json Model to parse
    nbaMatchup <- jsonlite::fromJSON(y5)
    nbaMatchupNums<-nbaMatchup$PlayerMatchups$Properties %>% select(-HasNews,-NewsItem,-EventId,-EventTeamId)#%>% rDailyFantasy::numericOnly() #%>% select(-DateId,-EventId,-EventTeamId,-DateId_RnkPct,-EventId_RnkPct,-EventTeamId_RnkPct)
    return(nbaMatchupNums)
  }
  fullPlayerMatchup<-bind_rows(playerMatchupLoop[]) %>% dplyr::select(-Team)

  #joinPlayerModel with PlayerMatchup

  #dkPlayerModelFull<-left_join(dkPlayerModel,fullPlayerMatchup,by="PlayerId") %>% unique() %>% dplyr::filter(!is.na(ContestSuffix))
  dkPlayerModelFull<-inner_join(dkPlayerModel,fullPlayerMatchup,by="PlayerId") %>% unique() %>% dplyr::filter(!is.na(ContestSuffix))

  #nestedPlayerModel by slateName/ContestGroupId
  dkPlayerModelNested<-dkPlayerModelFull %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  #create and write dataFile to NBA_Daily dataSets directory
  #suppressMessages(system(command = paste0("cd ~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/ && rm -rf *")))

  dkPlayerModelSaving<-foreach::foreach(i=1:nrow(dkPlayerModelNested)) %do% {
    dkModelSave<-tidyr::unnest(dkPlayerModelNested[i,]) %>% dplyr::ungroup() %>% data.frame()
    modelYearPath<-paste0("~/NBA_Daily/dataSets/",mYear)
    modelDatePath<-paste0(modelYearPath,"/",modelDate)
    suffixPath<-paste0(modelDatePath,"/",dkPlayerModelNested[i,]$ContestSuffix)
    dir.create(path =  modelYearPath,showWarnings = FALSE)
    dir.create(path =  modelDatePath,showWarnings = FALSE)
    dir.create(path =  suffixPath,showWarnings = FALSE)
    write.csv(dkModelSave,file = paste0(suffixPath,"/zeroStats.csv"))
  }
  suppressMessages(system(command = "cd ~/NBA_Daily/json/ && rm -rf *"))
  setwd(hmDir)



  return(dkPlayerModelFull %>% dplyr::filter(ContestSuffix==gameSlate) %>% select(-ContestSuffix) %>% unique())
}
