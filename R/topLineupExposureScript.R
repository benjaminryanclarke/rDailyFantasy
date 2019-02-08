#' NBA Top 100 Lineups Player Exposure addition
#'
#' @param modelDate "11_26_2018"
#' @param cookies nbaCookies
#'
#' @return dataSet with 1/0 in inLineup col to indicate if player was heavily owned in top 100 winning lineups
#' @export
#'
#' @examples nbaTop100Exposure(modelDate="11_26_2018",cookies=nbaCookies)
nbaTop100Exposure<-function(modelDate="11_26_2018",cookies=nbaCookies){

  #check for cookies data
  if(!exists("nbaCookies")){
    readline("missing Cookies, attempting to get them now!")
    nbaCookies<-rDailyFantasy::labsCookies()
    cookie<-nbaCookies$labs

    assign("nbaCookies",nbaCookies$labs,envir = .GlobalEnv)
    assign("dkCookies",nbaCookies$dk,envir = .GlobalEnv)
  }

  #split modelDate into separate variables
  modelDateSplit<-data.frame(modelDate) %>% setNames(nm = c("modelDate")) %>% tidyr::separate(col = "modelDate",into = c("mMonth","mDay","mYear"),sep = "_") %>% data.frame()
  mMonth<-modelDateSplit[,1]
  mDay<-modelDateSplit[,2]
  mYear<-modelDateSplit[,3]

  #getContestIds for top exposure 100 lineups best
  modelURL <- paste0("https://www.fantasylabs.com/api/contestlist/2/4/",modelDate,"/")
  setwd("~/NBA_Daily/json/")
  y <- paste0("NBA",modelDate,"ContestId.json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookies,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",modelURL," ",pmCurlHandles)
  #download Model json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  #read in json Model to parse
  nbaContests <- jsonlite::fromJSON(y, flatten = FALSE)
  expContestId <- nbaContests[1][nbaContests[1] %>% nrow(),]

  #get top exposures
  modelURL <- paste0("https://www.fantasylabs.com/api/contests/leaderboardexposure/2/",expContestId,"/")
  setwd("~/NBA_Daily/json/")
  y <- paste0("NBA",modelDate,"ContestTopExp.json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookies,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",modelURL," ",pmCurlHandles)
  #download Model json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  #read in json Model to parse
  nbaTopLineupsExp <- jsonlite::fromJSON(y, flatten = FALSE) %>% dplyr::filter(OwnPerc>=10) %>% dplyr::select(PlayerId,PlayerName,OwnPerc,FieldPerc,UserVariance)

  suppressMessages(system(command = "cd ~/NBA_Daily/json/ && rm -rf *"))

  setwd("~/rDFS/rDailyFantasy")

  write.csv(nbaTopLineupsExp,file=paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/Main/top100LineupsExposure.csv"))
  toJoin<-nbaTopLineupsExp %>% dplyr::select(PlayerId) %>% dplyr::mutate("inLineup"=1)

  nbaModel<-nbaModelSetup(modelDate = modelDate,gameSlate = "Main",labModel = "Money",sameDay = TRUE,cookie = cookies)
  expJoin<-dplyr::left_join(nbaModel,toJoin,by="PlayerId")
  expJoin$inLineup[is.na(expJoin$inLineup)]<-0
  write.csv(expJoin,file=paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/statsWithTop100LineupSelection.csv"))
  return(expJoin)
}
