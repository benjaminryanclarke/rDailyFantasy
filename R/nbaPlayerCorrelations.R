#' nbaPlayerCorrelations Function
#'
#' @param modelDate "10_18_2018"
#'
#' @return NULL
#' @export
#'
#' @examples nbaPlayerCorrelations(modelDate="10_18_2018")
nbaPlayerCorrelations<-function(modelDate="10_18_2018"){

hmDir<-getwd()

library(doSNOW)
if(foreach::getDoParRegistered()!=TRUE){
cl <- makeCluster(2)
registerDoSNOW(cl)
}

  modelDateSplit<-data.frame(modelDate) %>% setNames(nm = c("modelDate")) %>% tidyr::separate(col = "modelDate",into = c("mMonth","mDay","mYear"),sep = "_") %>% data.frame()
  mMonth<-modelDateSplit[,1]
  mDay<-modelDateSplit[,2]
  mYear<-modelDateSplit[,3]

gameSlate<-list.files(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate))

foreach(j=1:length(gameSlate),.packages = c("dplyr","foreach")) %dopar% {
 # if(!exists("nbaCookies")){
  #  readline("missing Cookies, attempting to get them now!")
   # nbaCookies<-rDailyFantasy::labsCookies()
    #cookie<-nbaCookies
#
 #   assign("nbaCookies",nbaCookies,envir = .GlobalEnv)
  #}
nba<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate[j],"/stats.csv")) %>% dplyr::select(-X)

nba18PlayerCors<-foreach(i=1:nrow(nba),.packages = c("dplyr")) %dopar% {
corrPID<-nba$PlayerId[i]
setwd("~/NBA_Daily/json/")
y6 <- paste0("nbaCorr",corrPID,".json")
corrDate<-gsub("_",replacement = "",x = modelDate)

corrCurl <- paste0("curl -L 'https://www.fantasylabs.com/api/events/2/players/",corrPID,"/correlations/4/\\[object%20Object\\]/' -H 'dnt: 1' -H 'accept-encoding: gzip, deflate, br' -H 'accept-language: en-US,en;q=0.9' -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36' -H 'accept: application/json, text/plain, */*' -H 'referer: https://www.fantasylabs.com/nba/player-models/?date=",corrDate,"' -H 'authority: www.fantasylabs.com'","-H 'Cookie: ",get("nbaCookies",envir = .GlobalEnv),"  -o ",y6," --compressed")
system(corrCurl)
playerCors<-jsonlite::fromJSON(y6) %>% data.frame()
playerCors<-playerCors$Properties %>% data.frame() %>% dplyr::mutate(PlayerId=corrPID)
dir.create(paste0("~/NBA_Daily/correlations/",nba$Player_Name[i]),showWarnings = FALSE)
write.csv(playerCors,file=paste0("~/NBA_Daily/correlations/",nba$Player_Name[i],"/",nba$Player_Name[i],".csv"))
return(playerCors)
}
suppressMessages(system(command = "cd ~/NBA_Daily/json/ && rm -rf *"))

nba18AllCors<-dplyr::bind_rows(nba18PlayerCors)
nba18AllCorsWPName<-dplyr::right_join(nba18AllCors,nba[,c("PlayerId","Player_Name")],by = "PlayerId")
nba18AllCorsWPName<-nba18AllCorsWPName[,c(6,1:5)]

dir.create(paste0("~/NBA_Daily/correlations/byDate"),showWarnings = FALSE)
dir.create(paste0("~/NBA_Daily/correlations/byDate/",modelDate),showWarnings = FALSE)
dir.create(paste0("~/NBA_Daily/correlations/byDate/",modelDate,"/",gameSlate[j]),showWarnings = FALSE)

write.csv(nba18AllCorsWPName,file=paste0("~/NBA_Daily/correlations/byDate/",modelDate,"/",gameSlate[j],"/","playerCorrelations.csv"))

setwd(hmDir)
}
}
