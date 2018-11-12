#' nfl Contest Results Function
#'
#' @param modelYear 2018
#' @param modelWeek 1-17
#'
#' @return contest results in NFL_Daily/results directory
#' @export
#'
#' @examples nflResults(2018,7)
nflResults<-function(modelYear=2018,modelWeek=7){

  hmDir<-getwd()

  nfl2014Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2014/09/03", weeks = 17)
  nfl2015Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2015/09/09", weeks = 17)
  nfl2016Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2016/09/07", weeks = 17)
  nfl2017Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2017/09/07", weeks = 17)
  nfl2018Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2018/09/05", weeks = 17)
  nflDates <- data.frame("2014"=nfl2014Dates,"2015"=nfl2015Dates,"2016"=nfl2016Dates,"2017"=nfl2017Dates,"2018"=nfl2018Dates)
  nflDateColumn <- dplyr::if_else(modelYear==2014,1,dplyr::if_else(modelYear==2015,2,dplyr::if_else(modelYear==2016,3,dplyr::if_else(modelYear==2017,4,dplyr::if_else(modelYear==2018,5,0)))))
  modelDate <- nflDates[,nflDateColumn][modelWeek]
  modelDateURL<-gsub(pattern = "_",replacement = "/",x = modelDate)
  #split modelDate into separate variables
  modelDateSplit<-data.frame(modelDate) %>% setNames(nm = c("modelDate")) %>% tidyr::separate(col = "modelDate",into = c("mMonth","mDay","mYear"),sep = "_") %>% data.frame()
  mMonth<-modelDateSplit[,1]
  mDay<-modelDateSplit[,2]
  mYear<-modelDateSplit[,3]
  fixD<-as.Date(paste0(mMonth,"/",mDay,"/",mYear), "%m/%d/%Y")
  fixD2<-fixD+4
  fixD3<-as.data.frame(fixD2) %>% tidyr::separate(col=fixD2,into=c("YYear","MMonth","DDay"),sep="-")
  modelDateURLFixed<-paste0(fixD3$MMonth,"/",fixD3$DDay,"/",fixD3$YYear)


  results<-jsonlite::fromJSON(url(paste0("https://resultsdb-api.rotogrinders.com/api/slates?start=",modelDateURLFixed))) %>% dplyr::filter(sport==1,slateTypeName=="Classic") %>% dplyr::select(`_id`,siteSlateId)
  source<-read.csv(paste0("~/NFL_Daily/sourceData/",mYear,"/",modelWeek,"/fullSourceData.csv")) %>% dplyr::select(ContestSuffix,DraftGroupId)
  source$DraftGroupId<-as.character(source$DraftGroupId)
  joined<- dplyr::inner_join(results,source,by=c("siteSlateId"="DraftGroupId"))

  dir.create(path=paste0("~/NFL_Daily/results/"),showWarnings = F)


  foreach(i=1:nrow(joined)) %do% {
    dir.create(path=paste0("~/NFL_Daily/results/",mYear),showWarnings = F)
    dir.create(path=paste0("~/NFL_Daily/results/",mYear,"/",modelDate),showWarnings = F)
    dir.create(path=paste0("~/NFL_Daily/results/",mYear,"/",modelDate,"/",joined[i,3]),showWarnings = F)
    setwd(paste0("~/NFL_Daily/results/",mYear,"/",modelDate,"/",joined[i,3]))
    system(command=paste0("curl https://resultsdb-api.rotogrinders.com/api/slates/",joined[i,1],"/summary -o results.json"))
    jsonIn<-jsonlite::fromJSON(file("results.json"))
    listRet<-list(bestLineUp=jsonIn$topScoringEntry$lineup$summary,gppAverage=jsonIn$gppAverage,cashAverage=jsonIn$cashAverage,best=jsonIn$topEntry$fpts %>% sum())
    listRet <- list(listRet) %>% set_names(nm=paste0(joined[i,3]))
    if(is.null(listRet[[1]]$bestLineUp)){
      listRet<-listRet[[1]][2:3]
    }
    write.csv(listRet,file="results.csv")
    return(listRet)
  }

  setwd(hmDir)

}
