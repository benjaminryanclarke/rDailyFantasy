#' nbaResults Function
#'
#' @param modelDate "11_10_2018"
#'
#' @return contest Results Summaries from modelDates Contests
#' @export
#'
#' @examples nbaResults(modelDate="11_10_2018")
nbaResults<-function(modelDate="11_10_2018"){

  hmDir<-getwd()

  modelDateURL<-gsub(pattern = "_",replacement = "/",x = modelDate)
  modelDateSplit<-data.frame(modelDate) %>% setNames(nm = c("modelDate")) %>% tidyr::separate(col = "modelDate",into = c("mMonth","mDay","mYear"),sep = "_") %>% data.frame()
  mMonth<-modelDateSplit[,1]
  mDay<-modelDateSplit[,2]
  mYear<-modelDateSplit[,3]
  fixD<-as.Date(paste0(mMonth,"/",mDay,"/",mYear), "%m/%d/%Y")
  modelDateURLFixed<-paste0(mMonth,"/",mDay,"/",mYear)

  results<-jsonlite::fromJSON(url(paste0("https://resultsdb-api.rotogrinders.com/api/slates?start=",modelDateURLFixed))) %>% dplyr::filter(sport==3,slateTypeName=="Classic") %>% dplyr::select(`_id`,siteSlateId)
  source<-read.csv(paste0("~/NBA_Daily/sourceData/",mYear,"/",modelDate,"/fullSourceData.csv")) %>% dplyr::select(ContestSuffix,DraftGroupId)
  source$DraftGroupId<-as.character(source$DraftGroupId)
  joined<- dplyr::inner_join(results,source,by=c("siteSlateId"="DraftGroupId"))

  dir.create(path=paste0("~/NBA_Daily/results/"),showWarnings = F)

  foreach(i=1:nrow(joined)) %do% {
    dir.create(path=paste0("~/NBA_Daily/results/",mYear),showWarnings = F)
    dir.create(path=paste0("~/NBA_Daily/results/",mYear,"/",modelDate),showWarnings = F)
    dir.create(path=paste0("~/NBA_Daily/results/",mYear,"/",modelDate,"/",joined[i,3]),showWarnings = F)
    setwd(paste0("~/NBA_Daily/results/",mYear,"/",modelDate,"/",joined[i,3]))
    system(command=paste0("curl https://resultsdb-api.rotogrinders.com/api/slates/",joined[i,1],"/summary -o results.json"),ignore.stdout = TRUE)
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
