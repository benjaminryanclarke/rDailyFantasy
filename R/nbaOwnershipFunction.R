#' NBA ownership file
#'
#' @param modelDate
#'
#' @return ownership data
#' @export
#'
#' @examples nbaOwn(modelDate="2_8_2019",cookies=nbaCookies)
nbaOwn<-function(modelDate,cookies=nbaCookies){
  dir.create(path=paste0("~/NBA_Daily/ownership"),showWarnings = FALSE)
  dir.create(path=paste0("~/NBA_Daily/ownership/2018"),showWarnings = FALSE)
  dir.create(path=paste0("~/NBA_Daily/ownership/2018/",modelDate),showWarnings = FALSE)
  setwd(paste0("~/NBA_Daily/ownership/2018/",modelDate))
  system(command=paste0("curl -L 'https://www.fantasylabs.com/api/contest-ownership/2/",modelDate,"/4/' -H 'dnt: 1' -H 'accept-encoding: gzip, deflate, br' -H 'accept-language: en-US,en;q=0.9' -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36' -H 'accept: application/json, text/plain, */*' -H 'referer: https://www.fantasylabs.com/nba/contest-ownership/' -H 'authority: www.fantasylabs.com' -H 'cookie: ",cookies," -o ownership.json --compressed"),ignore.stderr = TRUE)
  ownDf<-jsonlite::fromJSON('ownership.json')
  ownDf<-ownDf$Properties
  write.csv(ownDf,file=paste0("~/NBA_Daily/ownership/2018/",modelDate,"/ownership.csv"))
  return(ownDf)
}
