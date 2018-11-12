#' Monster DVP function
#'
#' @param modelDate modelDate
#' @param range c("Full Season","1Week","2Weeks","1Month","PastDays","DateRange")
#' @param days # of days if range="PastDays"
#' @param dates start and end dates in c("mm/dd/yyyy","mm/dd/yyyy") format
#' @param bbmCookie cookies from basketballmonster.com session
#' @param cookieRefresh T/F
#'
#' @return dvp for specified range
#' @export
#'
#' @examples monsterDVP(modelDate="10_31_2018",range="FullSeason",monsterCookies=mosterCookies)
monsterDVP<-function(modelDate="10_31_2018",range="FullSeason",days=NULL,dates=NULL,bbmCookie=monsterCookies,cookieRefresh=FALSE){
  #check for cookies data
  if(!exists("monsterCookies")){
    readline("missing Cookies, attempting to get them now!")
    monsterCookies<-rDailyFantasy::getBBMCookies()
    cookie<-monsterCookies

    assign("monsterCookies",monsterCookies,envir = .GlobalEnv)
  }

  if(cookieRefresh==TRUE){
    monsterCookies<-rDailyFantasy::getBBMCookies()
    cookie<-monsterCookies

    assign("monsterCookies",monsterCookies,envir = .GlobalEnv)
  }

  hmDir<-getwd()

  mYear<-tidyr::separate(as.data.frame(modelDate),col=modelDate,into = c("mMonth","mDay","mYear"),by = "_")[,'mYear']
  mDay<-tidyr::separate(as.data.frame(modelDate),col=modelDate,into = c("mMonth","mDay","mYear"),by = "_")[,'mDay']
  mMonth<-tidyr::separate(as.data.frame(modelDate),col=modelDate,into = c("mMonth","mDay","mYear"),by = "_")[,'mMonth']

  setwd(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate))

  #DateRange
  if(range=="DateRange"){
  dateFix<-gsub("/","%2F",dates,fixed = TRUE)
  system(command = paste0("curl -L 'https://basketballmonster.com/dfsdvp.aspx' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: https://basketballmonster.com' -H 'Upgrade-Insecure-Requests: 1' -H 'DNT: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://basketballmonster.com/dfsdvp.aspx' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",bbmCookie," --data '__EVENTTARGET=&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATEFIELDCOUNT=2&__VIEWSTATE=%2FwEPDwUJMzI3MzEzMjQ0ZGTVdGWgpocg5Ew0K%2FQJ&__VIEWSTATE1=UZiX5npWiqfIXEh8R4QTCvVWPw%3D%3D&__VIEWSTATEGENERATOR=A2289608&ctl00%24NameTextBox=&DateFilterControl=DateRange&DateFilterControlSTART=",dateFix[1],"&DateFilterControlEND=",dateFix[2],"&DAILYTYPEDROPDOWN=4&REFRESH=Refresh&TODAYONLY=on&hiddenInputToUpdateATBuffer_CommonToolkitScripts=1' -o bbmDVPCurl",range,".html --compressed"))
  }
  #FullSeason
  if(range=="FullSeason"){
    system(command = paste0("curl -L 'https://basketballmonster.com/dfsdvp.aspx' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: https://basketballmonster.com' -H 'Upgrade-Insecure-Requests: 1' -H 'DNT: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://basketballmonster.com/dfsdvp.aspx' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",bbmCookie," --data '__EVENTTARGET=TODAYONLY&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATEFIELDCOUNT=2&__VIEWSTATE=%2FwEPDwUJMzI3MzEzMjQ0ZGTVdGWgpocg5Ew0K%2FQJ&__VIEWSTATE1=UZiX5npWiqfIXEh8R4QTCvVWPw%3D%3D&__VIEWSTATEGENERATOR=A2289608&ctl00%24NameTextBox=&DateFilterControl=FullSeason&DAILYTYPEDROPDOWN=4&TODAYONLY=on&hiddenInputToUpdateATBuffer_CommonToolkitScripts=1' -o bbmDVPCurl",range,".html --compressed"))
  }
  #1Week
  if(range=="2Weeks"){
    system(command = paste0("curl -L 'https://basketballmonster.com/dfsdvp.aspx' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: https://basketballmonster.com' -H 'Upgrade-Insecure-Requests: 1' -H 'DNT: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://basketballmonster.com/dfsdvp.aspx' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",bbmCookie," --data '__EVENTTARGET=DateFilterControl&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATEFIELDCOUNT=2&__VIEWSTATE=%2FwEPDwUJMzI3MzEzMjQ0ZGTVdGWgpocg5Ew0K%2FQJ&__VIEWSTATE1=UZiX5npWiqfIXEh8R4QTCvVWPw%3D%3D&__VIEWSTATEGENERATOR=A2289608&ctl00%24NameTextBox=&DateFilterControl=LastTwoWeeks&DAILYTYPEDROPDOWN=4&TODAYONLY=on&hiddenInputToUpdateATBuffer_CommonToolkitScripts=1' -o bbmDVPCurl",range,".html --compressed"))
  }
  #2Weeks
  if(range=="1Week"){
    system(command = paste0("curl -L 'https://basketballmonster.com/dfsdvp.aspx' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: https://basketballmonster.com' -H 'Upgrade-Insecure-Requests: 1' -H 'DNT: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://basketballmonster.com/dfsdvp.aspx' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",bbmCookie," --data '__EVENTTARGET=DateFilterControl&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATEFIELDCOUNT=2&__VIEWSTATE=%2FwEPDwUJMzI3MzEzMjQ0ZGTVdGWgpocg5Ew0K%2FQJ&__VIEWSTATE1=UZiX5npWiqfIXEh8R4QTCvVWPw%3D%3D&__VIEWSTATEGENERATOR=A2289608&ctl00%24NameTextBox=&DateFilterControl=LastWeek&DAILYTYPEDROPDOWN=4&TODAYONLY=on&hiddenInputToUpdateATBuffer_CommonToolkitScripts=1' -o bbmDVPCurl",range,".html --compressed"))
  }
  #1Month
  if(range=="1Month"){
    system(command = paste0("curl -L 'https://basketballmonster.com/dfsdvp.aspx' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: https://basketballmonster.com' -H 'Upgrade-Insecure-Requests: 1' -H 'DNT: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://basketballmonster.com/dfsdvp.aspx' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",bbmCookie," --data '__EVENTTARGET=DateFilterControl&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATEFIELDCOUNT=2&__VIEWSTATE=%2FwEPDwUJMzI3MzEzMjQ0ZGTVdGWgpocg5Ew0K%2FQJ&__VIEWSTATE1=UZiX5npWiqfIXEh8R4QTCvVWPw%3D%3D&__VIEWSTATEGENERATOR=A2289608&ctl00%24NameTextBox=&DateFilterControl=LastMonth&DAILYTYPEDROPDOWN=4&TODAYONLY=on&hiddenInputToUpdateATBuffer_CommonToolkitScripts=1' -o bbmDVPCurl",range,".html --compressed"))
  }
  #PastDays
  if(range=="PastDays"){
    system(command = paste0("curl -L 'https://basketballmonster.com/dfsdvp.aspx' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: https://basketballmonster.com' -H 'Upgrade-Insecure-Requests: 1' -H 'DNT: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://basketballmonster.com/dfsdvp.aspx' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",bbmCookie," --data '__EVENTTARGET=&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATEFIELDCOUNT=2&__VIEWSTATE=%2FwEPDwUJMzI3MzEzMjQ0ZGTVdGWgpocg5Ew0K%2FQJ&__VIEWSTATE1=UZiX5npWiqfIXEh8R4QTCvVWPw%3D%3D&__VIEWSTATEGENERATOR=A2289608&ctl00%24NameTextBox=&DateFilterControl=PastDays&DateFilterControlDAYS=",days,"&DAILYTYPEDROPDOWN=4&REFRESH=Refresh&TODAYONLY=on&hiddenInputToUpdateATBuffer_CommonToolkitScripts=1' -o bbmDVPCurl",range,".html --compressed"))
  }

  table<-read_html(file(paste0("bbmDVPCurl",range,".html"))) %>% html_node(xpath = '//*[@id="form1"]/div[3]/div[2]/table') %>% html_table()
  write.csv(table,file=paste0("monsterDVP",range,".csv"))

  setwd(hmDir)

  return(table)

}
