#' lab Cookies Retrieval Functions
#'
#' @param dockerName docker container name(can be anything)
#' @param sport mlb,nba,nfl,nhl,pga
#'
#' @return all FantasyLab Cookies
#' @export
#'
#' @examples pgaCookies<-labsCookies("labCookies","pga")
labsCookies<-function(dockerName="labCookies",sport="pga"){

  suppressPackageStartupMessages(require(wdman,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(foreach,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(rapportools,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(dplyr,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(RSelenium,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(stringi,quietly = TRUE,warn.conflicts = F))

  #set $PATH in environment so system can find dockerRunning exec
  Sys.setenv(PATH="/usr/local/bin:/usr/bin:/usr/local/git/bin:/sw/bin:/usr/local/bin:/usr/local:/usr/local/sbin:/usr/local/mysql/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/usr/local/MacGPG2/bin:/opt/X11/bin:/usr/local/bin:/Users/Ben/bin")
  ####notrun(checklater)
  #start docker daemon if not already running
  #dockerShell <- Sys.getenv('R_HOME')
  #dockerWait<-as.numeric(system(command = paste0("cd ",dockerShell," && source Renviron && dockerRunning"),intern = TRUE))
  #let docker load if it needs to
  rDailyFantasy::waitFor(dplyr::if_else(rapportools::is.empty(if_else(suppressMessages(system(command = "dockerRunning",intern = TRUE))=="45",45,2,missing = 1)),as.numeric(1),as.numeric(45)))
  if(dplyr::if_else(length(suppressMessages(system("docker ps",intern = T)))==2,TRUE,FALSE)){suppressMessages(system(paste0("docker kill ",dockerName," && docker rm ",dockerName)))}  #create docker Environment for chromedriver to get fantasy labs session cookies
  suppressMessages(system(command=paste0("docker run -d --name ","'",dockerName,"'"," -p 4445:4444 selenium/standalone-firefox"),intern = FALSE,ignore.stdout = T))
  ####notRun(checkLater)
  #rd<-rsDriver(browser="firefox",verbose=FALSE)
  #web <- rd[["client"]]
  waitFor(3)
  web<-suppressMessages(RSelenium::remoteDriver(port=4445L))
  web$open(silent = TRUE)
  web$navigate(url="http://fantasylabs.com/account/login")
  usernameBox <- web$findElement(using='xpath',"/html/body/div[3]/form[1]/div[2]/div/input")
  passwordBox <- web$findElement(using='xpath',"/html/body/div[3]/form[1]/div[3]/div/input")
  usernameBox$sendKeysToElement(list(read.csv("~/rDFS/rDailyFantasy/passwordsFile.csv")[2][[1]][1]))
  passwordBox$sendKeysToElement(list(read.csv("~/rDFS/rDailyFantasy/passwordsFile.csv")[3][[1]][1]))
  web$findElement("xpath", "/html/body/div[3]/form[1]/div[4]/button")$clickElement()
  Sys.sleep(6)
  web$navigate(url=paste0("http://www.fantasylabs.com/",sport,"/player-models/"))

  LabsCookies <- web$getAllCookies()
  labsCookiesDF <- data.frame(dplyr::bind_rows(LabsCookies))
  cookieNumb<- dim(labsCookiesDF)[[1]]
  cookiesDF<-data.frame(labsCookiesDF$name,labsCookiesDF$value)
  cookiesList<- foreach(i=1:cookieNumb) %do% paste0(cookiesDF$labsCookiesDF.name[i],"=",cookiesDF$labsCookiesDF.value[i])

  finalCookies<-paste0(cookiesList[1],"; ",cookiesList[2],"; ",cookiesList[3],"; ",cookiesList[4],"; ",cookiesList[5],"; ",cookiesList[6],"; ",cookiesList[7],"; ",cookiesList[8],"; ",cookiesList[9],"; ",cookiesList[10],"; ",cookiesList[12],"; ",cookiesList[12],"; ",cookiesList[13])
  finalCookies<-unlist(list(unlist(foreach::foreach(j=2:length(cookiesList)-1) %do% paste0(cookiesList[j],"; ")),cookiesList[length(cookiesList)]))
  finalCookies<-stringi::stri_c_list(list(finalCookies))
  finalCookies <- paste0(finalCookies,"'")


  ##DKCookies
  web$navigate(url="https://www.draftkings.com/account/sitelogin/false?returnurl=%2Flobby")
  usernameBox <- web$findElement(using='xpath',paste0('//*[@id="react-mobile-home"]/section/section[2]/div[2]/div[3]/div/input'))
  passwordBox <- web$findElement(using='xpath',paste0('//*[@id="react-mobile-home"]/section/section[2]/div[2]/div[4]/div/input'))
  usernameBox$sendKeysToElement(list(read.csv("~/rDFS/rDailyFantasy/passwordsFile.csv")[2][[1]][2]))
  passwordBox$sendKeysToElement(list(read.csv("~/rDFS/rDailyFantasy/passwordsFile.csv")[3][[1]][2]))
  web$findElement("xpath", paste0('//*[@id="react-mobile-home"]/section/section[2]/div[3]/button'))$clickElement()
  Sys.sleep(8)
  web$navigate(url=paste0("https://www.draftkings.com/bulkentryedit/"))

  dkCookies <- web$getAllCookies()
  dkCookiesDF <- foreach(i=1:length(dkCookies)) %do% bind_rows(dkCookies[i][[1]] %>% flatten())
  #dkCookiesDF <- data.frame(dplyr::bind_rows(dkCookies))
  #cookieNumb<- dim(dkCookiesDF)[[1]]
  dkCookiesDF1 <- dkCookiesDF[1][[1]] %>% dplyr::mutate(expiry=NA)
  dkCookiesDF1 <- dkCookiesDF1[c(1,2,3,7,4,5,6)]
  cookieNumbDk<- length(dkCookiesDF)
  dkCookiesDF <- dplyr::bind_rows(dkCookiesDF[2:cookieNumbDk],dkCookiesDF1)
  cookiesDFDK<-data.frame(dkCookiesDF$name,dkCookiesDF$value)
  cookiesListDK<- foreach(i=1:cookieNumbDk) %do% paste0(cookiesDFDK$dkCookiesDF.name[i],"=",cookiesDFDK$dkCookiesDF.value[i])

  #finalCookies<-paste0(cookiesList[1],"; ",cookiesList[2],"; ",cookiesList[3],"; ",cookiesList[4],"; ",cookiesList[5],"; ",cookiesList[6],"; ",cookiesList[7],"; ",cookiesList[8],"; ",cookiesList[9],"; ",cookiesList[10],"; ",cookiesList[12],"; ",cookiesList[12],"; ",cookiesList[13])
  finalCookiesDK<-unlist(list(unlist(foreach::foreach(j=2:length(cookiesListDK)-1) %do% paste0(cookiesListDK[j],"; ")),cookiesListDK[length(cookiesListDK)]))
  finalCookiesDK<-stringi::stri_c_list(list(finalCookiesDK))
  finalCookiesDK <- paste0(finalCookiesDK,"'")



  web$close()



  #rd[["server"]]$stop()
  #rm(rd)
  suppressMessages(system(command=paste0("docker stop ",dockerName),intern = F,ignore.stdout = T))
  suppressMessages(system(command=paste0("docker rm ",dockerName),intern = F,ignore.stdout = T))

  return(list(labs=finalCookies,dk=finalCookiesDK))
}
