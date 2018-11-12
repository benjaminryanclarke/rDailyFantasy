#' getBBMCookies
#'
#' @return all BBM Cookies
#' @export
#'
#' @examples getBBMCookies()
getBBMCookies<-function(dockerName="monsterLab"){

  require(wdman,quietly = TRUE,warn.conflicts = F)
  require(foreach,quietly = TRUE,warn.conflicts = F)
  require(rapportools,quietly = TRUE,warn.conflicts = F)
  require(dplyr,quietly = TRUE,warn.conflicts = F)
  require(RSelenium,quietly = TRUE,warn.conflicts = F)
  require(stringi,quietly = TRUE,warn.conflicts = F)


  Sys.setenv(PATH="/usr/local/bin:/usr/bin:/usr/local/git/bin:/sw/bin:/usr/local/bin:/usr/local:/usr/local/sbin:/usr/local/mysql/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/usr/local/MacGPG2/bin:/opt/X11/bin:/usr/local/bin:/Users/Ben/bin")

  rDailyFantasy::waitFor(dplyr::if_else(rapportools::is.empty(if_else(suppressMessages(system(command = "dockerRunning",intern = TRUE))=="45",45,2,missing = 1)),as.numeric(1),as.numeric(45)))
  if(dplyr::if_else(length(suppressMessages(system("docker ps",intern = T)))==2,TRUE,FALSE)){suppressMessages(system(paste0("docker kill ",dockerName," && docker rm ",dockerName)))}  #create docker Environment for chromedriver to get fantasy labs session cookies
  suppressMessages(system(command=paste0("docker run -d --name ","'",dockerName,"'"," -p 4445:4444 selenium/standalone-firefox"),intern = FALSE,ignore.stdout = T))

  waitFor(4)

  web<-suppressMessages(RSelenium::remoteDriver(port=4445L))
  web$open(silent = TRUE)
  web$navigate(url="https://basketballmonster.com/login.aspx")
  usernameBox <- web$findElement(using='css selector',"#ContentPlaceHolder1_UsernameTextBox")
  passwordBox <- web$findElement(using='css selector',"#ContentPlaceHolder1_PasswordTextBox")
  usernameBox$sendKeysToElement(list("Benjaminryanclarke"))
  passwordBox$sendKeysToElement(list("tjk589682"))
  web$findElement("css selector", "#ContentPlaceHolder1_LoginButton")$clickElement()
  waitFor(7)
  web$navigate(url="https://basketballmonster.com/dfsdvp.aspx")

  monsterCookies <- web$getAllCookies()
  monsterCookiesDF <- foreach(i=1:length(monsterCookies)) %do% bind_rows(monsterCookies[i][[1]] %>% flatten())
  #monsterCookiesDF <- data.frame(dplyr::bind_rows(monsterCookies))
  #cookieNumb<- dim(monsterCookiesDF)[[1]]
  monsterCookiesDF1 <- monsterCookiesDF[1][[1]] %>% dplyr::mutate(expiry=NA)
  monsterCookiesDF1 <- monsterCookiesDF1[c(1,2,3,7,4,5,6)]
  cookieNumbDk<- length(monsterCookiesDF)
  monsterCookiesDF <- dplyr::bind_rows(monsterCookiesDF[2:cookieNumbDk],monsterCookiesDF1)
  cookiesDFDK<-data.frame(monsterCookiesDF$name,monsterCookiesDF$value)
  cookiesListDK<- foreach(i=1:cookieNumbDk) %do% paste0(cookiesDFDK$monsterCookiesDF.name[i],"=",cookiesDFDK$monsterCookiesDF.value[i])

  #finalCookies<-paste0(cookiesList[1],"; ",cookiesList[2],"; ",cookiesList[3],"; ",cookiesList[4],"; ",cookiesList[5],"; ",cookiesList[6],"; ",cookiesList[7],"; ",cookiesList[8],"; ",cookiesList[9],"; ",cookiesList[10],"; ",cookiesList[12],"; ",cookiesList[12],"; ",cookiesList[13])
  finalCookiesDK<-unlist(list(unlist(foreach::foreach(j=2:length(cookiesListDK)-1) %do% paste0(cookiesListDK[j],"; ")),cookiesListDK[length(cookiesListDK)]))
  finalCookiesDK<-stringi::stri_c_list(list(finalCookiesDK))
  finalCookiesDK <- paste0(finalCookiesDK,"'")



  web$close()



  #rd[["server"]]$stop()
  #rm(rd)
  suppressMessages(system(command=paste0("docker stop ",dockerName),intern = F,ignore.stdout = T))
  suppressMessages(system(command=paste0("docker rm ",dockerName),intern = F,ignore.stdout = T))


  return(finalCookiesDK)

}
