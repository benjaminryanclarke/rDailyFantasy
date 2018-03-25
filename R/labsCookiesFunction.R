#' lab Cookies Retrieval Functions
#'
#' @param dockerName docker container name(can be anything)
#' @param sport mlb,nba,nfl,nhl,pga
#'
#' @return all FantasyLab Cookies
#' @export
#'
#' @examples pgaCookies<-labsCookies("labCookies","mlb")
labsCookies<-function(dockerName="labCookies",sport="pga"){

  require(wdman,quietly = TRUE)
  require(foreach,quietly = TRUE)
  require(rapportools,quietly = TRUE)
  require(dplyr,quietly = TRUE)
  require(RSelenium,quietly = TRUE)
  require(stringi,quietly = TRUE)
  
  #set $PATH in environment so system can find dockerRunning exec
  Sys.setenv(PATH="/usr/local/bin:/usr/bin:/usr/local/git/bin:/sw/bin:/usr/local/bin:/usr/local:/usr/local/sbin:/usr/local/mysql/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/usr/local/MacGPG2/bin:/opt/X11/bin:/usr/local/bin:/Users/Ben/bin")
  ####notrun(checklater)
  #start docker daemon if not already running
  #dockerShell <- Sys.getenv('R_HOME')
  #dockerWait<-as.numeric(system(command = paste0("cd ",dockerShell," && source Renviron && dockerRunning"),intern = TRUE))
  #let docker load if it needs to
  rPgaDfs::waitFor(dplyr::if_else(rapportools::is.empty(if_else(system(command = "dockerRunning",intern = TRUE)=="45",45,2,missing = 1)),as.numeric(1),as.numeric(45)))
  if(dplyr::if_else(length(system("docker ps",intern = T))==2,TRUE,FALSE)){system(paste0("docker kill ",dockerName," && docker rm ",dockerName))}  #create docker Environment for chromedriver to get fantasy labs session cookies
  system(command=paste0("docker run -d --name ","'",dockerName,"'"," -p 4445:4444 selenium/standalone-firefox"),intern = TRUE)
  ####notRun(checkLater)
  #rd<-rsDriver(browser="firefox",verbose=FALSE)
  #web <- rd[["client"]]
  waitFor(3)
  web<-RSelenium::remoteDriver(port=4445L)
  web$open()
  web$navigate(url="http://fantasylabs.com/account/login")
  usernameBox <- web$findElement(using='xpath',"/html/body/div[3]/form[1]/div[2]/div/input")
  passwordBox <- web$findElement(using='xpath',"/html/body/div[3]/form[1]/div[3]/div/input")
  usernameBox$sendKeysToElement(list("Benjaminryanshopping@icloud.com"))
  passwordBox$sendKeysToElement(list("W3ytjk589682"))
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
  web$close()



  #rd[["server"]]$stop()
  #rm(rd)
  system(command=paste0("docker stop ",dockerName))
  system(command=paste0("docker rm ",dockerName))

  return(finalCookies)
}
