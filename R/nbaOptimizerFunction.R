#' nbaOptimizerFunction
#'
#' @param modelDate date
#' @param gameSlate slate
#' @param lineUps #lineups
#' @param bans exclude
#' @param locks include
#' @param saveNameAdd russellAndFlow
#' @param sameDay optimizing for same day as running function? T/F
#' @param cookie fantasyLabs Cookies
#' @param dCookies draftkings Cookies
#'
#' @return lineups
#' @export
#'
#' @examples nbaOptimizer(modelDate="11_10_2018",gameSlate="Main",sameDay=TRUE,lineUps=200L,bans=NULL,locks=c(""),saveNameAdd="zeroRussellAndFlow",cookie=nbaCookies,dCookies=dkCookies)
nbaOptimizer<-function(modelDate="11_10_2018",gameSlate="Main",sameDay=TRUE,lineUps=200L,bans=NULL,locks=c(""),saveNameAdd="zeroRussellAndFlow",cookie=nbaCookies,dCookies=dkCookies){

  require(coach, quietly = TRUE,warn.conflicts = F)

  #check for draftkings Cookies
  if(!exists("dkCookies")){
    readline("missing Cookies, attempting to get them now!")
    dkCookies<-rDailyFantasy::labsCookies()
    dCookies<-dkCookies$dk

    #assign("nbaCookies",nbaCookies$labs,envir = .GlobalEnv)
    assign("dkCookies",dkCookies$dk,envir = .GlobalEnv)
  }

  hmDir<-getwd()



  #separate modelDate into month,day,year objects
  modelDateSplit<-data.frame(modelDate) %>% setNames(nm = c("modelDate")) %>% tidyr::separate(col = "modelDate",into = c("mMonth","mDay","mYear"),sep = "_") %>% data.frame()
  mMonth<-modelDateSplit[,1]
  mDay<-modelDateSplit[,2]
  mYear<-modelDateSplit[,3]

  #get ContestGroupId for draftkingsData
  cgIdDf<-read.csv(file=paste0("~/NBA_Daily/sourceData/",mYear,"/",modelDate,"/",gameSlate,"/source.csv")) %>% dplyr::select(-X)
  cgId<-cgIdDf[,"DraftGroupId"]

  #create directory for dkData files
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear),showWarnings = FALSE)
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate),showWarnings = FALSE)
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate),showWarnings = FALSE)
  #DKSalaries directory
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKSalaries"),showWarnings = FALSE)
  #DKEntries directory
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKEntries"),showWarnings = FALSE)
  #fantasyLabs directory
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/fantasyLabs"),showWarnings = FALSE)

  #if sameDay as contest
  #download DKSalaries
  if(sameDay==TRUE){
  setwd(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKSalaries"))
  system(command = paste0("curl https://www.draftkings.com/bulklineup/getdraftablecsv?draftGroupId=",cgId," -H 'Connection: keep-alive' -H 'Upgrade-Insecure-Requests: 1' -H 'DNT: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://www.draftkings.com/lineup/upload' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",dCookies," -o DKSalaries.csv --compressed"))
  #import DKSalaries original
  dkData<-read.csv(file=paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKSalaries/DKSalaries.csv"),quote = "",skip = 7)[,10:18]
  }else{
  #download DKEntries
   # if(length(read.csv(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKEntries/DKEntries.csv")))<=1)
  setwd(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKEntries"))
  system(command = paste0("curl -L https://www.draftkings.com/bulkentryedit/getentriescsv?draftGroupId=",cgId," -H 'Connection: keep-alive' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36' -H 'DNT: 1' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: ",dCookies," -o DKEntries.csv --compressed"))
  #setwd(hmDir)
  #fix DKSalaries for optimization
  #import DKSalaries original
  dkData<-read.csv(file=paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKEntries/DKEntries.csv"),quote = "",skip = 7)[,14:22]
  }

  setwd(hmDir)

  #set Names for DKSalariesOptim
  names(dkData)<-c("Position","Name + ID","Name","ID","Roster Position","Salary","Game Info","TeamAbbrev","AvgPointsPerGame")
  #dkData$`Game Info`<-paste0("@",dkData$TeamAbbrev)
  #write DKSalariesOptim
  dkNameFix1<-gsub("(LOCKED)","",as.character(dkData$`Name + ID`))
  dkNameFix2<-gsub("\\s*\\()","",as.character(dkNameFix1))
  dkData$`Name + ID` <- dkNameFix2

  ##Get eventData for "Game Info" of past games for optimizer to work
  gameInfoURL <- paste0("https://www.fantasylabs.com/api/sportevents/2/",modelDate,"?range=true")
  setwd(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate))
  y6 <- paste0("gameInfo.json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y6," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",gameInfoURL," ",pmCurlHandles)
  #download gameInfo json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  gameInfoDf <- jsonlite::fromJSON(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/",y6), simplifyDataFrame = TRUE)
  foreach(i=1:nrow(gameInfoDf)) %do% {
    if(nchar(gameInfoDf$EventTime[i])==11){
      gameInfoDf$EventTime[i]<-paste0(0,gameInfoDf$EventTime[i])
    }
  }
  rightDate<-paste0(mYear,"-",mMonth,"-",mDay,"T00:00:00")
  gameInfoDf<-gameInfoDf %>% dplyr::filter(EventDate==rightDate)
  eventTimeSep<- tidyr::separate(gameInfoDf,col="EventTime",into=c("Time","PM","EST"),sep=" ")
  gameInfoDf[gameInfoDf$VisitorTeamShort == "NYK","VisitorTeamShort"]<-"NY"
  gameInfoDf[gameInfoDf$HomeTeamShort == "NYK","HomeTeamShort"]<-"NY"
  gameInfoDf[gameInfoDf$VisitorTeamShort == "SAS","VisitorTeamShort"]<-"SA"
  gameInfoDf[gameInfoDf$HomeTeamShort == "SAS","HomeTeamShort"]<-"SA"
  gameInfoDf[gameInfoDf$VisitorTeamShort == "PHX","VisitorTeamShort"]<-"PHO"
  gameInfoDf[gameInfoDf$HomeTeamShort == "PHX","HomeTeamShort"]<-"PHO"
  gameInfoDf[gameInfoDf$VisitorTeamShort == "GSW","VisitorTeamShort"]<-"GS"
  gameInfoDf[gameInfoDf$HomeTeamShort == "GSW","HomeTeamShort"]<-"GS"
  gameInfoDf[gameInfoDf$VisitorTeamShort == "NOP","VisitorTeamShort"]<-"NO"
  gameInfoDf[gameInfoDf$HomeTeamShort == "NOP","HomeTeamShort"]<-"NO"

  gameInfoFix<-paste0(gameInfoDf$VisitorTeamShort,"@",gameInfoDf$HomeTeamShort," ",mMonth,"/",mDay,"/",mYear," ",eventTimeSep$Time," ",eventTimeSep$PM," ET")
  gameInfoFixDf<-bind_rows(data.frame(Team=gameInfoDf$HomeTeamShort,gameInfoFix=gameInfoFix),data.frame(Team=gameInfoDf$VisitorTeamShort,gameInfoFix=gameInfoFix))
  gameInfoFixDf[gameInfoFixDf$Team == "NYK","Team"]<-"NY"
  gameInfoFixDf[gameInfoFixDf$Team == "SAS","Team"]<-"SA"
  gameInfoFixDf[gameInfoFixDf$Team == "PHX","Team"]<-"PHO"
  gameInfoFixDf[gameInfoFixDf$Team == "GSW","Team"]<-"GS"
  gameInfoFixDf[gameInfoFixDf$Team == "NOP","Team"]<-"NO"

  gameInfoJoin<-dplyr::inner_join(dkData[,c(1:6,8,9)],gameInfoFixDf,by=c("TeamAbbrev"="Team"))
  names(gameInfoJoin)<-c(names(gameInfoJoin[1:8]),"Game Info")

  dkData<-gameInfoJoin[,c(1:6,9,7,8)]

  nba <- read.csv(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/",saveNameAdd,"cubistProjections.csv")) %>% dplyr::select(-X)
  nbaExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/stats.csv")) %>% dplyr::select(-X)
  nbaZeroExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/zeroStats.csv")) %>% dplyr::select(-X)
  nbaFRIDBind<-bind_rows(nbaExtra,nbaZeroExtra)

  dkData$Name<-gsub('[[:punct:]]',"",dkData$Name)
  nba$Player_Name<-gsub('[[:punct:]]',"",nba$Player_Name)
  dkData[dkData$Name == "Kelly Oubre Jr","Name"]<-"Kelly Oubre"
  dkData[dkData$Name == "Otto Porter Jr","Name"]<-"Otto Porter"
  dkData[dkData$Name == "Wendell Carter Jr","Name"]<-"Wendell Carter"
  dkData[dkData$Name == "Mo Harkless","Name"]<-"Maurice Harkless"

  #nbaNamesID<-nbaFRIDBind %>% dplyr::select(Player_Name,FantasyResultId)
  #nba<-dplyr::inner_join(nba,nbaNamesID,by="Player_Name") %>% unique()
  #nba$FantasyResultId<-as.character(nba$FantasyResultId)

  dkData<-dkData[dkData$Name %in% nba$Player_Name,]

  write.csv(dkData,file = paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKSalaries/DKSalariesOptim.csv"),row.names=FALSE)

  #read in raw DKSalariesOptim and cleanup
  data <-read_dk(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/DKSalaries/DKSalariesOptim.csv"))
  #read in projections file
  #nba <- read.csv(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/",saveNameAdd,"cubistProjections.csv")) %>% dplyr::select(-X)
  #replace "Player_Name" column name with "player" for joining data with projections
  #names(nba)<-gsub("Player_Name", replacement = "player", names(nba))
  #join DKSalariesOptim with projections
  #nbaExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/stats.csv")) %>% dplyr::select(-X)
  #nbaZeroExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/zeroStats.csv")) %>% dplyr::select(-X)
  #nbaFRIDBind<-bind_rows(nbaExtra,nbaZeroExtra)

  #nbaNamesID<-nbaFRIDBind %>% dplyr::select(Player_Name,FantasyResultId)
  #nba<-dplyr::inner_join(nba,nbaNamesID,by="Player_Name") %>% unique()
  #nba$FantasyResultId<-as.character(nba$FantasyResultId)

  #data$player<-gsub('[[:punct:]]',"",data$player)
  #nba$Player_Name<-gsub('[[:punct:]]',"",nba$Player_Name)


  data2 <- dplyr::inner_join(data,nba,by = c("player" = "Player_Name"))
  #insert projections into "fpts_proj" column for optimization
  data3 <- data2 %>% dplyr::mutate(fpts_proj=data2$projection)
  #data4 <- read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/zeroStats.csv"))
  #data4<-data4[,c("Player_Name","Team","OppTeam")]
  #data3<-data3 %>% select(-team,-opp_team)
  #data3<-inner_join(data3,data4,by=c("player"="Player_Name"))
  #data3<-data3[,c(1:3,36,37,4:35)]
  #names(data3)<-c(names(data3[1:3]),"team","opp_team",names(data3[6:37]))
  #data3$location<-data3$team
  #data3$opp_team<-substr(data3$opp_team, start = 1, stop = 3)


  model <- model_dk_nba(data3)
  lineups<- optimize_generic(na.What(data3),model,L = lineUps,min_salary = 49500)
  lineupsNorm<-foreach(a=1:lineUps,.combine = data.frame) %do% {
    lnorm<-coach::normalize_lineup(lineups[[a]],site = "draftkings",sport = "nba")
    suppressWarnings(row.names(lnorm)<- c("PG","SG","SF","PF","C","G","F","UTIL"))
    t(lnorm)
  }
  lineupsSplit<-foreach(b=1:lineUps-1) %do%{
  newnames<-names(lineupsNorm[grepl(pattern = paste0(".",b),x = names(lineupsNorm))])
    lineupsNorm[,c(newnames)]
  }
  lineupsSplit[[1]]<-lineupsNorm[,c(1:8)]
  lineupOutForm<-foreach(c=1:lineUps) %do% {
    sdSplit<-sd(lineupsSplit[[c]][10,1:8])
    actual<-sum(as.numeric(lineupsSplit[[c]][33,1:8]))
    a<-sum(as.numeric(lineupsSplit[[c]][8,1:8]))
    b<-sum(as.numeric(lineupsSplit[[c]][9,1:8]))
    d<-sum(as.numeric(lineupsSplit[[c]][11,1:8]))
    e<-sum(as.numeric(lineupsSplit[[c]][12,1:8]))
    f<-sum(as.numeric(lineupsSplit[[c]][13,1:8]))
    g<-sum(as.numeric(lineupsSplit[[c]][14,1:8]))
    h<-sum(as.numeric(lineupsSplit[[c]][15,1:8]))
    i<-sum(as.numeric(lineupsSplit[[c]][16,1:8]))
    j<-sum(as.numeric(lineupsSplit[[c]][17,1:8]))
    k<-sum(as.numeric(lineupsSplit[[c]][19,1:8]))
    l<-sum(as.numeric(lineupsSplit[[c]][20,1:8]))
    m<-sum(as.numeric(lineupsSplit[[c]][21,1:8]))
    n<-sum(as.numeric(lineupsSplit[[c]][22,1:8]))
    o<-sum(as.numeric(lineupsSplit[[c]][23,1:8]))
    p<-sum(as.numeric(lineupsSplit[[c]][24,1:8]))
    q<-sum(as.numeric(lineupsSplit[[c]][25,1:8]))
    r<-sum(as.numeric(lineupsSplit[[c]][26,1:8]))
    s<-sum(as.numeric(lineupsSplit[[c]][27,1:8]))
    t<-sum(as.numeric(lineupsSplit[[c]][30,1:8]))
    u<-sum(as.numeric(lineupsSplit[[c]][32,1:8]))
    v<-sum(as.numeric(lineupsSplit[[c]][35,1:8]))
    w<-sum(as.numeric(lineupsSplit[[c]][36,1:8]))
    lineupsSplit[[c]][2,1:8] %>% mutate(sd=sdSplit,Actual=actual,salary=a,fpts_avg=b,gppRank=d,projection=e,Score=f,GPPP=g,p_own=h,Ceiling=i,Floor=j,MinutesProj=k,FantasyPerMinute=l,UsageProj=m,PtVal=n,ProjPlusMinus=o,PaceD=p,OppPlusMinus=q,PointsPerTouch=r,Touches=s,Upside=t,StealsPlusBlocks=u,sixTimesPts=v,projValue=w) %>% setNames(c("PG","SG","SF","PF","C","G","F","UTIL","risk","ActualPoints",rownames(lineupsSplit[[1]])[c(8:9,11:17,19:27,30,32,35,36)]))
    }
  lineOuts<-bind_rows(lineupOutForm)

  lineOutsUniqueRisk<-lineOuts$risk %>% unique()
  uniqueRiskLines<-foreach(a=1:length(lineOutsUniqueRisk)) %do% {
    first<-which(lineOuts$risk==lineOutsUniqueRisk[a])
    first[1]
  } %>% unlist()
  lineOuts<-lineOuts[uniqueRiskLines,] %>% dplyr::arrange(desc(risk))
  #timeSt<-Sys.time()
  #hour(timeSt)>=13){
  #  timeSt<- Sys.time() - hours(12)
  #}
  #timeSt<-gsub("[[:space:]]", "_", timeSt)
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate),showWarnings = FALSE)
  dir.create(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/optimizedLineups"),showWarnings = FALSE)
  luNumber<-nrow(lineOuts)
  #write.csv(lineOuts,file=paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/optimizedLineups/",luNumber,"__",timeSt,".csv"),row.names = FALSE)
  #lineupsSplit

  #dkData for joining Name + ID to Optimized Lineups
  dkDataNames<-dkData[,c(4,2)]
  names(dkDataNames)<-c("ID","NamePlusID")
  dkDataNames<-sapply(dkDataNames,as.character) %>% data.frame()
  lineOutsPG<-dplyr::left_join(lineOuts,dkDataNames,by=c("PG"="ID")) %>% dplyr::select(NamePlusID)
  lineOutsSG<-dplyr::left_join(lineOuts,dkDataNames,by=c("SG"="ID")) %>% dplyr::select(NamePlusID)
  lineOutsSF<-dplyr::left_join(lineOuts,dkDataNames,by=c("SF"="ID")) %>% dplyr::select(NamePlusID)
  lineOutsPF<-dplyr::left_join(lineOuts,dkDataNames,by=c("PF"="ID")) %>% dplyr::select(NamePlusID)
  lineOutsC<-dplyr::left_join(lineOuts,dkDataNames,by=c("C"="ID")) %>% dplyr::select(NamePlusID)
  lineOutsG<-dplyr::left_join(lineOuts,dkDataNames,by=c("G"="ID")) %>% dplyr::select(NamePlusID)
  lineOutsF<-dplyr::left_join(lineOuts,dkDataNames,by=c("F"="ID")) %>% dplyr::select(NamePlusID)
  lineOutsUTIL<-dplyr::left_join(lineOuts,dkDataNames,by=c("UTIL"="ID")) %>% dplyr::select(NamePlusID)

  lineOutsNames<- dplyr::bind_cols(lineOutsPG,lineOutsSG,lineOutsSF,lineOutsPF,lineOutsC,lineOutsG,lineOutsF,lineOutsUTIL)
  names(lineOutsNames)<-c("PG","SG","SF","PF","C","G","F","UTIL")
  lineOutsNames<-data.frame(lineOutsNames,lineOuts[,9:32])
  lineOuts<-lineOutsNames

  optimNumber<-length(list.files(paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/optimizedLineups/"))) + 1
  #write.csv(lineOuts,file=paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/optimizedLineups/",luNumber,"__",timeSt,".csv"),row.names = FALSE)
  write.csv(lineOuts,file=paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/optimizedLineups/",optimNumber,".csv"),row.names = FALSE)

  if(sameDay!=TRUE){
    nbaResults(modelDate = modelDate)
    cashLine<-read.csv(file=paste0("~/NBA_Daily/results/",mYear,"/",modelDate,"/",gameSlate,"/results.csv")) %>% dplyr::select(-X) %>% .[1,paste0(gameSlate,".gppAverage")]
    winningLine<-read.csv(file=paste0("~/NBA_Daily/results/",mYear,"/",modelDate,"/",gameSlate,"/results.csv")) %>% dplyr::select(-X) %>% .[1,paste0(gameSlate,".best")]
    greenLineUps<-foreach(z=1:nrow(lineOuts),.combine = "+") %do% dplyr::if_else(lineOuts[z,"ActualPoints"]>=cashLine,true = 1,false = 0)
    cashPercent<-greenLineUps/nrow(lineOuts)*100
    write.csv(lineOuts,file=paste0("~/NBA_Daily/draftkingsData/",mYear,"/",modelDate,"/",gameSlate,"/optimizedLineups/",optimNumber,"__",round(cashPercent,1),"%_or_",greenLineUps,"_of_",nrow(lineOuts),"_lineups_Over_",round(cashLine,1),"_inTheMoney.csv"),row.names = FALSE)
    return(list("lineUps"=lineOuts,"actualPerformance"=c(paste0(round(cashPercent,1),"% or ",greenLineUps," of ",nrow(lineOuts)," lineups Over ",round(cashLine,1)," inTheMoney."))))
  }else{
      return(lineOuts)
  }
}
