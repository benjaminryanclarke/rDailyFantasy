#' nbaProjectionsFunction
#'
#' @param modelDate "10_17_2018"
#' @param gameSlate "Main"
#' @param nbaModel "nbaCubist"
#' @param nbaPre "preProcNBA"
#' @param labModel "2017FirstModel"
#' @param saveNameAdd "russelAndFlow"
#' @param cookies nbaCookies
#' @param refreshModel T/F refreshdataSet pulled for projections?
#' @param zeroOwnModel zeroOwnershipProjModel
#' @param labZeroModel fantasylabsXeroOwnModel
#' @param getCorr getActualCorrelelation post contests
#'
#' @return nbaProjections/also output as .csv in NBA_Daily/dfsProjections/
#' @export
#'
#' @examples nbaProjections(modelDate="10_17_2018",gameSlate="Main",nbaModel="nbaCubist",zeroOwnModel="nbaZeroOwngbmModel",nbaPre="preProcNBA",labModel="2017FirstModel",labZeroModel="zeroOwnVarImp",getCorr=FALSE,saveNameAdd="zeroRussellAndFlow",refreshModel=TRUE,cookies=nbaCookies)
nbaProjections<-function(modelDate="10_17_2018",gameSlate="Main",nbaModel="nbaCubist",zeroOwnModel="nbaZeroOwngbmnba",nbaPre="preProcNBA",labModel="2017FirstModel",labZeroModel="zeroOwnVarImp",getCorr=FALSE,saveNameAdd="zeroRussellAndFlow",refreshModel=TRUE,cookies=nbaCookies){

  hmDir<-"~/rDFS/rDailyFantasy"


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

  #projection Model Check
  modelCheck<-list(ls())
  if(nbaModel %in% modelCheck[[1]]==FALSE){
    nbaModel<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/algos/",nbaModel,".rda"))
  }

  if(zeroOwnModel %in% modelCheck[[1]]==FALSE){
    zeroOwnModel<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/algos/",zeroOwnModel,".rda"))
  }


  if(nbaPre %in% modelCheck[[1]]==FALSE){
    nbaPre<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/preProcessing/",nbaPre,".rda"))
  }

  #clearExistingLabsProjections if they exist
  nbaSystemsDf<-read.csv(file = "~/NBA_Daily/systems.csv") %>% dplyr::select(-X)
  modelId<-nbaSystemsDf[which(nbaSystemsDf$sysNames==labModel),][2][[1]]

  if(file.exists(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","ProjectionsClear.csv"))){
    projectionsClearingDf<-read.csv(file=paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","ProjectionsClear.csv")) %>% dplyr::select(-X)
    foreach(d=1:nrow(projectionsClearingDf)) %do% {
    system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",projectionsClearingDf[d,3],"/AvgPts/",modelDate,"?value=null"," > /dev/null"))
    system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",projectionsClearingDf[d,3],"/Exp_min/",modelDate,"?value=null"," > /dev/null"))
    }
  }

# if(zeroOwnPre %in% modelCheck[[1]]==FALSE){
  #  zeroOwnPre<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/preProcessing/",zeroOwnPre,".rda"))
  #}

  nbaProjVariablesIndex<-loadRData("~/NBA_Daily/nbaProjVariablesIndex.rda")
  nbaProjVariables<-loadRData("~/NBA_Daily/nbaProjVariables.rda")
  nbaProjVariables2<-loadRData("~/NBA_Daily/nbaProjVariables2.rda")
  nbaProjVariables3<-loadRData("~/NBA_Daily/nbaProjVariables3.rda")


  #getProjectionsData
  if(refreshModel==TRUE){
  nbaExtra<-rDailyFantasy::nbaModelSetup(modelDate=modelDate,gameSlate=gameSlate,labModel=labModel,systemsRefresh=0,sourceRefresh=0,cookie=nbaCookies)
  nbaZeroExtra<-rDailyFantasy::nbaZeroModelSetup(modelDate=modelDate,gameSlate=gameSlate,labModel=labZeroModel,systemsRefresh=0,sourceRefresh=0,cookie=nbaCookies)
  }else{
    nbaExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/stats.csv")) %>% dplyr::select(-X)
    nbaZeroExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/zeroStats.csv")) %>% dplyr::select(-X)
  }
  nba<-nbaExtra %>% dplyr::select(-FantasyResultId,-EventId,-EventTeamId)
  nba<-nba[which(nba$p_own!=0),]
  nbaZero<-nbaZeroExtra %>% dplyr::select(-FantasyResultId,-EventId,-EventTeamId) %>% rDailyFantasy::na.What()


  nbaTrans<-suppressWarnings(predict(nbaPre,nba))
  nbaProjections<-predict(nbaModel,nbaTrans)
  nba<-nba %>% dplyr::mutate(projection=nbaProjections,projValue=nba$Salary/nbaProjections)

  #nbaZeroTrans<-suppressWarnings(predict(zeroOwnPre,na.What(nbaZero)))
  nbaZeroProjections<-predict(zeroOwnModel,nbaZero)
  nbaZero<-nbaZero %>% dplyr::mutate(projection=nbaZeroProjections,projValue=nbaZero$Salary/nbaZeroProjections)

  nba<- dplyr::bind_rows(nba,nbaZero) %>% unique()

  nba<-nba[,nbaProjVariables3] %>% dplyr::mutate("sixTimesPts"=nba$Salary/1000*6)
  nba<-nba %>% dplyr::mutate("GPP%"=nba$projection/nba$sixTimesPts*100)
  nba<-nba %>% dplyr::arrange(desc(projection)) %>% dplyr::mutate(projRank=1:nrow(nba))
  nba<-nba %>% dplyr::arrange(desc(`GPP%`)) %>% dplyr::mutate(GPPRank=1:nrow(nba))
  nba<-nba %>% dplyr::arrange(desc(Ceiling)) %>% dplyr::mutate(ceilingRank=1:nrow(nba))
  nba<-nba %>% dplyr::mutate(gppAvg=apply(nba[,30:32],1,sum)/3)
  nba<-nba %>% dplyr::arrange(gppAvg) %>% dplyr::mutate(gppRank=1:nrow(nba)) %>% dplyr::select(-projRank,-GPPRank,-ceilingRank,-gppAvg)
  nbaOut<-nba[,c(30,1,2,5,29,4,10,11,7,15,16,14,13,12,17:28,3,6)] %>% unique()



  dir.create(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate),showWarnings = FALSE)
  dir.create(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate),showWarnings = FALSE)
  setwd(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate))
  write.csv(nbaOut,file=paste0(saveNameAdd,nbaModel$method,"Projections.csv"))

  nbaNamesProj<-nbaOut %>% dplyr::select(Player_Name,projection)
  nbaNamesFRId<-nbaExtra %>% dplyr::select(Player_Name,FantasyResultId)
  nbaLabsProjUploadDf<-dplyr::inner_join(nbaNamesProj,nbaNamesFRId,by="Player_Name") %>% unique()
  write.csv(nbaLabsProjUploadDf,file=paste0("ProjectionsClear.csv"))

  #nbaSystemsDf<-read.csv(file = "~/NBA_Daily/systems.csv") %>% dplyr::select(-X)
  #modelId<-nbaSystemsDf[which(nbaSystemsDf$sysNames==labModel),][2][[1]]

  nbaOut2<-nbaOut %>% dplyr::filter(p_own==0,`GPP%`>=65,Score>60)
  nbaOut3<-nbaOut2 %>% dplyr::mutate(expMin=20/nrow(nbaOut2)*10) %>% dplyr::select(Player_Name,expMin)
  nbaOut4<-dplyr::inner_join(nbaOut3,nbaLabsProjUploadDf,by="Player_Name")

  setwd("~/")

  #foreach(b=1:nrow(nbaLabsProjUploadDf)) %do% {
   # system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaLabsProjUploadDf[b,3],"/AvgPts/",modelDate,"?value=null"," > /dev/null"))
  #}

  foreach(b=1:nrow(nbaLabsProjUploadDf)) %do% {
    system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaLabsProjUploadDf[b,3],"/AvgPts/",modelDate,"?value=",round(nbaLabsProjUploadDf[b,2],2)," > /dev/null"))
  }

  foreach(b=1:nrow(nbaOut4)) %do% {
    system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaOut4[b,4],"/Exp_min/",modelDate,"?value=",1," > /dev/null"))
  }


  setwd(hmDir)

  if(getCorr==TRUE){
    return(list(model=nbaOut,r2=cor(nbaOut$ActualPoints,nbaOut$projection)))
  }else{
    return(nbaOut)
  }
}
