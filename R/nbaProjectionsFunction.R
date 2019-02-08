#' nbaProjectionsFunction
#'
#' @param modelDate "10_17_2018"
#' @param gameSlate "Main"
#' @param nbaModel "nbaCubist"
#' @param nbaPre "preProcNBA"
#' @param labModel "2017FirstModel"
#' @param cookies nbaCookies
#' @param refreshModel T/F refreshdataSet pulled for projections?
#' @param zeroOwnModel zeroOwnershipProjModel
#' @param labZeroModel fantasylabsXeroOwnModel
#' @param getCorr getActualCorrelelation post contests
#' @param plusMinusPre "plusMinusPreProc"
#' @param valuePre "valuePreProc"
#' @param plusMinusModel "plusMinusgbm"
#' @param valueModel "valuegbm"
#' @param sameDay T/F
#' @param algoX xgboost algo
#' @param treatPlanX xgboost treatplan
#' @param newVarsX xgboost new_vars
#' @param upload labs Proj Upload 'finalOptimal'
#'
#' @return nbaProjections/also output as .csv in NBA_Daily/dfsProjections/
#' @export
#'
#' @examples nbaProjections(modelDate="2_8_2019",gameSlate="Main",upload='finalOptimal',algoX="xgb.actualPoints.big.full.Final",treatPlanX="treatplanBig",newVarsX="new_varsBig",nbaModel="nbaCubist",zeroOwnModel="nbaZeroOwngbmnba",nbaPre="preProcNBA",labModel="Money",labZeroModel="Contrarians",plusMinusPre="plusMinusPreProc",valuePre="valuePreProc",plusMinusModel="plusMinusgbm",valueModel="valuegbm",sameDay=TRUE,getCorr=FALSE,refreshModel=TRUE,cookies=nbaCookies)
nbaProjections<-function(modelDate="2_8_2019",gameSlate="Main",upload='finalOptimal',algoX="xgb.actualPoints.big.full.Final",treatPlanX="treatplanBig",newVarsX="new_varsBig",nbaModel="nbaCubist",zeroOwnModel="nbaZeroOwngbmnba",nbaPre="preProcNBA",labModel="Money",labZeroModel="Contrarians",plusMinusPre="plusMinusPreProc",valuePre="valuePreProc",plusMinusModel="plusMinusgbm",valueModel="valuegbm",sameDay=TRUE,getCorr=FALSE,refreshModel=TRUE,cookies=nbaCookies){

  hmDir<-"~/rDFS/rDailyFantasy"

  suppressPackageStartupMessages(require(glmnet,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(brnn,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(Cubist,quietly = TRUE,warn.conflicts = F))
  suppressPackageStartupMessages(require(earth,quietly = TRUE,warn.conflicts = F))



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

  if(plusMinusPre %in% modelCheck[[1]]==FALSE){
    plusMinusPre<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/preProcessing/",plusMinusPre,".rda"))
  }

  if(plusMinusModel %in% modelCheck[[1]]==FALSE){
    plusMinusModel<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/algos/",plusMinusModel,".rda"))
  }

  if(valuePre %in% modelCheck[[1]]==FALSE){
    valuePre<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/preProcessing/",valuePre,".rda"))
    }

  if(valueModel %in% modelCheck[[1]]==FALSE){
    valueModel<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/algos/",valueModel,".rda"))
  }

  #clearExistingLabsProjections if they exist
  nbaSystemsDf<-read.csv(file = "~/NBA_Daily/systems.csv") %>% dplyr::select(-X)
  modelId<-nbaSystemsDf[which(nbaSystemsDf$sysNames==labModel),][2][[1]]
  zeroModelId<-nbaSystemsDf[which(nbaSystemsDf$sysNames==labZeroModel),][2][[1]]

#if(clearLabs==TRUE && file.exists(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","ProjectionsClear.csv"))){
  #if(file.exists(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","ProjectionsClear.csv"))){
    #projectionsClearingDf<-read.csv(file=paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","ProjectionsClear.csv")) %>% dplyr::select(-X)
      #suppressMessages(foreach(d=1:nrow(projectionsClearingDf))) %do% {
      #  system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",projectionsClearingDf[d,3],"/AvgPts/",modelDate,"?value=null"," > /dev/null"))
      #  system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",projectionsClearingDf[d,3],"/Exp_min/",modelDate,"?value=null"," > /dev/null"))
      #  system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",projectionsClearingDf[d,3],"/Exp/",modelDate,"?value=null"," > /dev/null"))
    #  }}



# if(zeroOwnPre %in% modelCheck[[1]]==FALSE){
  #  zeroOwnPre<-rDailyFantasy::loadRData(paste0("~/NBA_Daily/machineLearning/preProcessing/",zeroOwnPre,".rda"))
  #}

  nbaProjVariablesIndex<-loadRData("~/NBA_Daily/nbaProjVariablesIndex.rda")
  nbaProjVariables<-loadRData("~/NBA_Daily/nbaProjVariables.rda")
  nbaProjVariables2<-loadRData("~/NBA_Daily/nbaProjVariables2.rda")
  nbaProjVariables3<-loadRData("~/NBA_Daily/nbaProjVariables3.rda")




  #getProjectionsData
  if(refreshModel==TRUE){
    if(file.exists(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","nbacubistMoney_ZerogbmContrarians_Projections.csv"))){
      oldFRID<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/stats.csv")) %>% dplyr::select(Player_Name,FantasyResultId) %>% unique()
      oldProjections<-read.csv(file = paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","nbacubistMoney_ZerogbmContrarians_Projections.csv")) %>% dplyr::select(Player_Name,upload) %>% dplyr::inner_join(.,oldFRID,by="Player_Name")
      suppressMessages(foreach(d=1:nrow(oldProjections))) %do% {
        system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",oldProjections[d,3],"/AvgPts/",modelDate,"?value=null"," > /dev/null"))
        system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",oldProjections[d,3],"/Exp_min/",modelDate,"?value=null"," > /dev/null"))
        system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",oldProjections[d,3],"/Exp/",modelDate,"?value=null"," > /dev/null"))
      }
    }
  nbaExtra<-rDailyFantasy::nbaModelSetup(modelDate=modelDate,gameSlate=gameSlate,algoX=algoX,treatPlanX=treatPlanX,newVarsX=newVarsX,labModel=labModel,sameDay=sameDay,systemsRefresh=0,sourceRefresh=0,cookie=cookies)
  #nbaZeroExtra<-rDailyFantasy::nbaZeroModelSetup(modelDate=modelDate,gameSlate=gameSlate,labModel=labZeroModel,systemsRefresh=0,sourceRefresh=0,cookie=nbaCookies)
  }else{
    nbaExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/stats.csv")) %>% dplyr::select(-X)
    #nbaZeroExtra<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/zeroStats.csv")) %>% dplyr::select(-X)
  }
  nba<-nbaExtra %>% dplyr::select(-FantasyResultId,-EventId,-EventTeamId)
  pOwnCheck2<-foreach(i=1:nrow(nba),.combine = rbind) %do%  is.na(nba$p_own[i]) %>% data.frame()
  if(gameSlate!="Main" && length(which(pOwnCheck2$.==FALSE))==0){
    nbaZero<-nba[which(nba$ownershipProj<=3),]
    nba<-nba[which(nba$ownershipProj>3),]
    }
  if(gameSlate=="Early"){
    nbaZero<-nba
  }else{
  nbaZero<-nba[which(nba$p_own==0),]
  nba<-nba[which(nba$p_own!=0),]
  }
  #nbaZero<-nbaZeroExtra %>% dplyr::select(-FantasyResultId,-EventId,-EventTeamId) %>% rDailyFantasy::na.What()
  if(gameSlate!="Early"){
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=",zeroModelId)
  setwd("~/NBA_Daily/json/")
  y <- paste0("NBA",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookies,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl -L ",modelURL," ",pmCurlHandles)
  #download Model json to json folder
  system(curlAddress,ignore.stderr = TRUE)
  #read in json Model to parse
  nbaModelAllZero <- jsonlite::fromJSON(y, flatten = FALSE)

  suppressMessages(system(command = "cd ~/NBA_Daily/json/ && rm -rf *"))

  nbaModelAllZeroScores<-nbaModelAllZero$PlayerModels$Properties %>% dplyr::filter(SourceId==4) %>% dplyr::select("Player_Name","Score")
  names(nbaModelAllZeroScores)<-c("Player_Name","zeroScore")

  nbaZero<-dplyr::inner_join(nbaZero,nbaModelAllZeroScores,by="Player_Name")
  nbaZero$Score<-nbaZero$zeroScore
  nbaZero<-nbaZero %>% dplyr::select(-"zeroScore")
}
  nbaTrans<-suppressWarnings(predict(nbaPre,na.What(nba)))
  nbaProjection<-predict(nbaModel,na.What(nbaTrans))
  nba<-nba %>% dplyr::mutate(projection=nbaProjection,projValue=nba$Salary/nbaProjection)
if(gameSlate!="Early"){
  #nbaZeroTrans<-suppressWarnings(predict(zeroOwnPre,na.What(nbaZero)))
  nbaZeroProjections<-predict(zeroOwnModel,na.What(nbaZero))
  nbaZero<-nbaZero %>% dplyr::mutate(projection=nbaZeroProjections,projValue=nbaZero$Salary/nbaZeroProjections)
}
  nbaOutPlusPre<-predict(plusMinusPre,na.What(nbaExtra))
 # nbaOutValuePre<-predict(valuePre,na.What(nbaExtra))
  plusMinusProj<-predict(plusMinusModel,na.What(nbaOutPlusPre))
 # valueProj<-predict(valueModel,na.What(nbaOutValuePre))

  nbaExtra<-nbaExtra %>% dplyr::mutate("plusMinusProj"=plusMinusProj)#,"valueProj"=valueProj)


  #if(sameDay!=TRUE){
   # actualPlusMinus<-nbaExtra$ActualPoints-(nbaExtra$Salary/1000*4)
    #actualValue<-na.What(nbaExtra$Salary/nbaExtra$ActualPoints)
    #nbaExtra<- nbaExtra %>% dplyr::mutate("actualPlusMinus"=actualPlusMinus,"ActualValue"=actualValue)
  #}

  if(gameSlate=="Early"){
    nba<-nba
  }else{
  nba<- dplyr::bind_rows(nba,nbaZero) %>% unique()
  }
  nba<-nba[,nbaProjVariables3] %>% dplyr::mutate("sixTimesPts"=nba$Salary/1000*6)
  nba<-nba %>% dplyr::mutate("GPP%"=nba$projection/nba$sixTimesPts*100)
  nba<-nba %>% dplyr::arrange(desc(projection)) %>% dplyr::mutate(projRank=1:nrow(nba))
  nba<-nba %>% dplyr::arrange(desc(`GPP%`)) %>% dplyr::mutate(GPPRank=1:nrow(nba))
  nba<-nba %>% dplyr::arrange(desc(Ceiling)) %>% dplyr::mutate(ceilingRank=1:nrow(nba))
  nba<-nba %>% dplyr::mutate(gppAvg=apply(nba[,30:32],1,sum)/3)
  nba<-nba %>% dplyr::arrange(gppAvg) %>% dplyr::mutate(gppRank=1:nrow(nba)) %>% dplyr::select(-projRank,-GPPRank,-ceilingRank,-gppAvg)
  nbaOut<-nba[,c(30,1,2,5,29,4,10,11,7,15,16,14,13,12,17:28,3,6)] %>% unique()
  #nbaOut<-inner_join(nbaOut,nbaExtra[,c("Player_Name","plusMinusMatchup","plusMinusProj","valueProj")],by="Player_Name")
  nbaOut<-inner_join(nbaOut,nbaExtra[,c("Player_Name","plusMinusMatchup","plusMinusProj","xgbProj","gppGradeProj","ownershipProj","inLineup")],by="Player_Name")
  if(sameDay!=TRUE){
    nbaOut<-inner_join(nbaOut,nbaExtra[,c("Player_Name","actualPlusMinus","ActualValue","GppGrade","Average")],by="Player_Name")
    nbaOut<-nbaOut[,c(1:3,31,24,27,35,29,30,35,32,37,33,38,4:23,25,26,28,34)] %>% unique()
  }else{
      nbaOut<-nbaOut[,c(1:3,31,27,29,30,32,33,4:26,28,34)] %>% unique()
  }
  #nbaOut$`GPP%`<-(((nbaOut$projection+nbaOut$xgbProj)/2)/nba$sixTimesPts*100)

  ####add new projections here
  load("~/NBA_Daily/machineLearning/algos/glmnetnbafull2_3.rda")
  load("~/NBA_Daily/machineLearning/vTreat/new_varsActualPoints.rda")
  load("~/NBA_Daily/machineLearning/vTreat/treatPlanActualPoints.rda")
  load("~/NBA_Daily/machineLearning/algos/glmnetnbaFULLActualPoints.rda")
  load("~/NBA_Daily/machineLearning/algos/bagEarthnbaFULLActualPoints.rda")
  load("~/NBA_Daily/machineLearning/algos/nbaFULLActualPointsLmFinal.rda")
  load("~/NBA_Daily/machineLearning/algos/cubistnbaFULLActualPoints.rda")
  load("~/NBA_Daily/machineLearning/algos/xgbnbaFULLActualPoints.final1.rda")
  load("~/NBA_Daily/machineLearning/algos/avNNetnbaFULLActualPoints.rda")
  load("~/NBA_Daily/machineLearning/algos/brnnnbaFULLActualPoints.rda")
  load("~/NBA_Daily/machineLearning/vTreat/new_varsnbaFULLActualPoints.rda")
  load("~/NBA_Daily/machineLearning/vTreat/treatplannbaFULLActualPoints.rda")

  secondAP<-nbaExtra %>% dplyr::mutate("FantasyPerMinutePoints"=nbaExtra$MinutesProj * nbaExtra$FantasyPerMinute,"FantasyPerMinutePoints_Mth"=nbaExtra$MinutesProj * nbaExtra$FantasyPerMinute_Mth)
  secondAP$FantasyPerMinute_Mth[which(secondAP$FantasyPerMinute_Mth == 0)]<-secondAP$FantasyPerMinute[which(secondAP$FantasyPerMinute_Mth == 0)]
  secondAP <- secondAP %>% dplyr::mutate("FantasyPerMinutePointsPlusMinus"=round(secondAP$FantasyPerMinutePoints - ((secondAP$Salary/1000)*4),3))
  secondAP <- secondAP %>% dplyr::mutate("FantasyPerMinutePointsPlusMinus_Mth"=round(secondAP$FantasyPerMinutePoints_Mth - ((secondAP$Salary/1000)*4),3))
  if(sameDay!=TRUE){
    secondAP <- na.What(secondAP) %>% dplyr::mutate("ActualFantasyPerMinutePointsPlusMinus"=secondAP$ActualPoints - secondAP$FantasyPerMinutePoints)
    AFPMPPM <- secondAP$ActualPoints - secondAP$FantasyPerMinutePoints_Mth %>% round(2)
    secondAP <- secondAP %>% dplyr::mutate("ActualFantasyPerMinutePointsPlusMinus_Mth"=AFPMPPM)
    secondAP <- secondAP %>% dplyr::mutate("ActualPlusMinusGPP"=secondAP$ActualPoints - ((secondAP$Salary/1000)*7))
    secondAP <- secondAP %>% dplyr::mutate("ActualPlusMinus"=secondAP$ActualPoints - ((secondAP$Salary/1000)*4))
  }
  secondAPTreat <- suppressWarnings(vtreat::prepare(treatplannbaFULLActualPoints, secondAP, varRestriction = new_varsnbaFULLActualPoints) %>% as.matrix())
  secondAPbrnn<-predict(brnnnbaFULLActualPoints,na.What(secondAP)) * 41.64
  secondAPxgb<-predict(xgbnbaFULLActualPoints.final1,na.What(secondAPTreat)) * 0.39
  secondAPlm<-predict(nbaFULLActualPointsLmFinal,na.What(secondAP)) * .04
  secondAPcubist<-predict(cubistnbaFULLActualPoints,na.What(secondAPTreat)) * .04
  secondAPbagEarth<-predict(bagEarthnbaFULLActualPoints,na.What(secondAPTreat)) * .04
  secondAPglmnet<-predict(glmnetnbaFULLActualPoints,na.What(secondAPTreat)) * .04
  secondAPavNNet<-predict(avNNetnbaFULLActualPoints,na.What(secondAPTreat)) * .04
  secondAPWeighted<-(secondAPbrnn+secondAPxgb+secondAPlm+secondAPcubist+secondAPbagEarth+secondAPglmnet+secondAPavNNet) * 0.02367985

  secondAPbrnn2<-predict(brnnnbaFULLActualPoints,na.What(secondAP))
  secondAPxgb2<-predict(xgbnbaFULLActualPoints.final1,na.What(secondAPTreat))
  #secondAPlm2<-predict(nbaFULLActualPointsLmFinal,na.What(secondAP))
  secondAPcubist2<-predict(cubistnbaFULLActualPoints,na.What(secondAPTreat))
  secondAPbagEarth2<-predict(bagEarthnbaFULLActualPoints,na.What(secondAPTreat))
  secondAPglmnet2<-predict(glmnetnbaFULLActualPoints,na.What(secondAPTreat))
  secondAPavNNet2<-predict(avNNetnbaFULLActualPoints,na.What(secondAPTreat))

  secondProjsAll<-data.frame(secondAPbrnn2,secondAPxgb2,secondAPcubist2,secondAPbagEarth2,secondAPglmnet2,secondAPavNNet2)
  names(secondProjsAll)<-c(names(secondProjsAll[1:4]),"secondAPglmnet2","secondAPavNNet2")


  nbaExtra$p_own <- as.factor(nbaExtra$p_own)
  nbaExtra$Days_Between_games <- as.factor(nbaExtra$Days_Between_games)
  nbaExtra$Depth <- as.factor(nbaExtra$Depth)
  nbaExtra$ProTrends_DK <- as.factor(nbaExtra$ProTrends_DK)
  nbaExtra$ProTrends_FD <- as.factor(nbaExtra$ProTrends_FD)
  nbaExtra$Salary <- as.factor(nbaExtra$Salary)
  nbaExtra$Salary_DK <- as.factor(nbaExtra$Salary_DK)
  nbaExtra$Salary_FD <- as.factor(nbaExtra$Salary_FD)
  nbaExtra$Trend <- as.factor(nbaExtra$Trend)
  nbaExtra$PlayerId <- as.factor(nbaExtra$PlayerId)
  glmTreat <- suppressWarnings(vtreat::prepare(treatplanActualPoints, nbaExtra, varRestriction = new_varsActualPoints) %>% as.matrix())
  GLMNetPreds<-predict(glmnetnbafull2_3,na.What(glmTreat))
  GLMNetPreds<-sapply(GLMNetPreds,as.numeric)


  nbaPredsDf<-nbaExtra %>% dplyr::select(Player_Name) %>% dplyr::mutate("glmProj"=GLMNetPreds,"optimalTwo"=as.numeric(secondAPWeighted))
  nbaPredsDf<-dplyr::bind_cols(nbaPredsDf,secondProjsAll)

  nbaOut<- dplyr::inner_join(nbaOut,nbaPredsDf,by="Player_Name")


  nbaOut<-nbaOut %>% dplyr::mutate(optimal=rDailyFantasy::weightedPredictions(weights=c(0.5,0.496,0.02),predictions = list(nbaOut$projection,nbaOut$xgbProj,nbaOut$glmProj)))
  weightedCombo<-nbaOut$secondAPbrnn2*0.18181818+nbaOut$secondAPxgb2*0.3030303+nbaOut$secondAPcubist2*0.0+nbaOut$secondAPbagEarth2*0.06060606+nbaOut$secondAPglmnet2*0.06060606+nbaOut$secondAPavNNet2*0.0+nbaOut$projection*0.12121212+nbaOut$xgbProj*0.24242424+nbaOut$glmProj*0.03030303
  nbaOut<-nbaOut %>% dplyr::mutate("optimalThree"=weightedCombo)
  finalOpt<-rDailyFantasy::weightedPredictions(weights = c(0.4761905,0.4761905,0.04761905),predictions = list(nbaOut$optimal,nbaOut$optimalTwo,nbaOut$optimalThree))
  nbaOut<-nbaOut %>% dplyr::mutate("finalOptimal"=finalOpt)


  nbaOut$sevenTimesPoints<-(nbaOut$Salary)/1000*7
  nbaOut$`GPP%`<-nbaOut[,upload]/(nbaOut$sevenTimesPoints)*100

  #nbaOut$`GPP%`<-(nbaOut$optimal/nba$sixTimesPts*100)


  dir.create(paste0("~/NBA_Daily/dfsProjections/",mYear),showWarnings = FALSE)
  dir.create(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate),showWarnings = FALSE)
  dir.create(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate),showWarnings = FALSE)
  setwd(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate))
  #write.csv(nbaOut,file=paste0("nba",nbaModel$method,labModel,"_Zero",zeroOwnModel$method,labZeroModel,"_Projections.csv"))

  #if(sendToLabs=="xgb"){
  #  nbaNamesProj<-nbaOut %>% dplyr::select(Player_Name,xgbProj)
  #  }else{
  #nbaNamesProj<-nbaOut %>% dplyr::select(Player_Name,projection)
  #  }
  # nbaNamesProj<- nbaOut %>% dplyr::select(Player_Name,optimal)
  # nbaNamesFRId<-nbaExtra %>% dplyr::select(Player_Name,FantasyResultId)
  # nbaLabsProjUploadDf<-dplyr::inner_join(nbaNamesProj,nbaNamesFRId,by="Player_Name") %>% unique()


  #write.csv(nbaLabsProjUploadDf,file=paste0("ProjectionsClear.csv"))

  #nbaSystemsDf<-read.csv(file = "~/NBA_Daily/systems.csv") %>% dplyr::select(-X)
  #modelId<-nbaSystemsDf[which(nbaSystemsDf$sysNames==labModel),][2][[1]]

  # nbaOut2<-nbaOut %>% dplyr::filter(p_own==0,`GPP%`>=65,Score>60)
  # nbaOut3<-nbaOut2 %>% dplyr::mutate(expMin=20/nrow(nbaOut2)*10) %>% dplyr::select(Player_Name,expMin)
  # nbaOut4<-dplyr::inner_join(nbaOut3,nbaLabsProjUploadDf,by="Player_Name")

  # nbaOut5<-nbaOut %>% dplyr::filter(inLineup==0)
  # nbaOut6<-nbaOut5 %>% dplyr::select(Player_Name,inLineup)
  # nbaOut7<-dplyr::inner_join(nbaOut6,nbaLabsProjUploadDf,by="Player_Name")

  #foreach(b=1:nrow(nbaLabsProjUploadDf)) %do% {
   # system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaLabsProjUploadDf[b,3],"/AvgPts/",modelDate,"?value=null"," > /dev/null"))
  #}

  #if(!is.null(sendToLabs)){
  # foreach(b=1:nrow(nbaLabsProjUploadDf)) %do% {
    # system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaLabsProjUploadDf[b,3],"/AvgPts/",modelDate,"?value=",round(nbaLabsProjUploadDf[b,2],2)," > /dev/null"))
  # }



  # foreach(b=1:nrow(nbaOut4)) %do% {
    # system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaOut4[b,4],"/Exp_min/",modelDate,"?value=",1," > /dev/null"))
  # }

  #foreach(b=1:nrow(nbaOut7)) %do% {
  #  system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaOut7[b,4],"/Exp/",modelDate,"?value=",0," > /dev/null"))
  #}


fpMinProj<-nbaOut$FantasyPerMinute*nbaOut$MinutesProj
fpMinProjOptimal<-nbaOut[,upload]-(nbaOut$FantasyPerMinute*nbaOut$MinutesProj)

nbaOut<-nbaOut %>% dplyr::mutate("fpMinProj"=fpMinProj,"fpMinProjOptimal"=fpMinProjOptimal)

load("~/NBA_Daily/machineLearning/vTreat/new_varsActualPointsFP.rda")
load("~/NBA_Daily/machineLearning/vTreat/treatplanActualPointsFP.rda")
load("~/NBA_Daily/machineLearning/algos/xgbFP.rda")

nbaFinalAdd<-nbaExtra %>% dplyr::select(Player_Name,Team,LeveragePct,FantasyPerMinute_Mth,CeilingPct,PtPerDPct,Depth)
projFP<-inner_join(nbaOut,nbaFinalAdd,by="Player_Name")
projFP$Depth<-as.numeric(projFP$Depth)
projFP<-na.What(projFP)
projFP$Depth<-as.factor(projFP$Depth)

fpmMthDiff<-projFP$FantasyPerMinute_Mth-projFP$FantasyPerMinute
pptPoints<-projFP$PointsPerTouch*projFP$Touches
ceilingAddedPtPerD<-projFP$CeilingPct-projFP$PtPerDPct
projFP<-projFP %>% dplyr::mutate("fpmMthDiff"=fpmMthDiff,"pptPoints"=pptPoints,"ceilingAddedPtPerD"=ceilingAddedPtPerD)
projFP$inLineup<-as.factor(projFP$inLineup)
projFP$Position<-as.factor(projFP$Position)
#projFP$Depth<-as.factor(projFP$Depth)

projFPTreat <- suppressWarnings(vtreat::prepare(treatplanActualPointsFP, projFP, varRestriction = new_varsActualPointsFP) %>% as.matrix())
projFPPred<-predict(xgbFP,na.What(projFPTreat))
projFP<-projFP %>% dplyr::select(Player_Name,Team,LeveragePct,fpmMthDiff,pptPoints,ceilingAddedPtPerD) %>% dplyr::mutate("finalProjections"=projFPPred)


nbaOut<- dplyr::inner_join(nbaOut,projFP,by="Player_Name")



###
###
###
###
#projplusminus

reReadStats<-read.csv(file = paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/Main/stats.csv"))

reReadStats<-reReadStats %>% dplyr::select(-X)
reReadStats$PositionType[is.na(reReadStats$PositionType)]<-"No"
reReadStats$ActualPoints<-na.What(reReadStats$ActualPoints)
reReadStats$ProjectedOwnership_DK[is.na(reReadStats$ProjectedOwnership_DK)]<-"0-1"
reReadStats$ProjectedOwnership_FD[is.na(reReadStats$ProjectedOwnership_FD)]<-"0-1"
reReadStats$Depth[is.na(reReadStats$Depth)]<-0
reReadStats$ProTrends_DK[is.na(reReadStats$ProTrends_DK)]<-0
reReadStats$ProTrends_FD[is.na(reReadStats$ProTrends_FD)]<-0
reReadStats$Season_Plus_Minus<-na.What(reReadStats$Season_Plus_Minus)
reReadStats$Season_PPG<-na.What(reReadStats$Season_PPG)
#reReadStats<-reReadStats[-which(is.na(reReadStats$Salary_FD)),]
reReadStats$FantasyPerMinutePct_Mth<-na.What(reReadStats$FantasyPerMinutePct_Mth)
reReadStats$Month_PPG<-na.What(reReadStats$Month_PPG)
reReadStats$Month_Count<-na.What(reReadStats$Month_Count)
reReadStats$Month_X0<-na.What(reReadStats$Month_X0)
reReadStats$Month_X1<-na.What(reReadStats$Month_X1)
reReadStats$Month_X2<-na.What(reReadStats$Month_X2)
#freqCols<-grep(pattern = "Frequency",x = names(reReadStats),fixed = TRUE)
#foreach(a=1:length(freqCols)) %do% {
#reReadStats[,freqCols[a]]<-na.What(reReadStats[,freqCols[a]],what = mean(reReadStats[,freqCols[a]]))
#}
#sapply(reReadStats[,freqCols], mean)
reReadStats$Position<-as.factor(reReadStats$Position)
reReadStats$PositionType<-as.factor(reReadStats$PositionType)
reReadStats$ProjectedOwnership_DK<-as.factor(reReadStats$ProjectedOwnership_DK)
reReadStats$ProjectedOwnership_FD<-as.factor(reReadStats$ProjectedOwnership_FD)
reReadStats$Depth<-as.factor(reReadStats$Depth)
reReadStats$Days_Between_games<-as.factor(reReadStats$Days_Between_games)
reReadStats$ProTrends_DK<-as.factor(reReadStats$ProTrends_DK)
reReadStats$ProTrends_FD<-as.factor(reReadStats$ProTrends_FD)




reReadStats<-reReadStats %>% dplyr::mutate("FantasyPerMinutePoints"=reReadStats$MinutesProj * reReadStats$FantasyPerMinute,"FantasyPerMinutePoints_Mth"=reReadStats$MinutesProj * reReadStats$FantasyPerMinute_Mth)
reReadStats$FantasyPerMinute_Mth[which(reReadStats$FantasyPerMinute_Mth == 0)]<-reReadStats$FantasyPerMinute[which(reReadStats$FantasyPerMinute_Mth == 0)]
reReadStats <- reReadStats %>% dplyr::mutate("FantasyPerMinutePointsPlusMinus"=round(reReadStats$FantasyPerMinutePoints - ((reReadStats$Salary/1000)*4),3))
reReadStats <- reReadStats %>% dplyr::mutate("FantasyPerMinutePointsPlusMinus_Mth"=round(reReadStats$FantasyPerMinutePoints_Mth - ((reReadStats$Salary/1000)*4),3))
if(sameDay!=TRUE){
reReadStats <- reReadStats %>% dplyr::mutate("ActualFantasyPerMinutePointsPlusMinus"=reReadStats$ActualPoints - reReadStats$FantasyPerMinutePoints)
AFPMPPM <- reReadStats$ActualPoints - reReadStats$FantasyPerMinutePoints_Mth %>% round(2)
reReadStats <- reReadStats %>% dplyr::mutate("ActualFantasyPerMinutePointsPlusMinus_Mth"=AFPMPPM)
reReadStats <- reReadStats %>% dplyr::mutate("ActualPlusMinusGPP"=reReadStats$ActualPoints - ((reReadStats$Salary/1000)*7))
reReadStats <- reReadStats %>% dplyr::mutate("ActualPlusMinus"=reReadStats$ActualPoints - ((reReadStats$Salary/1000)*4))
}

load(file="~/NBA_Daily/machineLearning/preProcessing/nba2018DataPreProc.rda")
load(file="~/NBA_Daily/machineLearning/algos/bst.rda")
load(file="~/NBA_Daily/machineLearning/algos/bst2.rda")

reReadStatsPreProcessed<-predict(nba2018DataPreProc,reReadStats)

testData<-reReadStatsPreProcessed[,bst$feature_names]

testData.mat<-data.matrix(testData)
plusMinusPredds<-predict(bst2,testData.mat)

# pmDF<-data.frame("Player_Name"=reReadStats$Player_Name,"pm"=plusMinusPredds,"basePts"=reReadStats$ImpPts)
 pmDF<-data.frame("Player_Name"=reReadStats$Player_Name,"pm"=plusMinusPredds,"basePts"=(reReadStats$Salary/1000)*4)

nbaOut<-dplyr::inner_join(nbaOut,pmDF,by="Player_Name")
nbaOut<-nbaOut %>% dplyr::mutate("pmProj"=nbaOut$basePts+nbaOut$pm)












nbaNamesProj<- nbaOut %>% dplyr::select(Player_Name,upload)
nbaNamesFRId<-nbaExtra %>% dplyr::select(Player_Name,FantasyResultId)
nbaLabsProjUploadDf<-dplyr::inner_join(nbaNamesProj,nbaNamesFRId,by="Player_Name") %>% unique()


#write.csv(nbaLabsProjUploadDf,file=paste0("ProjectionsClear.csv"))

#nbaSystemsDf<-read.csv(file = "~/NBA_Daily/systems.csv") %>% dplyr::select(-X)
#modelId<-nbaSystemsDf[which(nbaSystemsDf$sysNames==labModel),][2][[1]]

nbaOut2<-nbaOut %>% dplyr::filter(p_own==0,`GPP%`>=65,Score>60)
nbaOut3<-nbaOut2 %>% dplyr::mutate(expMin=20/nrow(nbaOut2)*10) %>% dplyr::select(Player_Name,expMin)
nbaOut4<-dplyr::inner_join(nbaOut3,nbaLabsProjUploadDf,by="Player_Name")

nbaOut5<-nbaOut %>% dplyr::filter(inLineup==0)
nbaOut6<-nbaOut5 %>% dplyr::select(Player_Name,inLineup)
nbaOut7<-dplyr::inner_join(nbaOut6,nbaLabsProjUploadDf,by="Player_Name")

#foreach(b=1:nrow(nbaLabsProjUploadDf)) %do% {
# system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaLabsProjUploadDf[b,3],"/AvgPts/",modelDate,"?value=null"," > /dev/null"))
#}

#if(!is.null(sendToLabs)){
foreach(b=1:nrow(nbaLabsProjUploadDf)) %do% {
  system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaLabsProjUploadDf[b,3],"/AvgPts/",modelDate,"?value=",round(nbaLabsProjUploadDf[b,2],2)," > /dev/null"))
}



foreach(b=1:nrow(nbaOut4)) %do% {
  system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaOut4[b,4],"/Exp_min/",modelDate,"?value=",1," > /dev/null"))
}

foreach(b=1:nrow(nbaOut7)) %do% {
  system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",nbaOut7[b,4],"/Exp/",modelDate,"?value=",0," > /dev/null"))
}






nbaOut<-nbaOut %>% dplyr::mutate(change=0)

if(file.exists(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","nbacubistMoney_ZerogbmContrarians_Projections.csv"))){
  oldFRID<-read.csv(paste0("~/NBA_Daily/dataSets/",mYear,"/",modelDate,"/",gameSlate,"/stats.csv")) %>% dplyr::select(Player_Name,FantasyResultId) %>% unique()
  oldProjections<-read.csv(file = paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate,"/","nbacubistMoney_ZerogbmContrarians_Projections.csv")) %>% dplyr::select(Player_Name,upload) %>% dplyr::inner_join(.,oldFRID,by="Player_Name")
  names(oldProjections)<-c("Player_Name","oldFinalOptimal","FantasyResultId")
  nbaOut<-dplyr::inner_join(nbaOut,oldProjections,by="Player_Name") %>% dplyr::select(-FantasyResultId)
  projDiff<-round(nbaOut$finalOptimal,3)-round(nbaOut$oldFinalOptimal,3)
  nbaOut<-nbaOut %>% dplyr::mutate(change=projDiff)
  }


  setwd(hmDir)

if(sameDay==TRUE){
  outAdd<-secondAP %>% dplyr::select(Player_Name,FantasyPerMinutePoints_Mth,FantasyPerMinutePointsPlusMinus,FantasyPerMinutePointsPlusMinus_Mth)
  nbaOut<- dplyr::inner_join(nbaOut,outAdd,by = "Player_Name")
  nbaOut<-nbaOut %>% dplyr::select(-Floor,-PtVal,-ProjPlusMinus,-PointsPerTouch,-Touches,-Pts,-Spread,-PositionType,-PlayerId,-sixTimesPts)
  nbaOut<-nbaOut %>% dplyr::select(inLineup,gppRank,Player_Name,Salary,Position,Team,finalOptimal,optimalThree,optimalTwo,finalProjections,optimal,xgbProj,projection,glmProj,`GPP%`,ownershipProj,p_own,fpMinProjOptimal,fpMinProj,FantasyPerMinutePoints_Mth,FantasyPerMinutePointsPlusMinus,FantasyPerMinutePointsPlusMinus_Mth,FantasyPerMinute,MinutesProj,UsageProj,plusMinusProj,plusMinusMatchup,gppGradeProj,OppPlusMinus,PaceD,Score,LeveragePct,fpmMthDiff,pptPoints,ceilingAddedPtPerD,pm,pmProj,change)
 }else{
    outAdd<-secondAP %>% dplyr::select(Player_Name,FantasyPerMinutePoints_Mth,FantasyPerMinutePointsPlusMinus,FantasyPerMinutePointsPlusMinus_Mth,ActualPlusMinusGPP)
    nbaOut<- dplyr::inner_join(nbaOut,outAdd,by = "Player_Name")
    nbaOut<-nbaOut %>% dplyr::select(-Floor,-PtVal,-ProjPlusMinus,-PointsPerTouch,-Touches,-Pts,-Spread,-PositionType,-PlayerId,-sixTimesPts)
    nbaOut<-nbaOut %>% dplyr::select(inLineup,gppRank,Player_Name,Salary,Position,Team,finalOptimal,optimalThree,optimalTwo,finalProjections,optimal,xgbProj,projection,glmProj,`GPP%`,ownershipProj,p_own,fpMinProjOptimal,fpMinProj,FantasyPerMinutePoints_Mth,FantasyPerMinutePointsPlusMinus,FantasyPerMinutePointsPlusMinus_Mth,FantasyPerMinute,MinutesProj,UsageProj,plusMinusProj,plusMinusMatchup,gppGradeProj,OppPlusMinus,PaceD,Score,LeveragePct,fpmMthDiff,pptPoints,ceilingAddedPtPerD,pm,pmProj,ActualPoints,actualPlusMinus,ActualPlusMinusGPP,GppGrade,Average,change)
  }
  setwd(paste0("~/NBA_Daily/dfsProjections/",mYear,"/",modelDate,"/",gameSlate))

  nbaOut$`GPP%`<-(nbaOut[,upload]/((nbaOut$Salary/1000)*7)*100)

  write.csv(nbaOut,file=paste0("nba",nbaModel$method,labModel,"_Zero",zeroOwnModel$method,labZeroModel,"_Projections.csv"))
  setwd(hmDir)

  if(getCorr==TRUE){
    return(suppressMessages(list(model=nbaOut,c(cubist_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$projection),cubist_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$projection)),c(xgb_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$xgbProj),xgb_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$xgbProj)),c(glm_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$glmProj),glm_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$glmProj)),c(optimal_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$projection),optimal_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$optimal)),c(optimalTwo_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$optimalTwo),optimalTwo_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$optimalTwo)),c(optimalThree_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$optimalThre),optimalThree_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$optimalThre)),c(finalOptimal_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$finalOptimal),finalOptimal_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$finalOptimal)),c(pm_r2=cor(na.What(nbaOut$actualPlusMinus),nbaOut$pm),pm_RMSE=caret::RMSE(na.What(nbaOut$actualPlusMinus),nbaOut$pm)),c(pmPts_r2=cor(na.What(nbaOut$ActualPoints),nbaOut$pmProj),pmProj_RMSE=caret::RMSE(na.What(nbaOut$ActualPoints),nbaOut$pmProj)))))
    }else{
    return(suppressMessages(nbaOut))
  }
}
