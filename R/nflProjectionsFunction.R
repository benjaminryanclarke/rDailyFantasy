#' nfl Player Position Projections Function
#'
#' @param gameSlate .."Main","Turbo","Afternoon","Allday","Night"...etc
#' @param qbModel the model used to mak projections. Saved in ~/MLB_Daily/machineLearning/hitters/
#' @param labModel name of the fantasyLabs model used in the projections model training...default "mainBae"..6_11_2018
#' @param modelWeek mW
#' @param modelYear mY
#' @param rbModel algoName
#' @param wrModel algoName
#' @param pffData TRUE?FALSE
#' @param teModel algoName
#' @param dstModel algoName
#' @param qbVars variableNamesFile
#' @param rbVars variableNamesFile
#' @param wrVars variableNamesFile
#' @param teVars variableNamesFile
#' @param dstVars variableNamesFile
#' @param qbPre preProcessingFile
#' @param rbPre preProcessingFile
#' @param wrPre preProcessingFile
#' @param tePre preProcessingFile
#' @param dstPre preProcessingFile
#' @param saveNameAdd addtoOriginalName
#'
#' @return saves csv file with projections to ~/NFL_Daily/dfsProjections/QB/WR/TE/RB/DST/..../ and returns the data.frames in the r console in a list
#' @export
#'
#' @examples nflProjection(modelWeek=6,modelYear=2018,gameSlate="Main",qbModel="qbBig_ridgeModel_tuneUniq2",rbModel="rbBig_gbmModel_tuneUniq2",wrModel="wrBig_gbmModel_tuneUniq2",teModel="teBig_treebagModel_tuneUniq2",dstModel="dstBig_icrModel_tuneUniq2",pffModel="pffCombined_rangerModel_trimmed",qbVars="qbVariablesBig",rbVars="rbVariablesBig",wrVars="wrVariablesBig",teVars="teVariablesBig",dstVars="dstVariablesBig",qbPre="qbUPre",rbPre="rbUPre",wrPre="wrUPre",tePre="teUPre",dstPre="dstUPre",pffPre="preProcPFFDataTrimmed",labModel="2018",saveNameAdd="theMoneyMaker",pffData=TRUE)
nflProjection<-function(modelWeek=6,modelYear=2018,gameSlate="Main",qbModel="qbBig_ridgeModel_tuneUniq2",rbModel="rbBig_gbmModel_tuneUniq2",wrModel="wrBig_gbmModel_tuneUniq2",teModel="teBig_treebagModel_tuneUniq2",dstModel="dstBig_icrModel_tuneUniq2",pffModel="pffCombined_rangerModel_trimmed",qbVars="qbVariablesBig",rbVars="rbVariablesBig",wrVars="wrVariablesBig",teVars="teVariablesBig",dstVars="dstVariablesBig",qbPre="qbUPre",rbPre="rbUPre",wrPre="wrUPre",tePre="teUPre",dstPre="dstUPre",pffPre="preProcPFFDataTrimmed",labModel="2018",saveNameAdd="theMoneyMaker",pffData=TRUE){

  hmDir<-getwd()

  nfl2014Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2014/09/03", weeks = 17)
  nfl2015Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2015/09/09", weeks = 17)
  nfl2016Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2016/09/07", weeks = 17)
  nfl2017Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2017/09/07", weeks = 17)
  nfl2018Dates<-rDailyFantasy::dateRangeFormNfl(yyyymmdd = "2018/09/05", weeks = 17)
  nflDates <- data.frame("2014"=nfl2014Dates,"2015"=nfl2015Dates,"2016"=nfl2016Dates,"2017"=nfl2017Dates,"2018"=nfl2018Dates)
  nflDateColumn <- dplyr::if_else(modelYear==2014,1,dplyr::if_else(modelYear==2015,2,dplyr::if_else(modelYear==2016,3,dplyr::if_else(modelYear==2017,4,dplyr::if_else(modelYear==2018,5,0)))))
  modelDate <- nflDates[,nflDateColumn][modelWeek]

  #projection Model Check
  modelCheck<-list(ls())
  if(qbModel %in% modelCheck[[1]]==FALSE){
    qbModel<-rDailyFantasy::loadRData(paste0("~/NFL_Daily/machineLearning/QB/algos/",qbModel,".rda"))
  }
  if(rbModel %in% modelCheck[[1]]==FALSE){
    rbModel<-rDailyFantasy::loadRData(paste0("~/NFL_Daily/machineLearning/RB/algos/",rbModel,".rda"))
  }
  if(wrModel %in% modelCheck[[1]]==FALSE){
    wrModel<-rDailyFantasy::loadRData(paste0("~/NFL_Daily/machineLearning/WR/algos/",wrModel,".rda"))
  }
  if(teModel %in% modelCheck[[1]]==FALSE){
    teModel<-rDailyFantasy::loadRData(paste0("~/NFL_Daily/machineLearning/TE/algos/",teModel,".rda"))
  }
  if(dstModel %in% modelCheck[[1]]==FALSE){
    dstModel<-rDailyFantasy::loadRData(paste0("~/NFL_Daily/machineLearning/DST/algos/",dstModel,".rda"))
  }
  if(pffModel %in% modelCheck[[1]]==FALSE){
    pffModel<-rDailyFantasy::loadRData(paste0("~/NFL_Daily/machineLearning/pffData/algos/",pffModel,".rda"))
  }
  #if(dstModel %in% modelCheck[[1]]==FALSE){
  #  dstModel<-rDailyFantasy::loadRData(paste0("~/NFL_Daily/machineLearning/DST/algos/",dstModel,".rda"))
  #}

  nflSystemsDf<-read.csv(file = "~/NFL_Daily/systemsQB.csv") %>% dplyr::select(-X)
  modelId<-nflSystemsDf[which(nflSystemsDf$sysNames==labModel),][2][[1]]

  #get football data
  #QB
  nfl<- nflCoreFun(mW=modelWeek, mY=modelYear, labModel=labModel,style="projections")
  QB<-nfl$QB %>% tidyr::unnest() %>% dplyr::filter(ContestSuffix==gameSlate)
  qbNames<- QB$Player_Name
  qbTeam<- QB$Team.x
  qbLeverage<- QB$LeveragePct
  qbActualPoints<- QB$ActualPoints
  qbSalary <- QB$Salary
  qbPlayerId <- QB$PlayerId
  qbScore<- QB$Score
  qbFRId<-QB %>% dplyr::select(Player_Name,FantasyResultId)

  qbVariableNames <- rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/QB/modelVariables/",qbVars,".rda"))
  preProcQB<-rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/QB/preProcessing/",qbPre,".rda"))
  qbProjForm<- QB[,names(QB) %in% qbVariableNames]
  qbProjForm<-predict(preProcQB,qbProjForm)
  projectedQb<-predict(qbModel,newdata=qbProjForm)
  qbComplete<- qbProjForm %>% mutate(projection=projectedQb, Player_Name=qbNames, ActualPoints=qbActualPoints, Salary=qbSalary, Team=qbTeam, Score=qbScore) %>% data.frame(stringsAsFactors = F)
  #qbCompletePitcherWOBADiff<-qbComplete$OppWobaAllowed - qbComplete$OppWobaAllowed15
  #qbCompletePitcherISODiff<-qbComplete$OppIsoAllowed - qbComplete$OppIsoAllowed15
  qbModelSaveName<-qbModel$method
  if(!is.na(saveNameAdd)){
  qbModelSaveName<-paste0(qbModel$method,saveNameAdd)
  }
  qbComplete2<-qbComplete %>% select(Player_Name,Salary,projection,ActualPoints,Team,Score) %>% mutate(projValue=qbComplete$Salary/qbComplete$projection,actValue=qbComplete$Salary/qbComplete$ActualPoints,ownProj=QB$p_own,Leverage=QB$LeveragePct) %>% dplyr::mutate(position=101) # %>% select(Player_Name,projection,ActualPoints,p_own,HRPerAB,SB_Per_Game,sb_pct,park_f,ProjPlusMinus,PtVal,wOBA_Split,ISO_Split,distance_r,ev_r,hh_r,fb_r,ld_r,gb_r,distance_diff,evd,hh_diff,BattedBallLuck,airtime_r,Vegas,Run_Change,WeatherRatingAlt,Wind_Speed,PrecipProb,Upside,OppWOBAAvg,OppWobaAllowed,OppWobaAllowed15,OppIsoAllowed,OppIsoAllowed15) %>% mutate(Opp_WOBADiff=qbCompletePitcherWOBADiff,OppISODiff=qbCompletePitcherISODiff)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/QB/",modelYear),showWarnings = FALSE)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/QB/",modelYear,"/",modelWeek),showWarnings = FALSE)
  qbComplete2[qbComplete2$Team == "ARI", "Team"] <- "ARZ"
  qbComplete2[qbComplete2$Team == "BAL", "Team"] <- "BLT"
  qbComplete2[qbComplete2$Team == "CLE", "Team"] <- "CLV"
  qbComplete2[qbComplete2$Team == "HOU", "Team"] <- "HST"
  qbComplete2[qbComplete2$Team == "LA", "Team"] <- "LAR"

  qbComplete2<-qbComplete2 %>% unique()


  write.csv(qbComplete2,file = paste0("~/NFL_Daily/dfsProjections/QB/",modelYear,"/",modelWeek,"/",gameSlate,"_",qbModelSaveName,".csv"))


  #RB
  #nfl<- nflCoreFun(mW=modelWeek, mY=modelYear, labModel=labModel,style="projections")
  RB<-nfl$RB %>% tidyr::unnest() %>% dplyr::filter(ContestSuffix==gameSlate)
  rbNames<- RB$Player_Name
  rbTeam<- RB$Team.x
  rbLeverage<- RB$LeveragePct
  rbScore<- RB$Score
  rbFRId<-RB %>% dplyr::select(Player_Name,FantasyResultId)


  rbActualPoints<- RB$ActualPoints
  rbSalary <- RB$Salary
  rbVariableNames <- rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/RB/modelVariables/",rbVars,".rda"))
  preProcRB<-rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/RB/preProcessing/",rbPre,".rda"))

  rbProjForm<- RB[,names(RB) %in% rbVariableNames]
  rbProjForm<-predict(preProcRB,rbProjForm)
  projectedRb<-predict(rbModel,newdata=rbProjForm)
  rbComplete<- rbProjForm %>% mutate(projection=projectedRb, Player_Name=rbNames, ActualPoints=rbActualPoints, Salary=rbSalary, Team=rbTeam, Score=rbScore) %>% data.frame(stringsAsFactors = F)
  #rbCompletePitcherWOBADiff<-rbComplete$OppWobaAllowed - rbComplete$OppWobaAllowed15
  #rbCompletePitcherISODiff<-rbComplete$OppIsoAllowed - rbComplete$OppIsoAllowed15
  rbModelSaveName<-rbModel$method
  if(!is.na(saveNameAdd)){
    rbModelSaveName<-paste0(rbModel$method,saveNameAdd)
  }
  rbComplete2<-rbComplete %>% select(Player_Name,Salary,projection,ActualPoints,Team,Score) %>% mutate(projValue=rbComplete$Salary/rbComplete$projection,actValue=rbComplete$Salary/rbComplete$ActualPoints,ownProj=RB$p_own,Leverage=RB$LeveragePct) %>% dplyr::mutate(position=102) # %>% select(Player_Name,projection,ActualPoints,p_own,HRPerAB,SB_Per_Game,sb_pct,park_f,ProjPlusMinus,PtVal,wOBA_Split,ISO_Split,distance_r,ev_r,hh_r,fb_r,ld_r,gb_r,distance_diff,evd,hh_diff,BattedBallLuck,airtime_r,Vegas,Run_Change,WeatherRatingAlt,Wind_Speed,PrecipProb,Upside,OppWOBAAvg,OppWobaAllowed,OppWobaAllowed15,OppIsoAllowed,OppIsoAllowed15) %>% mutate(Opp_WOBADiff=rbCompletePitcherWOBADiff,OppISODiff=rbCompletePitcherISODiff)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/RB/",modelYear),showWarnings = FALSE)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/RB/",modelYear,"/",modelWeek),showWarnings = FALSE)

  rbComplete2[rbComplete2$Team == "ARI", "Team"] <- "ARZ"
  rbComplete2[rbComplete2$Team == "BAL", "Team"] <- "BLT"
  rbComplete2[rbComplete2$Team == "CLE", "Team"] <- "CLV"
  rbComplete2[rbComplete2$Team == "HOU", "Team"] <- "HST"
  rbComplete2[rbComplete2$Team == "LA", "Team"] <- "LAR"

  rbComplete2<-rbComplete2 %>% unique()

  write.csv(rbComplete2,file = paste0("~/NFL_Daily/dfsProjections/RB/",modelYear,"/",modelWeek,"/",gameSlate,"_",rbModelSaveName,".csv"))
  #WR
  #nfl<- nflCoreFun(mW=modelWeek, mY=modelYear, labModel=labModel,style="projections")
  WR<-nfl$WR %>% tidyr::unnest() %>% dplyr::filter(ContestSuffix==gameSlate)
  wrNames<- WR$Player_Name
  wrTeam<- WR$Team.x
  wrLeverage<- WR$LeveragePct
  wrScore<- WR$Score
  wrFRId<-WR %>% dplyr::select(Player_Name,FantasyResultId)


  wrActualPoints<- WR$ActualPoints
  wrSalary <- WR$Salary
  wrVariableNames <- rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/WR/modelVariables/",wrVars,".rda"))
  preProcWR<-rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/WR/preProcessing/",wrPre,".rda"))

  wrProjForm<- WR[,names(WR) %in% wrVariableNames]
  wrProjForm<-predict(preProcWR,wrProjForm)
  projectedWr<-predict(wrModel,newdata=wrProjForm)
  wrComplete<- wrProjForm %>% mutate(projection=projectedWr, Player_Name=wrNames, ActualPoints=wrActualPoints, Salary=wrSalary, Team=wrTeam, Score=wrScore) %>% data.frame(stringsAsFactors = F)
  #wrCompletePitcherWOBADiff<-wrComplete$OppWobaAllowed - wrComplete$OppWobaAllowed15
  #wrCompletePitcherISODiff<-wrComplete$OppIsoAllowed - wrComplete$OppIsoAllowed15
  wrModelSaveName<-wrModel$method
  if(!is.na(saveNameAdd)){
    wrModelSaveName<-paste0(wrModel$method,saveNameAdd)
  }
  wrComplete2<-wrComplete %>% select(Player_Name,Salary,projection,ActualPoints,Team,Score) %>% mutate(projValue=wrComplete$Salary/wrComplete$projection,actValue=wrComplete$Salary/wrComplete$ActualPoints,ownProj=WR$p_own,Leverage=WR$LeveragePct) %>% dplyr::mutate(position=103) # %>% select(Player_Name,projection,ActualPoints,p_own,HRPerAB,SB_Per_Game,sb_pct,park_f,ProjPlusMinus,PtVal,wOBA_Split,ISO_Split,distance_r,ev_r,hh_r,fb_r,ld_r,gb_r,distance_diff,evd,hh_diff,BattedBallLuck,airtime_r,Vegas,Run_Change,WeatherRatingAlt,Wind_Speed,PrecipProb,Upside,OppWOBAAvg,OppWobaAllowed,OppWobaAllowed15,OppIsoAllowed,OppIsoAllowed15) %>% mutate(Opp_WOBADiff=wrCompletePitcherWOBADiff,OppISODiff=wrCompletePitcherISODiff)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/WR/",modelYear),showWarnings = FALSE)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/WR/",modelYear,"/",modelWeek),showWarnings = FALSE)

  wrComplete2[wrComplete2$Team == "ARI", "Team"] <- "ARZ"
  wrComplete2[wrComplete2$Team == "BAL", "Team"] <- "BLT"
  wrComplete2[wrComplete2$Team == "CLE", "Team"] <- "CLV"
  wrComplete2[wrComplete2$Team == "HOU", "Team"] <- "HST"
  wrComplete2[wrComplete2$Team == "LA", "Team"] <- "LAR"

  wrComplete2<-wrComplete2 %>% unique()

  write.csv(wrComplete2,file = paste0("~/NFL_Daily/dfsProjections/WR/",modelYear,"/",modelWeek,"/",gameSlate,"_",wrModelSaveName,".csv"))
  #TE
  #nfl<- nflCoreFun(mW=modelWeek, mY=modelYear, labModel=labModel,style="projections")
  TE<-nfl$TE %>% tidyr::unnest() %>% dplyr::filter(ContestSuffix==gameSlate)
  teNames<- TE$Player_Name
  teTeam<- TE$Team.x
  teLeverage<- TE$LeveragePct
  teScore<- TE$Score
  teFRId<-TE %>% dplyr::select(Player_Name,FantasyResultId)



  teActualPoints<- TE$ActualPoints
  teSalary <- TE$Salary
  teVariableNames <- rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/TE/modelVariables/",teVars,".rda"))
  preProcTE<-rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/TE/preProcessing/",tePre,".rda"))

  teProjForm<- TE[,names(TE) %in% teVariableNames]
  teProjForm<-predict(preProcTE,teProjForm)
  projectedTe<-predict(teModel,newdata=teProjForm)
  teComplete<- teProjForm %>% mutate(projection=projectedTe, Player_Name=teNames, ActualPoints=teActualPoints, Salary=teSalary, Team=teTeam, Score=teScore) %>% data.frame(stringsAsFactors = F)
  #teCompletePitcherWOBADiff<-teComplete$OppWobaAllowed - teComplete$OppWobaAllowed15
  #teCompletePitcherISODiff<-teComplete$OppIsoAllowed - teComplete$OppIsoAllowed15
  teModelSaveName<-teModel$method
  if(!is.na(saveNameAdd)){
    teModelSaveName<-paste0(teModel$method,saveNameAdd)
  }
  teComplete2<-teComplete %>% select(Player_Name,Salary,projection,ActualPoints,Team,Score) %>% mutate(projValue=teComplete$Salary/teComplete$projection,actValue=teComplete$Salary/teComplete$ActualPoints,ownProj=TE$p_own,Leverage=TE$LeveragePct) %>% dplyr::mutate(position=104) # %>% select(Player_Name,projection,ActualPoints,p_own,HRPerAB,SB_Per_Game,sb_pct,park_f,ProjPlusMinus,PtVal,wOBA_Split,ISO_Split,distance_r,ev_r,hh_r,fb_r,ld_r,gb_r,distance_diff,evd,hh_diff,BattedBallLuck,airtime_r,Vegas,Run_Change,WeatherRatingAlt,Wind_Speed,PrecipProb,Upside,OppWOBAAvg,OppWobaAllowed,OppWobaAllowed15,OppIsoAllowed,OppIsoAllowed15) %>% mutate(Opp_WOBADiff=teCompletePitcherWOBADiff,OppISODiff=teCompletePitcherISODiff)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/TE/",modelYear),showWarnings = FALSE)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/TE/",modelYear,"/",modelWeek),showWarnings = FALSE)


  teComplete2[teComplete2$Team == "ARI", "Team"] <- "ARZ"
  teComplete2[teComplete2$Team == "BAL", "Team"] <- "BLT"
  teComplete2[teComplete2$Team == "CLE", "Team"] <- "CLV"
  teComplete2[teComplete2$Team == "HOU", "Team"] <- "HST"
  teComplete2[teComplete2$Team == "LA", "Team"] <- "LAR"

  teComplete2<-teComplete2 %>% unique()


  write.csv(teComplete2,file = paste0("~/NFL_Daily/dfsProjections/TE/",modelYear,"/",modelWeek,"/",gameSlate,"_",teModelSaveName,".csv"))

  #DST
  #nfl<- nflCoreFun(mW=modelWeek, mY=modelYear, labModel=labModel,style="projections")
  DST<-nfl$DST %>% tidyr::unnest() %>% dplyr::filter(ContestSuffix==gameSlate)
  dstNames<- DST$Player_Name
  dstTeam<- DST$Team.x
  dstLeverage<- DST$LeveragePct
  dstScore<- DST$Score
  dstFRId<-DST %>% dplyr::select(Player_Name,FantasyResultId)
  dstActualPoints<- DST$ActualPoints
  dstSalary <- DST$Salary
  dstVariableNames <- rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/DST/modelVariables/",dstVars,".rda"))
  preProcDST<-rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/DST/preProcessing/",dstPre,".rda"))

  dstProjForm<- DST[,names(DST) %in% dstVariableNames]
  dstProjForm<-predict(preProcDST,dstProjForm)
  projectedDst<-predict(dstModel,newdata=dstProjForm)
  dstComplete<- dstProjForm %>% mutate(projection=projectedDst, Player_Name=dstNames, ActualPoints=dstActualPoints, Salary=dstSalary, Team=dstTeam, Score=dstScore) %>% data.frame(stringsAsFactors = F)
  #dstCompletePitcherWOBADiff<-dstComplete$OppWobaAllowed - dstComplete$OppWobaAllowed15
  #dstCompletePitcherISODiff<-dstComplete$OppIsoAllowed - dstComplete$OppIsoAllowed15
  dstModelSaveName<-dstModel$method
  if(!is.na(saveNameAdd)){
    dstModelSaveName<-paste0(dstModel$method,saveNameAdd)
  }
  dstComplete2<-dstComplete %>% select(Player_Name,Salary,projection,ActualPoints,Team,Score) %>% mutate(projValue=dstComplete$Salary/dstComplete$projection,actValue=dstComplete$Salary/dstComplete$ActualPoints,ownProj=DST$p_own,Leverage=DST$LeveragePct) %>% dplyr::mutate(position=105) # %>% select(Player_Name,projection,ActualPoints,p_own,HRPerAB,SB_Per_Game,sb_pct,park_f,ProjPlusMinus,PtVal,wOBA_Split,ISO_Split,distance_r,ev_r,hh_r,fb_r,ld_r,gb_r,distance_diff,evd,hh_diff,BattedBallLuck,airtime_r,Vegas,Run_Change,WeatherRatingAlt,Wind_Speed,PrecipProb,Upside,OppWOBAAvg,OppWobaAllowed,OppWobaAllowed15,OppIsoAllowed,OppIsoAllowed15) %>% mutate(Opp_WOBADiff=dstCompletePitcherWOBADiff,OppISODiff=dstCompletePitcherISODiff)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/DST/",modelYear),showWarnings = FALSE)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/DST/",modelYear,"/",modelWeek),showWarnings = FALSE)

  dstComplete2[dstComplete2$Team == "ARI", "Team"] <- "ARZ"
  dstComplete2[dstComplete2$Team == "BAL", "Team"] <- "BLT"
  dstComplete2[dstComplete2$Team == "CLE", "Team"] <- "CLV"
  dstComplete2[dstComplete2$Team == "HOU", "Team"] <- "HST"
  dstComplete2[dstComplete2$Team == "LA", "Team"] <- "LAR"

  dstComplete2<-dstComplete2 %>% unique()

  write.csv(dstComplete2,file = paste0("~/NFL_Daily/dfsProjections/DST/",modelYear,"/",modelWeek,"/",gameSlate,"_",dstModelSaveName,".csv"))

  #PFF Data

  if(pffData==TRUE){
    wrCbMatchups<-read.csv(paste0(file="~/NFL_Daily/dfsProjections/WR/",modelYear,"/",modelWeek,"/wr_cb_matchup.csv"))
    teMatchups<-read.csv(paste0(file="~/NFL_Daily/dfsProjections/TE/",modelYear,"/",modelWeek,"/te_matchup.csv"))
    lineMatchups<-read.csv(paste0(file="~/NFL_Daily/dfsProjections/RB/",modelYear,"/",modelWeek,"/ol_dl_matchup.csv"))
    #adjPoints/Opponent
    adjPointsAllowedData<-read.csv(paste0(file="~/NFL_Daily/dfsProjections/",modelYear,"/",modelWeek,"/adjPointsAllowed.csv"))
    names(adjPointsAllowedData)<-c("Team",names(adjPointsAllowedData[,2:6]))
    #OppQB
    adjOppQB<-adjPointsAllowedData[,c(1,2)]
    qbComplete2<-inner_join(qbComplete2,adjOppQB,by = "Team")  ###change nfl2018_2_QB to qbComplete2
    #adjPtsQB
    adjPtQb<-adjPointsAllowedData[,c(1,3)]
    names(adjPtQb)<-c("Team","AdjFPAl")
    qbComplete2<-inner_join(qbComplete2,adjPtQb,by = "Team")
    #advantageQB
    AdvQB<-lineMatchups[,c(2,8)]
    names(AdvQB)<-c("Team","adv")
    qbComplete2<-inner_join(qbComplete2,AdvQB,by="Team")
    #OppRB
    adjOppRB<-adjPointsAllowedData[,c(1,2)]
    rbComplete2<-inner_join(rbComplete2,adjOppRB,by = "Team")   ###change nfl2018_2_RB to rbComplete2
    #adjPtsRB
    adjPtRb<-adjPointsAllowedData[,c(1,4)]
    names(adjPtRb)<-c("Team","AdjFPAl")
    rbComplete2<-inner_join(rbComplete2,adjPtRb,by = "Team")
    #advantageRB
    AdvRB<-lineMatchups[,c(2,9)]
    names(AdvRB)<-c("Team","adv")
    rbComplete2<-inner_join(rbComplete2,AdvRB,by="Team")


    #OppWR
    adjOppWR<-adjPointsAllowedData[,c(1,2)]
    wrComplete2<-inner_join(wrComplete2,adjOppWR,by = "Team")   ###change nfl2018_2_WR to wrComplete2
    #adjPtsWR
    adjPtWr<-adjPointsAllowedData[,c(1,5)]
    names(adjPtWr)<-c("Team","AdjFPAl")
    wrComplete2<-left_join(wrComplete2,adjPtWr,by = "Team")
    #advantageWR
    AdvWR<-wrCbMatchups[,c(2,16)]
    names(AdvWR)<-c("Player_Name","adv")
    AdvWR[,1]<-tolower(AdvWR$Player_Name)
    wrComplete2[,1]<-tolower(wrComplete2$Player_Name)
    wrComplete2[wrComplete2$Player_Name=="paul richardson","Player_Name"]<-"paul richardson jr."
    wrComplete2[wrComplete2$Player_Name=="ted ginn","Player_Name"]<-"ted ginn jr."
    wrComplete2[wrComplete2$Player_Name=="marvin jones","Player_Name"]<-"marvin jones jr."
    wrComplete2[wrComplete2$Player_Name=="will fuller","Player_Name"]<-"will fuller v"
    wrComplete2[wrComplete2$Player_Name=="odell beckham","Player_Name"]<-"odell beckham jr."
    wrComplete2[wrComplete2$Player_Name=="allen robinson","Player_Name"]<-"allen robinson ii"
    wrComplete2[wrComplete2$Player_Name=="josh bellamy","Player_Name"]<-"joshua bellamy"
    wrComplete2[wrComplete2$Player_Name=="willie snead","Player_Name"]<-"willie snead iv"
    wrComplete2[wrComplete2$Player_Name=="key'vantanie coutee","Player_Name"]<-"keke coutee"
    wrComplete2<-left_join(wrComplete2,AdvWR,by="Player_Name")

    #OppTE
    adjOppTE<-adjPointsAllowedData[,c(1,2)]
    teComplete2<-inner_join(teComplete2,adjOppTE,by = "Team")   ###change nfl2018_2_TE to teComplete2
    #adjPtsTE
    adjPtTe<-adjPointsAllowedData[,c(1,6)]
    names(adjPtTe)<-c("Team","AdjFPAl")
    teComplete2<-inner_join(teComplete2,adjPtTe,by = "Team")
    #advantageTE
    AdvTE<-teMatchups[,c(2,15)]
    names(AdvTE)<-c("Player_Name","adv")
    AdvTE[,1]<-tolower(AdvTE$Player_Name)
    teComplete2[,1]<-tolower(teComplete2$Player_Name)
    teComplete2[teComplete2$Player_Name=="oj howard","Player_Name"]<-"o.j. howard"
    teComplete2<-left_join(teComplete2,AdvTE,by="Player_Name")

    #OppDST
    adjOppDST<-adjPointsAllowedData[,c(1,2)]
    dstComplete2<-inner_join(dstComplete2,adjOppDST,by = "Team")

    pffJson<-"pffGrades.json"
    setwd(paste0("~/NFL_Daily/dfsProjections/",modelYear,"/",modelWeek,"/"))
    pffGrades <- jsonlite::fromJSON(pffJson, flatten = TRUE)

    setwd("~/rDFS/rDailyFantasy")

    pffGradesDf <- pffGrades$team_overview %>% data.frame() %>% select(-name,-franchise_id)
    names(pffGradesDf)[6]<-c("Team")

    pffGradesDfOpp <- pffGradesDf
    names(pffGradesDfOpp)<-gsub("grades.","opp.",names(pffGradesDf))
    pffGradesDfOpp <- pffGradesDfOpp %>% select(-wins,-ties,-points_scored,-points_allowed,-losses)

    names(pffGradesDfOpp)<-gsub("Team","Opp",names(pffGradesDfOpp))



    #RB
    rbComplete2<-inner_join(rbComplete2,pffGradesDf,by="Team") %>% inner_join(pffGradesDfOpp,by = "Opp")
    tmAdv<-rbComplete2$grades.offense-rbComplete2$opp.defense
    lnAdv<-rbComplete2$grades.run_block-rbComplete2$opp.run_defense
    rcAdv<-as.numeric(0)
    rbComplete2<-rbComplete2 %>% dplyr::mutate(teamAdv=tmAdv,lineAdv=lnAdv,recAdv=rcAdv)
    #rbPff_O<-pffGradesDf[,c(8,11,12,18)]
    #names(rbPff_O)<-c("Team","runBlk","run","offense")
    #rbComplete2<-inner_join(rbComplete2,rbPff_O,by="Team")
    #rbPff_D<-pffGradesDf[,c(8,10,20)]
    #names(rbPff_D)<-c("Opp","runDef","defense")
    #rbComplete2<-inner_join(rbComplete2,rbPff_D,by="Opp")
    #rbrun<-((rbComplete2$runBlk+rbComplete2$run)/2 - rbComplete2$runDef)
    #rbComplete2 <- rbComplete2 %>% mutate(runDif=rbrun)


    #WR
    wrComplete2<-inner_join(wrComplete2,pffGradesDf,by="Team") %>% inner_join(pffGradesDfOpp,by = "Opp")
    tmAdv<-wrComplete2$grades.offense-wrComplete2$opp.defense
    lnAdv<-as.numeric(0)
    rcAdv<-wrComplete2$grades.pass_route - wrComplete2$opp.coverage_defense
    wrComplete2<-wrComplete2 %>% dplyr::mutate(teamAdv=tmAdv,lineAdv=lnAdv,recAdv=rcAdv)

    #wrPff_O<-pffGradesDf[,c(8,15,16,17,18)]
    #names(wrPff_O)<-c("Team","passRte","passBlk","pass","offense")
    #wrComplete2<-inner_join(wrComplete2,wrPff_O,by="Team")
    #wrPff_D<-pffGradesDf[,c(8,13,21,20)]
    #names(wrPff_D)<-c("Opp","passRush","passCovge","defense")
    #wrComplete2<-inner_join(wrComplete2,wrPff_D,by="Opp")
    #wrpass<-((wrComplete2$passRte+wrComplete2$passBlk+wrComplete2$pass)/3 - (wrComplete2$passRush+wrComplete2$passCovge)/2)
    #wrComplete2 <- wrComplete2 %>% mutate(passDif=wrpass)

    #QB
    qbComplete2<-inner_join(qbComplete2,pffGradesDf,by="Team") %>% inner_join(pffGradesDfOpp,by = "Opp")
    tmAdv<-qbComplete2$grades.offense-qbComplete2$opp.defense
    lnAdv<-qbComplete2$grades.pass_block-qbComplete2$opp.pass_rush_defense
    rcAdv<-qbComplete2$grades.pass-qbComplete2$opp.coverage_defense
    qbComplete2<-qbComplete2 %>% dplyr::mutate(teamAdv=tmAdv,lineAdv=lnAdv,recAdv=rcAdv)

    #qbPff_O<-pffGradesDf[,c(8,15,16,17,18)]
    #names(qbPff_O)<-c("Team","passRte","passBlk","pass","offense")
    #qbComplete2<-inner_join(qbComplete2,qbPff_O,by="Team")
    #qbPff_D<-pffGradesDf[,c(8,13,21,20)]
    #names(qbPff_D)<-c("Opp","passRush","passCovge","defense")
    #qbComplete2<-inner_join(qbComplete2,qbPff_D,by="Opp")
    #qbpass<-((qbComplete2$passRte+qbComplete2$passBlk+qbComplete2$pass)/3 - (qbComplete2$passRush+qbComplete2$passCovge)/2)
    #qbComplete2 <- qbComplete2 %>% mutate(passDif=qbpass)

    #TE
    teComplete2<-inner_join(teComplete2,pffGradesDf,by="Team") %>% inner_join(pffGradesDfOpp,by = "Opp")
    tmAdv<-teComplete2$grades.offense-teComplete2$opp.defense
    lnAdv<-as.numeric(0)
    rcAdv<-teComplete2$grades.pass_route - teComplete2$opp.coverage_defense
    teComplete2<-teComplete2 %>% dplyr::mutate(teamAdv=tmAdv,lineAdv=lnAdv,recAdv=rcAdv)

    #tePff_O<-pffGradesDf[,c(8,15,16,17,18)]
    #names(tePff_O)<-c("Team","passRte","passBlk","pass","offense")
    #teComplete2<-inner_join(teComplete2,tePff_O,by="Team")
    #tePff_D<-pffGradesDf[,c(8,13,21,20)]
    #names(tePff_D)<-c("Opp","passRush","passCovge","defense")
    #teComplete2<-inner_join(teComplete2,tePff_D,by="Opp")
    #tepass<-((teComplete2$passRte+teComplete2$passBlk+teComplete2$pass)/3 - (teComplete2$passRush+teComplete2$passCovge)/2)
    #teComplete2 <- teComplete2 %>% mutate(passDif=tepass)

    #dst
    dstComplete2<-inner_join(dstComplete2,pffGradesDf,by="Team") %>% inner_join(pffGradesDfOpp,by = "Opp")
    tmAdv<-dstComplete2$opp.offense - dstComplete2$grades.defense
    lnAdv<-as.numeric(0)
    rcAdv<-as.numeric(0)
    dstComplete2<-dstComplete2 %>% dplyr::mutate(teamAdv=tmAdv,lineAdv=lnAdv,recAdv=rcAdv)

    #dstPff_O<-pffGradesDf[,c(8,13,10,21,20)]
    #names(dstPff_O)<-c("Team","passRush","runDef","covDef","defense")
    #dstComplete2<-inner_join(dstComplete2,dstPff_O,by="Team")

    qbmUniq<- make.unique(qbComplete2$Player_Name)
    qbRepl<- grep(".[[:digit:]]",qbmUniq)
    if(length(qbRepl)!=0){
    qbComplete2<- qbComplete2[-qbRepl,]
    }

    rbmUniq<- make.unique(rbComplete2$Player_Name)
    rbRepl<- grep(".[[:digit:]]",rbmUniq)
    if(length(rbRepl)!=0){
    rbComplete2<- rbComplete2[-rbRepl,]
    }

    wrmUniq<- make.unique(wrComplete2$Player_Name)
    wrRepl<- grep(".[[:digit:]]",wrmUniq)
    if(length(wrRepl)!=0){
    wrComplete2<- wrComplete2[-wrRepl,]
    }

    temUniq<- make.unique(teComplete2$Player_Name)
    teRepl<- grep(".[[:digit:]]",temUniq)
    if(length(teRepl)!=0){
    teComplete2<- teComplete2[-teRepl,]
    }

    dstmUniq<- make.unique(dstComplete2$Player_Name)
    dstRepl<- grep(".[[:digit:]]",dstmUniq)
    if(length(dstRepl)!=0){
    dstComplete2<- dstComplete2[-dstRepl,]
    }



    write.csv(qbComplete2,file = paste0("~/NFL_Daily/dfsProjections/QB/",modelYear,"/",modelWeek,"/",gameSlate,"_",qbModelSaveName,".csv"))

    write.csv(rbComplete2,file = paste0("~/NFL_Daily/dfsProjections/RB/",modelYear,"/",modelWeek,"/",gameSlate,"_",rbModelSaveName,".csv"))

    write.csv(wrComplete2,file = paste0("~/NFL_Daily/dfsProjections/WR/",modelYear,"/",modelWeek,"/",gameSlate,"_",wrModelSaveName,".csv"))

    write.csv(teComplete2,file = paste0("~/NFL_Daily/dfsProjections/TE/",modelYear,"/",modelWeek,"/",gameSlate,"_",teModelSaveName,".csv"))

    write.csv(dstComplete2,file = paste0("~/NFL_Daily/dfsProjections/DST/",modelYear,"/",modelWeek,"/",gameSlate,"_",dstModelSaveName,".csv"))

    }

  #combined
  projectionsList<-list(QB=qbComplete2,RB=rbComplete2,WR=wrComplete2,TE=teComplete2,DST=dstComplete2)
  allProjectionsCombined<-bind_rows(projectionsList)
  if(pffData==TRUE){
    preProcPFF<-rDailyFantasy::loadRData(fileName = paste0("~/NFL_Daily/machineLearning/pffData/preProcessing/",pffPre,".rda"))
    pffProjForm<-predict(preProcPFF,allProjectionsCombined)
    projectedPFF<-predict(pffModel,newdata=pffProjForm)
    allProjectionsCombined<- allProjectionsCombined %>% mutate(pffProj=projectedPFF)
  }

  dir.create(path = paste0("~/NFL_Daily/dfsProjections/combined/",modelYear),showWarnings = FALSE)
  dir.create(path = paste0("~/NFL_Daily/dfsProjections/combined/",modelYear,"/",modelWeek),showWarnings = FALSE)
  write.csv(allProjectionsCombined,file = paste0("~/NFL_Daily/dfsProjections/combined/",modelYear,"/",modelWeek,"/",gameSlate,"_",qbModelSaveName,".csv"))


  fridBind<-dplyr::bind_rows(qbFRId,rbFRId,wrFRId,teFRId,dstFRId)
  allProjs<-allProjectionsCombined %>% select(Player_Name,pffProj)


  allProjs[,1]<-tolower(allProjs$Player_Name)
  fridBind[,1]<-tolower(fridBind$Player_Name)
  fridBind[fridBind$Player_Name=="oj howard","Player_Name"]<-"o.j. howard"
  fridBind[fridBind$Player_Name=="paul richardson","Player_Name"]<-"paul richardson jr."
  fridBind[fridBind$Player_Name=="ted ginn","Player_Name"]<-"ted ginn jr."
  fridBind[fridBind$Player_Name=="marvin jones","Player_Name"]<-"marvin jones jr."
  fridBind[fridBind$Player_Name=="will fuller","Player_Name"]<-"will fuller v"
  fridBind[fridBind$Player_Name=="odell beckham","Player_Name"]<-"odell beckham jr."
  fridBind[fridBind$Player_Name=="allen robinson","Player_Name"]<-"allen robinson ii"
  fridBind[fridBind$Player_Name=="josh bellamy","Player_Name"]<-"joshua bellamy"
  fridBind[fridBind$Player_Name=="willie snead","Player_Name"]<-"willie snead iv"
  fridBind[fridBind$Player_Name=="key'vantanie coutee","Player_Name"]<-"keke coutee"

  allFRIDProj<- dplyr::inner_join(allProjs,fridBind,by="Player_Name") %>% unique()

  setwd("~/")

  foreach(b=1:nrow(allFRIDProj)) %do% {
    system(command = paste0("curl --silent https://www.fantasylabs.com/api/playermodel/",modelId,"/customize/",allFRIDProj[b,3],"/AvgPts/",modelDate,"?value=",round(allFRIDProj[b,2],2)," > /dev/null"))
  }

  setwd(hmDir)



  return(list(QB=qbComplete2,RB=rbComplete2,WR=wrComplete2,TE=teComplete2,DST=dstComplete2, combined=allProjectionsCombined))


}
