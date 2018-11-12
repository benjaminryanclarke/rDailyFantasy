#' mlbCoreFun
#'
#' @param dateV vector
#' @param pastEvent T/F
#' @param research T/F
#' @param partitionSize 0-1
#' @param labModel modelName
#' @param statsRefresh T/F
#' @param toProject T/F
#'
#' @return researchData or projectionData
#' @export
#'
#' @examples mlbCoreFun()
mlbCoreFun<-function(dateV=rDailyFantasy::dateRangeForm("2018/03/29",days = 1),
                     #pastEvent=FALSE,
                     #research=FALSE,
                     partitionSize=0.8,
                     labModel="2018Early",
                     #statsRefresh=FALSE,
                     #toProject=TRUE
                     style="projections"){
  require(foreach)
  require(dplyr)
  require(tidyr)
  require(caret)

  #getStatsData
  #prf<-if_else(statsRefresh==TRUE,TRUE,FALSE)

  if(style=="research"){
    pastEvent=TRUE
    statsRefresh=TRUE
    research=TRUE
  }

  if(style=="projections"){
    pastEvent=FALSE
    statsRefresh=FALSE
    research=FALSE
  }

  prf<-if_else(statsRefresh==TRUE,1,0)

  if(style=="projections"){
mlbModelSetup(modelDate=dateV,pastEvent=FALSE,labModel = labModel,postRefresh = prf)
}
  if(style=="research"){
foreach(i=1:length(dateV)) %do% mlbModelSetup(modelDate=dateV[i],pastEvent=TRUE,postRefresh=prf,labModel=labModel)
}

  #bats
  bats<-foreach(a=1:length(dateV)) %do% {
    slates<-data.frame(slates=unlist(list.files(path=paste0("~/MLB_Daily/dataSets/",dateV[a]))))
    foreach(b=1:nrow(slates),.combine = 'rbind') %do% read.csv(file=paste0("~/MLB_Daily/dataSets/",dateV[a],"/",slates$slates[b],"/hitters/stats.csv"),stringsAsFactors = FALSE)
  }
  if(pastEvent==FALSE || style=="projections"){
    binded<-dplyr::bind_rows(bats) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-`L.R`,-Exp_min,-Exp,-MyTrends,-RunsP,-RunsB,-Opp_RunsP,-Opp_RunsB,-PrecipProbBatter,-PrecipProbPitcher,-PlayerId,-QualityStartsPerStart) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)
  }else{
    binded<-dplyr::bind_rows(bats) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-`L.R`,-Exp_min,-Exp,-MyTrends,-RunsP,-RunsB,-Opp_RunsP,-Opp_RunsB,-PrecipProbBatter,-PrecipProbPitcher,-PlayerId,-QualityStartsPerStart,-PointsPlusMinus,-TourneyOwnership,-PA,-Doubles,-R,-IBB,-SF,-SB,-LineupOrder,-H,-Triples,-RBI,-SO,-SH,-CS,-AB,-Singles,-HR,-BB,-HBP,-GDP,-AVG) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)
  }
  bindedNumericVersion<-apply(binded,2,as.numeric) %>% data.frame()
  predictors<-bindedNumericVersion %>% dplyr::select(-ActualPoints)
  actual<-bindedNumericVersion$ActualPoints
  predictorsNAZero<-rDailyFantasy::na.What(predictors,0)
  allData<-predictorsNAZero %>% dplyr::mutate(ActualPoints=actual)

  #byGameSlate
  if(style=="projections"){
  bySlate<-foreach(i=1:length(bats)) %do% bats[[i]] %>% data.frame(stringsAsFactors=F) %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  }

  if(research==TRUE){
    dataPartition<-caret::createDataPartition(allData$ActualPoints,p=partitionSize,list=FALSE)
    train<-allData[dataPartition,]
    test<-allData[-dataPartition,]

    trainX<-train %>% dplyr::select(-ActualPoints)
    trainY<-train$ActualPoints
    testX<-test %>% dplyr::select(-ActualPoints)
    testY<-test$ActualPoints
  }

  #Arms

  arms<-foreach(a=1:length(dateV)) %do% {
    slates<-data.frame(slates=unlist(list.files(path=paste0("~/MLB_Daily/dataSets/",dateV[a]))))
    foreach(b=1:nrow(slates),.combine = 'rbind') %do% read.csv(file=paste0("~/MLB_Daily/dataSets/",dateV[a],"/",slates$slates[b],"/pitchers/stats.csv"),stringsAsFactors = FALSE)
  }

  if(pastEvent==FALSE || style=="projections"){
    binded.P<-dplyr::bind_rows(arms) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-`L.R`,-Exp_min,-Exp,-MyTrends,-RunsP,-RunsB,-Opp_RunsP,-Opp_RunsB,-PrecipProbBatter,-PrecipProbPitcher,-PlayerId)
  }else{
    binded.P<-dplyr::bind_rows(arms) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-`L.R`,-Exp_min,-Exp,-MyTrends,-RunsP,-RunsB,-Opp_RunsP,-Opp_RunsB,-PrecipProbBatter,-PrecipProbPitcher,-PlayerId,-PointsPlusMinus,-TourneyOwnership,-PitchL,-PitchG,-PitchGS,-PitchCG,-PitchShO,-PitchSV,-PitchHLD,-PitchBS,-PitchIP,-PitchTBF,-PitchH,-PitchR,-PitchER,-PitchHR,-PitchBB,-PitchIBB,-PitchHBP,-PitchWP,-PitchBK,-PitchSO,-PitchERA,-PitchPA,-QualityStart,-GameScore) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)
  }

  bindedNumericVersion.P<-apply(binded,2,as.numeric) %>% data.frame()
  predictors.P<-bindedNumericVersion.P %>% dplyr::select(-ActualPoints)
  actual.P<-bindedNumericVersion.P$ActualPoints
  predictorsNAZero.P<-rDailyFantasy::na.What(predictors.P,0)
  allData.P<-predictorsNAZero.P %>% dplyr::mutate(ActualPoints=actual.P)

  #byGameSlate
  if(style=="projections"){
  bySlate.P<-foreach(i=1:length(arms)) %do% arms[[i]] %>% data.frame(stringsAsFactors=F) %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  }


  if(research==TRUE){
    dataPartition<-caret::createDataPartition(allData$ActualPoints,p=partitionSize,list=FALSE)
    train.P<-allData.P[dataPartition,]
    test.P<-allData.P[-dataPartition,]

    trainX.P<-train.P %>% dplyr::select(-ActualPoints)
    trainY.P<-train.P$ActualPoints
    testX.P<-test.P %>% dplyr::select(-ActualPoints)
    testY.P<-test.P$ActualPoints
  }

  if(research==TRUE){
    return(list(bats=allData,arms=allData.P,batsTrain=train,batsTest=test,armsTrain=train.P,armsTest=test.P,batsTrainX=trainX,batsTrainY=trainY,batsTestX=testX,batsTestY=testY,armsTrainX=trainX.P,arsmTrainY=trainY.P,armsTestX=testX.P,armsTestY=testY.P))
  }else{
    return(list(bats=bySlate,arms=bySlate.P))
  }



}
