#' nflCoreFun
#'
#' @param partitionSize 0-1
#' @param mY modelYears
#' @param mW modelWeeks
#' @param style research/projections
#' @param labModel modelName
#'
#' @return researchData or projectionData
#' @export
#'
#' @examples mlbCoreFun(mY,mW,partitionSize,labModel,style)
nflCoreFun<-function(mY=2015:2017,mW=1:17,
                     #pastEvent=FALSE,
                     #research=FALSE,
                     partitionSize=0.8,
                     labModel="2018",
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
    nflModelSetup(modelYear=mY,modelWeek=mW,pastEvent=FALSE,labModel = labModel,postRefresh = prf,systemRefresh = 0)
  }
  #if(style=="research"){
   # foreach(i=1:length(mY)) %do%
    #  foreach(j=1:length(mW)) %do% nflModelSetup(modelYear = mY[i],modelWeek = mW[j],pastEvent=TRUE,postRefresh=prf,labModel=labModel)
     # }
  #  }

#QB

  QB<-foreach(a=1:length(mY)) %do% {
    foreach(b=1:length(mW)) %do% {
      slates<-data.frame(slates=unlist(list.files(path=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b]))))
      slates<-slates %>% dplyr::filter(slates!="matchupData")
      foreach(c=1:nrow(slates),.combine = 'rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b],"/",slates$slates[c],"/QB/stats.csv"),stringsAsFactors = FALSE)
    }
  }

  bindedQb<-dplyr::bind_rows(QB[[1]]) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-PlayerId,-PositionId) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)


  suppressWarnings(bindedNumericVersionQb<-apply(bindedQb,2,as.numeric) %>% data.frame())
  predictorsQb<-bindedNumericVersionQb %>% dplyr::select(-ActualPoints)
  actualQb<-bindedNumericVersionQb$ActualPoints
  predictorsNAZeroQb<-rDailyFantasy::na.What(predictorsQb,0)
  allDataQb<-predictorsNAZeroQb %>% dplyr::mutate(ActualPoints=actualQb)

  #byGameSlate
  if(style=="projections"){
    bySlateQb<-foreach(i=1:length(QB)) %do% QB[[i]] %>% data.frame(stringsAsFactors=F) %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  }


  if(research==TRUE){
    dataPartition<-caret::createDataPartition(allDataQb$ActualPoints,p=partitionSize,list=FALSE)
    trainQb<-allDataQb[dataPartition,]
    testQb<-allDataQb[-dataPartition,]

    trainXQb<-trainQb %>% dplyr::select(-ActualPoints)
    trainYQb<-trainQb$ActualPoints
    testXQb<-testQb %>% dplyr::select(-ActualPoints)
    testYQb<-testQb$ActualPoints
  }

  #RB

  RB<-foreach(a=1:length(mY)) %do% {
    foreach(b=1:length(mW)) %do% {
      slates<-data.frame(slates=unlist(list.files(path=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b]))))
      slates<-slates %>% dplyr::filter(slates!="matchupData")
      foreach(c=1:nrow(slates),.combine = 'rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b],"/",slates$slates[c],"/RB/stats.csv"),stringsAsFactors = FALSE)
    }
  }

  bindedRb<-dplyr::bind_rows(RB[[1]]) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-PlayerId,-PositionId) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)


  suppressWarnings(bindedNumericVersionRb<-apply(bindedRb,2,as.numeric) %>% data.frame())
  predictorsRb<-bindedNumericVersionRb %>% dplyr::select(-ActualPoints)
  actualRb<-bindedNumericVersionRb$ActualPoints
  predictorsNAZeroRb<-rDailyFantasy::na.What(predictorsRb,0)
  allDataRb<-predictorsNAZeroRb %>% dplyr::mutate(ActualPoints=actualRb)

  #byGameSlate
  if(style=="projections"){
    bySlateRb<-foreach(i=1:length(RB)) %do% RB[[i]] %>% data.frame(stringsAsFactors=F) %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  }


  if(research==TRUE){
    dataPartition<-caret::createDataPartition(allDataRb$ActualPoints,p=partitionSize,list=FALSE)
    trainRb<-allDataRb[dataPartition,]
    testRb<-allDataRb[-dataPartition,]

    trainXRb<-trainRb %>% dplyr::select(-ActualPoints)
    trainYRb<-trainRb$ActualPoints
    testXRb<-testRb %>% dplyr::select(-ActualPoints)
    testYRb<-testRb$ActualPoints
  }

  #WR

  WR<-foreach(a=1:length(mY)) %do% {
    foreach(b=1:length(mW)) %do% {
      slates<-data.frame(slates=unlist(list.files(path=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b]))))
      slates<-slates %>% dplyr::filter(slates!="matchupData")

      foreach(c=1:nrow(slates),.combine = 'rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b],"/",slates$slates[c],"/WR/stats.csv"),stringsAsFactors = FALSE)
    }
  }

  bindedWr<-dplyr::bind_rows(WR[[1]]) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-PlayerId,-PositionId) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)


  suppressWarnings(bindedNumericVersionWr<-apply(bindedWr,2,as.numeric) %>% data.frame())
  predictorsWr<-bindedNumericVersionWr %>% dplyr::select(-ActualPoints)
  actualWr<-bindedNumericVersionWr$ActualPoints
  predictorsNAZeroWr<-rDailyFantasy::na.What(predictorsWr,0)
  allDataWr<-predictorsNAZeroWr %>% dplyr::mutate(ActualPoints=actualWr)

  #byGameSlate
  if(style=="projections"){
    bySlateWr<-foreach(i=1:length(WR)) %do% WR[[i]] %>% data.frame(stringsAsFactors=F) %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  }


  if(research==TRUE){
    dataPartition<-caret::createDataPartition(allDataWr$ActualPoints,p=partitionSize,list=FALSE)
    trainWr<-allDataWr[dataPartition,]
    testWr<-allDataWr[-dataPartition,]

    trainXWr<-trainWr %>% dplyr::select(-ActualPoints)
    trainYWr<-trainWr$ActualPoints
    testXWr<-testWr %>% dplyr::select(-ActualPoints)
    testYWr<-testWr$ActualPoints
  }

  #TE

  TE<-foreach(a=1:length(mY)) %do% {
    foreach(b=1:length(mW)) %do% {
      slates<-data.frame(slates=unlist(list.files(path=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b]))))
      slates<-slates %>% dplyr::filter(slates!="matchupData")
      foreach(c=1:nrow(slates),.combine = 'rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b],"/",slates$slates[c],"/TE/stats.csv"),stringsAsFactors = FALSE)
    }
  }

  bindedTe<-dplyr::bind_rows(TE[[1]]) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-PlayerId,-PositionId) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)


  suppressWarnings(bindedNumericVersionTe<-apply(bindedTe,2,as.numeric) %>% data.frame())
  predictorsTe<-bindedNumericVersionTe %>% dplyr::select(-ActualPoints)
  actualTe<-bindedNumericVersionTe$ActualPoints
  predictorsNAZeroTe<-rDailyFantasy::na.What(predictorsTe,0)
  allDataTe<-predictorsNAZeroTe %>% dplyr::mutate(ActualPoints=actualTe)

  #byGameSlate
  if(style=="projections"){
    bySlateTe<-foreach(i=1:length(TE)) %do% TE[[i]] %>% data.frame(stringsAsFactors=F) %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  }


  if(research==TRUE){
    dataPartition<-caret::createDataPartition(allDataTe$ActualPoints,p=partitionSize,list=FALSE)
    trainTe<-allDataTe[dataPartition,]
    testTe<-allDataTe[-dataPartition,]

    trainXTe<-trainTe %>% dplyr::select(-ActualPoints)
    trainYTe<-trainTe$ActualPoints
    testXTe<-testTe %>% dplyr::select(-ActualPoints)
    testYTe<-testTe$ActualPoints
  }

  #DST

  DST<-foreach(a=1:length(mY)) %do% {
    foreach(b=1:length(mW)) %do% {
      slates<-data.frame(slates=unlist(list.files(path=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b]))))
      slates<-slates %>% dplyr::filter(slates!="matchupData")
      foreach(c=1:nrow(slates),.combine = 'rbind') %do% read.csv(file=paste0("~/NFL_Daily/dataSets/",mY[a],"/",mW[b],"/",slates$slates[c],"/DST/stats.csv"),stringsAsFactors = FALSE)
    }
  }

  bindedDst<-dplyr::bind_rows(DST[[1]]) %>% dplyr::select(-X,-ContestGroupId,-GameCount,-Player_Name,-PlayerId,-PositionId) %>% dplyr::filter(ContestSuffix=="Main") %>% dplyr::select(-ContestSuffix)


  suppressWarnings(bindedNumericVersionDst<-apply(bindedDst,2,as.numeric) %>% data.frame())
  predictorsDst<-bindedNumericVersionDst %>% dplyr::select(-ActualPoints)
  actualDst<-bindedNumericVersionDst$ActualPoints
  predictorsNAZeroDst<-rDailyFantasy::na.What(predictorsDst,0)
  allDataDst<-predictorsNAZeroDst %>% dplyr::mutate(ActualPoints=actualDst)

  #byGameSlate
  if(style=="projections"){
    bySlateDst<-foreach(i=1:length(DST)) %do% DST[[i]] %>% data.frame(stringsAsFactors=F) %>% dplyr::group_by(ContestSuffix) %>% tidyr::nest()
  }


  if(research==TRUE){
    dataPartition<-caret::createDataPartition(allDataDst$ActualPoints,p=partitionSize,list=FALSE)
    trainDst<-allDataDst[dataPartition,]
    testDst<-allDataDst[-dataPartition,]

    trainXDst<-trainDst %>% dplyr::select(-ActualPoints)
    trainYDst<-trainDst$ActualPoints
    testXDst<-testDst %>% dplyr::select(-ActualPoints)
    testYDst<-testDst$ActualPoints
  }




  if(research==TRUE){
    return(list(QB=allDataQb,QBTrain=trainQb,QBTest=testQb,QBTrainX=trainXQb,QBTrainY=trainYQb,QBTestX=testXQb,QBTestY=testYQb,
                RB=allDataRb,RBTrain=trainRb,RBTest=testRb,RBTrainX=trainXRb,RBTrainY=trainYRb,RBTestX=testXRb,RBTestY=testYRb,
                WR=allDataWr,WRTrain=trainWr,WRTest=testWr,WRTrainX=trainXWr,WRTrainY=trainYWr,WRTestX=testXWr,WRTestY=testYWr,
                TE=allDataTe,TETrain=trainTe,TETest=testTe,TETrainX=trainXTe,TETrainY=trainYTe,TETestX=testXTe,TETestY=testYTe,
                DST=allDataDst,DSTTrain=trainDst,DSTTest=testDst,DSTTrainX=trainXDst,DSTTrainY=trainYDst,DSTTestX=testXDst,DSTTestY=testYDst))
  }else{
    return(list(QB=bySlateQb,
                RB=bySlateRb,
                WR=bySlateWr,
                TE=bySlateTe,
                DST=bySlateDst))
  }



}
