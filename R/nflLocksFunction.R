#' nflLocksFunction
#'
#' @param modelWeek 11
#' @param modelYear 2018
#' @param gameSlate "Main"
#' @param pffAdjPtsAll T/F
#'
#' @return players to lock
#' @export
#'
#' @examples nflLocks(modelWeek=11,modelYear=2018,gameSlate="Main",pffAdjPtsAll=TRUE)
nflLocks<-function(modelWeek=11,modelYear=2018,gameSlate="Main",pffAdjPtsAll=TRUE){
  allLs<-list.dirs("~/NFL_Daily/dataSets") %>% data.frame()
  qbs1<-allLs[grep(pattern = paste0("dataSets/",modelYear,"/",modelWeek,"/",gameSlate,"/QB"),x = allLs$.),]
  rbs1<-allLs[grep(pattern = paste0("dataSets/",modelYear,"/",modelWeek,"/",gameSlate,"/RB"),x = allLs$.),]
  wrs1<-allLs[grep(pattern = paste0("dataSets/",modelYear,"/",modelWeek,"/",gameSlate,"/WR"),x = allLs$.),]
  tes1<-allLs[grep(pattern = paste0("dataSets/",modelYear,"/",modelWeek,"/",gameSlate,"/TE"),x = allLs$.),]
  dsts1<-allLs[grep(pattern = paste0("dataSets/",modelYear,"/",modelWeek,"/",gameSlate,"/DST"),x = allLs$.),]

  #QB
  qb<-read.csv(file=paste0(qbs1[1],"/stats.csv")) %>% dplyr::select(Player_Name,Site_Salary,OppPlusMinus,ProTrends_DK,Ceiling,ProjPlusMinus,PtVal,Score,Salary,Pts,ActualPoints,Team.x)
  qbCeiling<-round((qb$Ceiling*99)/max(qb$Ceiling),0)
  qb<-qb %>% dplyr::mutate("CeilingPct"=qbCeiling) %>% dplyr::select(-Ceiling) %>% unique() %>% na.What()

  qbMinFilterValues<- read.csv(file="~/NFL_Daily/qb2018HighsMeansSd.csv") %>% dplyr::select(-X)
  qbActualPtsFilter<-qbMinFilterValues[which(qbMinFilterValues$variable=="ActualPoints"),] %>% rownames()
  #filter out variables not used in loop
  qbFilterValuesDf<-qbMinFilterValues %>% dplyr::filter(variable!="ActualPoints",variable!="Salary")
  qbFiltered<-foreach(i=1:nrow(qbFilterValuesDf),.combine = rbind) %do% {
   # if(dim(qbF<-qb %>% dplyr::filter(qbFilterValuesDf[i,"variable"]!>=qbFilterValuesDf[i,"Mean"]))[1][1]==0){
    varqb<-qbFilterValuesDf[i,"variable"]
    qbF<-qb[qb[,varqb]>=qbFilterValuesDf[i,"Mean"],]
    }
  qbFiltered<-qbFiltered %>% dplyr::select(Player_Name,ActualPoints,ProjPlusMinus,Salary,Pts,Team.x) %>% dplyr::arrange(desc(ProjPlusMinus))
  qbFiltered<-qbFiltered %>% dplyr::group_by(Player_Name) %>% dplyr::add_count() %>% dplyr::filter(n==nrow(qbFilterValuesDf)) %>% unique() %>% dplyr::select(-n) %>% data.frame()
  names(qbFiltered)<-c(names(qbFiltered)[1:5],"Team")

  qbFiltered
  write.csv(qbFiltered,file=paste0("~/NFL_Daily/dfsProjections/","QB/",modelYear,"/",modelWeek,"/qbLabLocks.csv"))


  #RB
  rb<-read.csv(file=paste0(rbs1[1],"/stats.csv")) %>% dplyr::select(Player_Name,Site_Salary,OppPlusMinus,ProTrends_DK,Ceiling,ProjPlusMinus,PtVal,Score,Salary,Pts,ActualPoints,Team.x)
  rbCeiling<-round((rb$Ceiling*99)/max(rb$Ceiling),0)
  rb<-rb %>% dplyr::mutate("CeilingPct"=rbCeiling) %>% dplyr::select(-Ceiling) %>% unique() %>% na.What()

  rbMinFilterValues<- read.csv(file="~/NFL_Daily/rb2018HighsMeansSd.csv") %>% dplyr::select(-X)
  rbActualPtsFilter<-rbMinFilterValues[which(rbMinFilterValues$variable=="ActualPoints"),] %>% rownames()
  #filter out variables not used in loop
  rbFilterValuesDf<-rbMinFilterValues %>% dplyr::filter(variable!="ActualPoints",variable!="Site_Salary")
  rbFiltered<-foreach(i=1:nrow(rbFilterValuesDf),.combine = rbind) %do% {
    # if(dim(rbF<-rb %>% dplyr::filter(rbFilterValuesDf[i,"variable"]!>=rbFilterValuesDf[i,"Mean"]))[1][1]==0){
    varrb<-rbFilterValuesDf[i,"variable"]
    rbF<-rb[rb[,varrb]>=rbFilterValuesDf[i,"Mean"],]
  }
  rbFiltered<-rbFiltered %>% dplyr::select(Player_Name,ActualPoints,CeilingPct,PtVal,Salary,Team.x) %>% dplyr::arrange(desc(CeilingPct))
  rbFiltered<-rbFiltered %>% dplyr::group_by(Player_Name) %>% dplyr::add_count() %>% dplyr::filter(n==nrow(rbFilterValuesDf)) %>% unique() %>% dplyr::select(-n) %>% data.frame()

  names(rbFiltered)<-c(names(rbFiltered)[1:5],"Team")
  rbFiltered
  write.csv(rbFiltered,file=paste0("~/NFL_Daily/dfsProjections/","RB/",modelYear,"/",modelWeek,"/rbLabLocks.csv"))


  #WR
  wr<-read.csv(file=paste0(wrs1[1],"/stats.csv")) %>% dplyr::select(Player_Name,Site_Salary,OppPlusMinus,ProTrends_DK,Ceiling,ProjPlusMinus,PtVal,Score,Salary,Pts,ActualPoints,Team.x)
  wrCeiling<-round((wr$Ceiling*99)/max(wr$Ceiling),0)
  wr<-wr %>% dplyr::mutate("CeilingPct"=wrCeiling) %>% dplyr::select(-Ceiling) %>% unique() %>% na.What()

  wrMinFilterValues<- read.csv(file="~/NFL_Daily/wr2018HighsMeansSd.csv") %>% dplyr::select(-X)
  wrActualPtsFilter<-wrMinFilterValues[which(wrMinFilterValues$variable=="ActualPoints"),] %>% rownames()
  #filter out variables not used in loop
  wrFilterValuesDf<-wrMinFilterValues %>% dplyr::filter(variable!="ActualPoints",variable!="Site_Salary",variable!="ProjPlusMinus",variable!="OppPlusMinus",variable!="PtVal")
  wrFiltered<-foreach(i=1:nrow(wrFilterValuesDf),.combine = rbind) %do% {
    # if(dim(wrF<-wr %>% dplyr::filter(wrFilterValuesDf[i,"variable"]!>=wrFilterValuesDf[i,"Mean"]))[1][1]==0){
    varwr<-wrFilterValuesDf[i,"variable"]
    wrF<-wr[wr[,varwr]>=wrFilterValuesDf[i,"Mean"],]
  }
  wrFiltered2<-wr[wr[,"PtVal"]>=1.94,]
  wrFiltered<-bind_rows(wrFiltered,wrFiltered2)
  wrFiltered<-wrFiltered %>% dplyr::select(Player_Name,ActualPoints,Salary,CeilingPct,PtVal,Team.x) %>% dplyr::arrange(desc(Salary))
  wrFiltered<-wrFiltered %>% dplyr::group_by(Player_Name) %>% dplyr::add_count() %>% dplyr::filter(n>=nrow(wrFilterValuesDf)) %>% unique() %>% dplyr::select(-n) %>% data.frame()
  wrFiltered
  names(wrFiltered)<-c(names(wrFiltered)[1:5],"Team")

  write.csv(wrFiltered,file=paste0("~/NFL_Daily/dfsProjections/","WR/",modelYear,"/",modelWeek,"/wrLabLocks.csv"))


  #TE
  te<-read.csv(file=paste0(tes1[1],"/stats.csv")) %>% dplyr::select(Player_Name,Site_Salary,OppPlusMinus,ProTrends_DK,Ceiling,PtVal,Score,Salary,Pts,ActualPoints,Team.x)
  teCeiling<-round((te$Ceiling*99)/max(te$Ceiling),0)
  te<-te %>% dplyr::mutate("CeilingPct"=teCeiling) %>% dplyr::select(-Ceiling) %>% unique() %>% na.What()

  teMinFilterValues<- read.csv(file="~/NFL_Daily/te2018HighsMeansSd.csv") %>% dplyr::select(-X)
  teActualPtsFilter<-teMinFilterValues[which(teMinFilterValues$variable=="ActualPoints"),] %>% rownames()
  #filter out variables not used in loop
  teFilterValuesDf<-teMinFilterValues %>% dplyr::filter(variable!="ActualPoints",variable!="Salary",variable!="ProTrends_DK",variable!="Site_Salary",variable!="OppPlusMinus")
  teFiltered<-foreach(i=1:nrow(teFilterValuesDf),.combine = rbind) %do% {
    # if(dim(teF<-te %>% dplyr::filter(teFilterValuesDf[i,"variable"]!>=teFilterValuesDf[i,"Mean"]))[1][1]==0){
    varte<-teFilterValuesDf[i,"variable"]
    teF<-te[te[,varte]>=teFilterValuesDf[i,"Mean"],]
  }
  teFiltered2<-te[te[,"ProTrends_DK"]>=0,]
  teFiltered<-bind_rows(teFiltered,teFiltered2)
  teFiltered<-teFiltered %>% dplyr::select(Player_Name,ActualPoints,CeilingPct,Salary,PtVal,Team.x) %>% dplyr::arrange(desc(CeilingPct))
  teFiltered<-teFiltered %>% dplyr::group_by(Player_Name) %>% dplyr::add_count() %>% dplyr::filter(n>=nrow(teFilterValuesDf)) %>% unique() %>% dplyr::select(-n) %>% data.frame()
  names(teFiltered)<-c(names(teFiltered)[1:5],"Team")

  teFiltered
  write.csv(teFiltered,file=paste0("~/NFL_Daily/dfsProjections/","TE/",modelYear,"/",modelWeek,"/teLabLocks.csv"))


  #DST
  dst<-read.csv(file=paste0(dsts1[1],"/stats.csv")) %>% dplyr::select(Player_Name,Site_Salary,OppPlusMinus,ProTrends_DK,Ceiling,ProjPlusMinus,PtVal,Score,Salary,Pts,ActualPoints,Team.x)
  dstCeiling<-round((dst$Ceiling*99)/max(dst$Ceiling),0)
  dst<-dst %>% dplyr::mutate("CeilingPct"=dstCeiling) %>% dplyr::select(-Ceiling) %>% unique() %>% na.What()

  dstMinFilterValues<- read.csv(file="~/NFL_Daily/dst2018HighsMeansSd.csv") %>% dplyr::select(-X)
  dstActualPtsFilter<-dstMinFilterValues[which(dstMinFilterValues$variable=="ActualPoints"),] %>% rownames()
  #filter out variables not used in loop
  dstFilterValuesDf<-dstMinFilterValues %>% dplyr::filter(variable!="ActualPoints",variable!="Site_Salary",variable!="ProjPlusMinus",variable!="PtVal",variable!="Pts",variable!="OppPlusMinus",variable!="CeilingPct")
  dstFiltered<-foreach(i=1:nrow(dstFilterValuesDf),.combine = rbind) %do% {
    # if(dim(dstF<-dst %>% dplyr::filter(dstFilterValuesDf[i,"variable"]!>=dstFilterValuesDf[i,"Mean"]))[1][1]==0){
    vardst<-dstFilterValuesDf[i,"variable"]
    dstF<-dst[dst[,vardst]>=dstFilterValuesDf[i,"Mean"],]
  }
  dstFiltered2<-dst[dst[,"CeilingPct"]>=55.9,]
  dstFiltered<-bind_rows(dstFiltered,dstFiltered2)
  dstFiltered<-dstFiltered %>% dplyr::select(Player_Name,ActualPoints,Salary,Score,ProTrends_DK,Team.x) %>% dplyr::arrange(desc(Salary))
  dstFiltered<-dstFiltered %>% dplyr::group_by(Player_Name) %>% dplyr::add_count() %>% dplyr::filter(n>nrow(dstFilterValuesDf)) %>% unique() %>% dplyr::select(-n) %>% data.frame()
  names(dstFiltered)<-c(names(dstFiltered)[1:5],"Team")
   dstFiltered
  write.csv(dstFiltered,file=paste0("~/NFL_Daily/dfsProjections/","DST/",modelYear,"/",modelWeek,"/dstLabLocks.csv"))

  if(pffAdjPtsAll==TRUE){
    pffAdjPtsAllDf<-read.csv(file=paste0("~/NFL_Daily/dfsProjections/",modelYear,"/",modelWeek,"/adjPointsAllowed.csv"))
    names(pffAdjPtsAllDf)<-c(names(pffAdjPtsAllDf)[1:2],"qb","rb","wr","te",names(pffAdjPtsAllDf)[7:length(pffAdjPtsAllDf)])
    pffAdjPtsAllDf[pffAdjPtsAllDf$Tm == "ARZ", "Tm"] <- "ARI"
    pffAdjPtsAllDf[pffAdjPtsAllDf$Tm == "BLT", "Tm"] <- "BAL"
    pffAdjPtsAllDf[pffAdjPtsAllDf$Tm == "CLV", "Tm"] <- "CLE"
    pffAdjPtsAllDf[pffAdjPtsAllDf$Tm == "HST", "Tm"] <- "HOU"
    qbPlus<-grep(pattern = "+",x = pffAdjPtsAllDf$qb,fixed = TRUE)
    rbPlus<-grep(pattern = "+",x = pffAdjPtsAllDf$rb,fixed = TRUE)
    wrPlus<-grep(pattern = "+",x = pffAdjPtsAllDf$wr,fixed = TRUE)
    tePlus<-grep(pattern = "+",x = pffAdjPtsAllDf$te,fixed = TRUE)
    qbPlusDf<-qb %>% dplyr::filter(Team.x %in% pffAdjPtsAllDf[qbPlus,"Tm"])
    rbPlusDf<-rb %>% dplyr::filter(Team.x %in% pffAdjPtsAllDf[rbPlus,"Tm"])
    wrPlusDf<-wr %>% dplyr::filter(Team.x %in% pffAdjPtsAllDf[wrPlus,"Tm"])
    tePlusDf<-te %>% dplyr::filter(Team.x %in% pffAdjPtsAllDf[tePlus,"Tm"])
    write.csv(qbPlusDf,file=paste0("~/NFL_Daily/dfsProjections/","QB/",modelYear,"/",modelWeek,"/qbPffLocks.csv"))
    write.csv(rbPlusDf,file=paste0("~/NFL_Daily/dfsProjections/","RB/",modelYear,"/",modelWeek,"/rbPffLocks.csv"))
    write.csv(wrPlusDf,file=paste0("~/NFL_Daily/dfsProjections/","WR/",modelYear,"/",modelWeek,"/wrPffLocks.csv"))
    write.csv(tePlusDf,file=paste0("~/NFL_Daily/dfsProjections/","TE/",modelYear,"/",modelWeek,"/tePffLocks.csv"))
    qbFLP<-qbPlusDf[qbPlusDf$Player_Name %in% qbFiltered$Player_Name,] %>% dplyr::arrange(desc(ProjPlusMinus))
    rbFLP<-rbPlusDf[rbPlusDf$Player_Name %in% rbFiltered$Player_Name,] %>% dplyr::arrange(desc(CeilingPct))
    wrFLP<-wrPlusDf[wrPlusDf$Player_Name %in% wrFiltered$Player_Name,] %>% dplyr::arrange(desc(Salary))
    teFLP<-tePlusDf[tePlusDf$Player_Name %in% teFiltered$Player_Name,] %>% dplyr::arrange(desc(CeilingPct))
    write.csv(qbFLP,file=paste0("~/NFL_Daily/dfsProjections/","QB/",modelYear,"/",modelWeek,"/qbFinalLocks.csv"))
    write.csv(rbFLP,file=paste0("~/NFL_Daily/dfsProjections/","RB/",modelYear,"/",modelWeek,"/rbFinalLocks.csv"))
    write.csv(wrFLP,file=paste0("~/NFL_Daily/dfsProjections/","WR/",modelYear,"/",modelWeek,"/wrFinalLocks.csv"))
    write.csv(teFLP,file=paste0("~/NFL_Daily/dfsProjections/","TE/",modelYear,"/",modelWeek,"/teFinalLocks.csv"))
    }
return(list("qb"=qbFLP,"rb"=rbFLP,"wr"=wrFLP,"te"=teFLP,"dst"=dstFiltered))

}
