nflOptimizer<-function(modelYear=2018,modelWeek=7,gameSlate="Main",projectionFile="ridgetheMoneyMaker",make=20){

  require(coach)
  #get draftkingsEntries BulkUpload CSV file
  draftGroupDf<-read.csv(file=paste0("~/NFL_Daily/sourceData/",modelYear,"/",modelWeek,"/",gameSlate,"/source.csv")) %>% dplyr::select(DraftGroupId)
  draftGroup<-draftGroupDf[[1]]
  hmDir<-getwd()
  setwd(paste0("~/NFL_Daily/draftkingsData/",modelYear,"/",modelWeek))
  curlAddress<-paste0("curl -L https://www.draftkings.com/bulklineup/getdraftablecsv?draftGroupId=",draftGroup," -o DKSalaries",gameSlate,".csv")
  system(command = curlAddress)
  dkData<-read.csv(file=paste0("DKSalaries",gameSlate,".csv"),quote = "",skip = 7)[,11:19]
  names(dkData)<-c("Position","Name + ID","Name","ID","Roster Position","Salary","Game Info","TeamAbbrev","AvgPointsPerGame")
  write.csv(dkData,file=paste0("DKSalaries",gameSlate,"Fixed.csv"))
  #cleanup/tidy for optimization
  dk<-coach::read_dk(paste0("DKSalaries",gameSlate,"Fixed.csv")) %>% dplyr::select(-fpts_proj)
  nfl<-read.csv(paste0("~/NFL_Daily/dfsProjections/combined/",modelYear,"/",modelWeek,"/",gameSlate,"_",projectionFile,".csv")) %>% dplyr::select(-X)
  playerPros<-data.frame(nfl$Player_Name,nfl$pffProj) %>% setNames(c("player","fpts_proj"))
  dkData<-left_join(dk,playerPros,by="player")
  #### add more shit when I get the chance from playerPros model.
  nfl_opt<-model_dk_nfl(dkData)
  lineups<-optimize_generic(na.What(dkData),model=nfl_opt,L = make)
  lineupsNorm<-foreach(a=1:make) %do% coach::normalize_lineup(lineups[[a]],site="draftkings",sport="nfl")
  write_lineups(lineupsNorm,path=paste0("~/NFL_Daily/draftkingsData/",modelYear,"/",modelWeek,"/",gameSlate,modelYear,"_",modelWeek,".csv"))


}
