#' mlb Bat Projections Function
#'
#' @param pDate gameDate
#' @param gameSlate .."Main","Turbo","Afternoon","Allday","Night"...etc
#' @param batModel the model used to mak projections. Saved in ~/MLB_Daily/machineLearning/hitters/
#' @param labModel name of the fantasyLabs model used in the projections model training...default "mainBae"..6_11_2018
#'
#' @return saves csv file with projections to ~/MLB_Daily/dfsProjections/hitters/ and returns the data.frame in the r console
#' @export
#'
#' @examples mlbBatProjection(pDate="6_10_2018",gameSlate="Main",batModel="svmFit.actual6_11_18",labModel="mainBae")
mlbBatProjection<-function(pDate="6_10_2018",gameSlate="Main",batModel="svmFit.actual6_11_18",labModel="mainBae"){

#projection Model Check
modelCheck<-list(ls())
if(batModel %in% modelCheck[[1]]==FALSE){
   batModel<-rDailyFantasy::loadRData(paste0("~/MLB_Daily/machineLearning/hitters/",batModel,".rda"))
}

#get baseball data
  mlb<-rDailyFantasy::mlbCoreFun(dateV = pDate, labModel = labModel,style = "projections")
  bats<-mlb$bats %>% tidyr::unnest() %>% dplyr::filter(ContestSuffix==gameSlate)
  batNames<- bats$Player_Name
  batsProjForm<- bats %>% dplyr::select("RunsP","Opp_RunsP","Score","Salary","Lineup_Order","AvgPts","Ceiling","Floor","ProjPlusMinus","PtVal","ActualPoints","wOBA_Split","wOBA_Diff","ISO_Split","ISO_SplitDiff","SLG_Split","SOPerAB_Split","HRPerAB","SB_Per_Game","OppBullpenRating","OppSOPerABAvg","OppWOBAAvg","Trend","OfficialPlusMinus","Site_Salary","so_pred","park_f","Run_Change","Moneyline","Total","Moneyline_Pct","WeatherRatingAlt","Temperature","Wind_Speed","PrecipProb","Humidity","cnt_r","distance_r","ev_r","fb_r","gb_r","ld_r","hh_r","distance_diff","evd","hh_diff","BattedBallLuck","airtime_r","cnt_l","distance_l","ev_y","fb_y","gb_y","ld_y","hh_l","gbtofb_y","airtime_l","OppWobaAllowed","OppIsoAllowed","OppWobaAllowed15","OppIsoAllowed15","Season_PPG","Season_Salary_Change","Season_Plus_Minus","Season_X1","Season_X2","Season_X0","Season_Count","Month_PPG","Month_Salary_Change","Month_X1","Month_X2","Month_X0","Month_Count","sb_pct","so_pred_pct","ProjPct","PtPerDPct","FloorPct","ProjPlusMinusPct","CeilingPct","gb_pct","Vegas","VegasPercentile_Slate","Salary_Movement","SalaryChangePercentile_Slate","OwnRank","OwnRank_Slate","Upside","Consistency","Season_PPG_Percentile","Pro_Pct","ProTrendsPercentile_Slate","SiteDiffPercentile_Slate","Plus_Minus","OppSOPerABPct","OppWobaTeamPct","OppTeamEVPct15","DistanceDiffPct","ExitVelocityPct15","BattedBallLuckPct","IPPct","OppIsoAllowedPct15","HHDiffPct","OppHHPct15","HHPct15","EvDPct","OppDistDiffPct","OppTeamHHPct15","DistPct15","OppWobaAllowedPct15","OppEVPct15","OppPVPct15","OppTeamDistPct15","wOBA_Percentile","wOBADiffPercentile_Slate","OppSOPer9Pct","AltWHIP_Pct","ISO_MTH_PCT","WOBA_MTH_PCT","LineupPercentile_Slate","ISO_Pct","IsoDiffPercentile_Slate","p_own") %>% rDailyFantasy::na.What(what = 0)
  projectedBats<-predict(batModel,newdata=batsProjForm)
  batsComplete<- batsProjForm %>% mutate(Player_Name=batNames,projection=projectedBats) %>% data.frame(stringsAsFactors = F)
  batsCompletePitcherWOBADiff<-batsComplete$OppWobaAllowed - batsComplete$OppWobaAllowed15
  batsCompletePitcherISODiff<-batsComplete$OppIsoAllowed - batsComplete$OppIsoAllowed15

  batsComplete2<-batsComplete %>% select(Player_Name,projection,ActualPoints,p_own,HRPerAB,SB_Per_Game,sb_pct,park_f,ProjPlusMinus,PtVal,wOBA_Split,ISO_Split,distance_r,ev_r,hh_r,fb_r,ld_r,gb_r,distance_diff,evd,hh_diff,BattedBallLuck,airtime_r,Vegas,Run_Change,WeatherRatingAlt,Wind_Speed,PrecipProb,Upside,OppWOBAAvg,OppWobaAllowed,OppWobaAllowed15,OppIsoAllowed,OppIsoAllowed15) %>% mutate(Opp_WOBADiff=batsCompletePitcherWOBADiff,OppISODiff=batsCompletePitcherISODiff)

  write.csv(batsComplete2,file = paste0("~/MLB_Daily/dfsProjections/hitters/",pDate,"__",gameSlate,".csv"))

  return(batsComplete2)


}
