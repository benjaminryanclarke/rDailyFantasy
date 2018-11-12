#' mlbArmsImportForMachineLearningFunction
#'
#' @param firstDate "yyyy/mm/dd"
#' @param number number of days to research
#' @param slateName game time to analyze over period of days researched
#' @param refreshData get post game data where needed..True/False.. defaults as FALSE
#'
#' @return 4 dataframes to use for machine learning
#' @export
#'
#' @examples mlbArmsImportForLearning("2018/04/18",number=43,slateName="Main",refreshData=FALSE)
mlbArmsImportForLearning<- function(firstDate="2018/04/18",number=43,slateName="Main",refreshData=FALSE){

  if(refreshData==TRUE){
    mlbCoreFun(dateV = dateRangeForm(firstDate,days = number),labModel = "mainBae",style = "research")
  }

  gameDates <- dateRangeForm(firstDate, days = number)
  allArms<-foreach(i=1:number) %do% read.csv(file=paste0("~/MLB_Daily/dataSets/",gameDates[i],"/",slateName,"/pitchers/stats.csv"),stringsAsFactors = F)
  allArms<-dplyr::bind_rows(allArms)
  #Arms.Filter<-data.frame(allArms) %>% dplyr::select("RunsP","Opp_RunsP","Score","Salary","Lineup_Order","AvgPts","Ceiling","Floor","ProjPlusMinus","PtVal","ActualPoints","wOBA_Split","wOBA_Diff","ISO_Split","ISO_SplitDiff","SLG_Split","SOPerAB_Split","HRPerAB","SB_Per_Game","OppBullpenRating","OppSOPerABAvg","OppWOBAAvg","Trend","OfficialPlusMinus","Site_Salary","so_pred","park_f","Run_Change","Moneyline","Total","Moneyline_Pct","WeatherRatingAlt","Temperature","Wind_Speed","PrecipProb","Humidity","cnt_r","distance_r","ev_r","fb_r","gb_r","ld_r","hh_r","distance_diff","evd","hh_diff","BattedBallLuck","airtime_r","cnt_l","distance_l","ev_y","fb_y","gb_y","ld_y","hh_l","gbtofb_y","airtime_l","OppWobaAllowed","OppIsoAllowed","OppWobaAllowed15","OppIsoAllowed15","Season_PPG","Season_Salary_Change","Season_Plus_Minus","Season_X1","Season_X2","Season_X0","Season_Count","Month_PPG","Month_Salary_Change","Month_X1","Month_X2","Month_X0","Month_Count","sb_pct","so_pred_pct","ProjPct","PtPerDPct","FloorPct","ProjPlusMinusPct","CeilingPct","gb_pct","Vegas","VegasPercentile_Slate","Salary_Movement","SalaryChangePercentile_Slate","OwnRank","OwnRank_Slate","Upside","Consistency","Season_PPG_Percentile","Pro_Pct","ProTrendsPercentile_Slate","SiteDiffPercentile_Slate","Plus_Minus","OppSOPerABPct","OppWobaTeamPct","OppTeamEVPct15","DistanceDiffPct","ExitVelocityPct15","BattedBallLuckPct","IPPct","OppIsoAllowedPct15","HHDiffPct","OppHHPct15","HHPct15","EvDPct","OppDistDiffPct","OppTeamHHPct15","DistPct15","OppWobaAllowedPct15","OppEVPct15","OppPVPct15","OppTeamDistPct15","wOBA_Percentile","wOBADiffPercentile_Slate","OppSOPer9Pct","AltWHIP_Pct","ISO_MTH_PCT","WOBA_MTH_PCT","LineupPercentile_Slate","ISO_Pct","IsoDiffPercentile_Slate","p_own","PointsPlusMinus","HR","SB")
  #Arms.FilterActual<- Arms.Filter %>% select(-PointsPlusMinus,-HR,-SB) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()
  #Arms.FilterPlusMinus<- Arms.Filter %>% select(-ActualPoints,-HR,-SB) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()
  #Arms.FilterHR<- Arms.Filter %>% select(-PointsPlusMinus,-ActualPoints,-SB) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()
  #Arms.FilterSB<- Arms.Filter %>% select(-PointsPlusMinus,-HR,-ActualPoints) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()

  Arms.Filter<-data.frame(allArms) %>% dplyr::select("RunsP","Opp_RunsP","Score","Salary","Lineup_Order","AvgPts","Ceiling","Floor","ProjPlusMinus","PtVal","ImpPts","ActualPoints","wOBA_Split","wOBA_Diff","ISO_SplitDiff","SLG_Split","SOPerAB_Split","Starter_WHIP","HR_Per_9","SO_Per_9","OppBullpenRating","IP","QualityStartsPerStart","OppSOPerABAvg","OppWOBAAvg","Trend","OfficialPlusMinus","Site_Salary","so_pred","park_f","Run_Change","Moneyline","Total","Moneyline_Pct","WeatherRatingAlt","Temperature","Wind_Speed","PrecipProb","Humidity","cnt_r","distance_r","ev_r","fb_r","gb_r","ld_r","hh_r","velocity_r","strball_r","distance_diff","evd","PitchCount15","hh_diff","pitch_diff","BattedBallLuck","airtime_r","cnt_l","distance_l","ev_y","fb_y","gb_y","ld_y","hh_l","gbtofb_y","velocity_l","strball_l","PitchCount","airtime_l","OppWobaAllowed","OppIsoAllowed","OppWobaAllowed15","OppIsoAllowed15","Season_PPG","Season_Salary_Change","Season_Plus_Minus","Season_X1","Season_X2","Season_X0","Season_Count","Month_PPG","Month_Salary_Change","Month_X1","Month_X2","Month_X0","Month_Count","so_pred_pct","ProjPct","PtPerDPct","FloorPct","ProjPlusMinusPct","CeilingPct","PitchCountPct","PitchCountPct15","pitch_diff_pct","gb_pct","Vegas","VegasPercentile_Slate","Salary_Movement","SalaryChangePercentile_Slate","OwnRank","OwnRank_Slate","Upside","Consistency","Season_PPG_Percentile","Pro_Pct","ProTrendsPercentile_Slate","SiteDiffPercentile_Slate","Plus_Minus","SO_Per_9Pct","OppSOPerABPct","OppWobaTeamPct","OppTeamEVPct15","IsoAllowedPct15","DistanceDiffPct","StrikePct15","ExitVelocityPct15","BattedBallLuckPct","IPPct","OppIsoAllowedPct15","QualityStartsPerStartPct","WobaAllowedPct15","HHDiffPct","OppHHPct15","HHPct15","EvDPct","OppDistDiffPct","OppTeamHHPct15","DistPct15","OppWobaAllowedPct15","PVPct15","OppEVPct15","OppPVPct15","OppTeamDistPct15","wOBA_Percentile","wOBADiffPercentile_Slate","OppSOPer9Pct","AltWHIP_Pct","WHIP_Pct","WOBA_MTH_PCT","LineupPercentile","LineupPercentile_Slate","ISO_Pct","IsoDiffPercentile_Slate","p_own","PointsPlusMinus","PitchW","PitchSO")
  Arms.FilterActual<- Arms.Filter %>% select(-PointsPlusMinus,-PitchW,-PitchSO) %>% sapply(as.numeric) %>% as.data.frame()
  Arms.FilterPlusMinus<- Arms.Filter %>% select(-ActualPoints,-PitchW,-PitchSO) %>% sapply(as.numeric) %>% as.data.frame()
  Arms.FilterW<- Arms.Filter %>% select(-PointsPlusMinus,-ActualPoints,-PitchSO) %>% sapply(as.numeric) %>% as.data.frame()
  Arms.FilterSO<- Arms.Filter %>% select(-PointsPlusMinus,-PitchW,-ActualPoints) %>% sapply(as.numeric) %>% as.data.frame()

  return(list(actual=Arms.FilterActual,plusMinus=Arms.FilterPlusMinus,strikeOuts=Arms.FilterSO,win=Arms.FilterW))

}
