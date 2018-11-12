#' mlbBatsImportForMachineLearningFunction
#'
#' @param firstDate "yyyy/mm/dd"
#' @param number number of days to research
#' @param slateName game time to analyze over period of days researched
#' @param refreshData get post game data where needed..True/False.. defaults as FALSE
#'
#' @return 4 dataframes to use for machine learning
#' @export
#'
#' @examples mlbBatsImportForLearning("2018/04/18",number=43,slateName="Main",refreshData=FALSE)
mlbBatsImportForLearning<- function(firstDate="2018/04/18",number=43,slateName="Main",refreshData=FALSE){

if(refreshData==TRUE){
  mlbCoreFun(dateV = dateRangeForm(firstDate,days = number),labModel = "mainBae",style = "research")
}

gameDates <- dateRangeForm(firstDate, days = number)
allBats<-foreach(i=1:number) %do% read.csv(file=paste0("~/MLB_Daily/dataSets/",gameDates[i],"/",slateName,"/hitters/stats.csv"),stringsAsFactors = F)
allBats<-dplyr::bind_rows(allBats)
#bats.Filter<-data.frame(allBats) %>% dplyr::select("RunsP","Opp_RunsP","Score","Salary","Lineup_Order","AvgPts","Ceiling","Floor","ProjPlusMinus","PtVal","ActualPoints","wOBA_Split","wOBA_Diff","ISO_Split","ISO_SplitDiff","SLG_Split","SOPerAB_Split","HRPerAB","SB_Per_Game","OppBullpenRating","OppSOPerABAvg","OppWOBAAvg","Trend","OfficialPlusMinus","Site_Salary","so_pred","park_f","Run_Change","Moneyline","Total","Moneyline_Pct","WeatherRatingAlt","Temperature","Wind_Speed","PrecipProb","Humidity","cnt_r","distance_r","ev_r","fb_r","gb_r","ld_r","hh_r","distance_diff","evd","hh_diff","BattedBallLuck","airtime_r","cnt_l","distance_l","ev_y","fb_y","gb_y","ld_y","hh_l","gbtofb_y","airtime_l","OppWobaAllowed","OppIsoAllowed","OppWobaAllowed15","OppIsoAllowed15","Season_PPG","Season_Salary_Change","Season_Plus_Minus","Season_X1","Season_X2","Season_X0","Season_Count","Month_PPG","Month_Salary_Change","Month_X1","Month_X2","Month_X0","Month_Count","sb_pct","so_pred_pct","ProjPct","PtPerDPct","FloorPct","ProjPlusMinusPct","CeilingPct","gb_pct","Vegas","VegasPercentile_Slate","Salary_Movement","SalaryChangePercentile_Slate","OwnRank","OwnRank_Slate","Upside","Consistency","Season_PPG_Percentile","Pro_Pct","ProTrendsPercentile_Slate","SiteDiffPercentile_Slate","Plus_Minus","OppSOPerABPct","OppWobaTeamPct","OppTeamEVPct15","DistanceDiffPct","ExitVelocityPct15","BattedBallLuckPct","IPPct","OppIsoAllowedPct15","HHDiffPct","OppHHPct15","HHPct15","EvDPct","OppDistDiffPct","OppTeamHHPct15","DistPct15","OppWobaAllowedPct15","OppEVPct15","OppPVPct15","OppTeamDistPct15","wOBA_Percentile","wOBADiffPercentile_Slate","OppSOPer9Pct","AltWHIP_Pct","ISO_MTH_PCT","WOBA_MTH_PCT","LineupPercentile_Slate","ISO_Pct","IsoDiffPercentile_Slate","p_own","PointsPlusMinus","HR","SB")
#bats.FilterActual<- bats.Filter %>% select(-PointsPlusMinus,-HR,-SB) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()
#bats.FilterPlusMinus<- bats.Filter %>% select(-ActualPoints,-HR,-SB) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()
#bats.FilterHR<- bats.Filter %>% select(-PointsPlusMinus,-ActualPoints,-SB) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()
#bats.FilterSB<- bats.Filter %>% select(-PointsPlusMinus,-HR,-ActualPoints) %>% na.What(what = 0) %>% sapply(as.numeric) %>% as.data.frame()

bats.Filter<-data.frame(allBats) %>% dplyr::select("RunsP","Opp_RunsP","Score","Salary","Lineup_Order","AvgPts","Ceiling","Floor","ProjPlusMinus","PtVal","ActualPoints","wOBA_Split","wOBA_Diff","ISO_Split","ISO_SplitDiff","SLG_Split","SOPerAB_Split","HRPerAB","SB_Per_Game","OppBullpenRating","OppSOPerABAvg","OppWOBAAvg","Trend","OfficialPlusMinus","Site_Salary","so_pred","park_f","Run_Change","Moneyline","Total","Moneyline_Pct","WeatherRatingAlt","Temperature","Wind_Speed","PrecipProb","Humidity","cnt_r","distance_r","ev_r","fb_r","gb_r","ld_r","hh_r","distance_diff","evd","hh_diff","BattedBallLuck","airtime_r","cnt_l","distance_l","ev_y","fb_y","gb_y","ld_y","hh_l","gbtofb_y","airtime_l","OppWobaAllowed","OppIsoAllowed","OppWobaAllowed15","OppIsoAllowed15","Season_PPG","Season_Salary_Change","Season_Plus_Minus","Season_X1","Season_X2","Season_X0","Season_Count","Month_PPG","Month_Salary_Change","Month_X1","Month_X2","Month_X0","Month_Count","sb_pct","so_pred_pct","ProjPct","PtPerDPct","FloorPct","ProjPlusMinusPct","CeilingPct","gb_pct","Vegas","VegasPercentile_Slate","Salary_Movement","SalaryChangePercentile_Slate","OwnRank","OwnRank_Slate","Upside","Consistency","Season_PPG_Percentile","Pro_Pct","ProTrendsPercentile_Slate","SiteDiffPercentile_Slate","Plus_Minus","OppSOPerABPct","OppWobaTeamPct","OppTeamEVPct15","DistanceDiffPct","ExitVelocityPct15","BattedBallLuckPct","IPPct","OppIsoAllowedPct15","HHDiffPct","OppHHPct15","HHPct15","EvDPct","OppDistDiffPct","OppTeamHHPct15","DistPct15","OppWobaAllowedPct15","OppEVPct15","OppPVPct15","OppTeamDistPct15","wOBA_Percentile","wOBADiffPercentile_Slate","OppSOPer9Pct","AltWHIP_Pct","ISO_MTH_PCT","WOBA_MTH_PCT","LineupPercentile_Slate","ISO_Pct","IsoDiffPercentile_Slate","p_own","PointsPlusMinus","HR","SB")
bats.FilterActual<- bats.Filter %>% select(-PointsPlusMinus,-HR,-SB) %>% sapply(as.numeric) %>% as.data.frame()
bats.FilterPlusMinus<- bats.Filter %>% select(-ActualPoints,-HR,-SB) %>% sapply(as.numeric) %>% as.data.frame()
bats.FilterHR<- bats.Filter %>% select(-PointsPlusMinus,-ActualPoints,-SB) %>% sapply(as.numeric) %>% as.data.frame()
bats.FilterSB<- bats.Filter %>% select(-PointsPlusMinus,-HR,-ActualPoints) %>% sapply(as.numeric) %>% as.data.frame()

return(list(actual=bats.FilterActual,plusMinus=bats.FilterPlusMinus,homeRuns=bats.FilterHR,stolenBases=bats.FilterSB))

}
