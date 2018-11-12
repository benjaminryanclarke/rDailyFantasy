#' labs dateRange Formatting Function
#'
#' @param yyyymmdd "2018/09/2"
#' @param weeks any# i.e:17
#'
#' @return list of dates for use
#' @export
#'
#' @examples dateRangeFormNfl("2018/09/05",weeks=17)
dateRangeFormNfl<-function(yyyymmdd="2014/09/03",weeks=17){

  nflDates<- seq(as.Date(yyyymmdd), by = "week", length.out = weeks) %>% data.frame() %>% tidyr::separate(col = ".",into=c("Year","Month","Day"),sep = "-")
  nflYear<-nflDates$Year
  nflMonth<-gsub(pattern = "^0+","",nflDates$Month)
  nflDay<-gsub(pattern = "^0+","",nflDates$Day)
  reformedDates<-paste0(nflMonth,"_",nflDay,"_",nflYear) %>% data.frame(stringsAsFactors = FALSE)
  datesOut<-c(reformedDates)
  return(datesOut$.)
}
