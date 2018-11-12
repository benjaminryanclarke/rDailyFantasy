#' labs dateRange Formatting Function
#'
#' @param yyyymmdd "2018/03/29"
#' @param days any# i.e:44
#'
#' @return list of dates for use
#' @export
#'
#' @examples dateRnageForm("2018/03/29",days=33)
dateRangeForm<-function(yyyymmdd="2018/03/29",days=33){

mlbDates<- seq(as.Date(yyyymmdd), by = "day", length.out = days) %>% data.frame() %>% tidyr::separate(col = ".",into=c("Year","Month","Day"),sep = "-")
mlbYear<-mlbDates$Year
mlbMonth<-gsub(pattern = "^0+","",mlbDates$Month)
mlbDay<-gsub(pattern = "^0+","",mlbDates$Day)
reformedDates<-paste0(mlbMonth,"_",mlbDay,"_",mlbYear) %>% data.frame(stringsAsFactors = FALSE)
datesOut<-c(reformedDates)
return(datesOut$.)

}
