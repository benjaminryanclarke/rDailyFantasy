#' remove Column
#'
#' @param dat dataframe
#' @param remove part of name of column to remove
#'
#' @return dataframe without unwanted columns
#' @export
#'
#' @examples removeColumn(dat=nba1026_31[[4]],remove="|custom")
removeColumn<-function(dat=nba1026_31[[4]],remove="|custom"){
removeCol<-grep(pattern = remove,x = names(dat),fixed = TRUE)
dt<-dat[,-removeCol]
return(dt)
}
