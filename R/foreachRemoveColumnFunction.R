#' remove Columns Function
#'
#' @param d dataframe
#' @param r pattern to match in column names to be removed
#'
#' @return subbed dF
#' @export
#'
#' @examples foreach RemoveColumn(d=nba1026_31,r = "|custom")
foreachRemoveColumn<-function(d=nba1026_31,r = "|custom"){
dd<-foreach(j=1:length(d)) %do% if(length(grep(pattern = r,x = names(d[[j]]),fixed = TRUE)) != 0){
  removeColumn(dat = d[[j]],remove = r)}else{
  d[[j]]
}

return(dd)
}
