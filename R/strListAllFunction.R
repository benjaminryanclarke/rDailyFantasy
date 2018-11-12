#' str Function
#'
#' @param x data
#'
#' @return str w/ no truncation
#' @export
#'
#' @examples strListAll(df)
strListAll<-function(x){
  strList<-str(x,list.len=ncol(x))
}
