#' normalize Function
#'
#' @param x 
#'
#' @return normalized vectors
#' @export
#'
#' @examples normalize(x)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}