#' nearZeroVariance predictors function filter
#'
#' @param x data.frame of predictors
#'
#' @return predictors minus zeroVariance predictors from orginal set
#' @export
#'
#' @examples nzv.filer(x=bats[,1:160])
nzv.filter<-function(x){
  nzv<-nearZeroVar(x)
  nzvFilter<-x[,-nzv]
  return(nzvFilter)
}
