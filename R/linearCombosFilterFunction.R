#' lcs removal
#'
#' @param x data
#'
#' @return dataframe
#' @export
#'
#' @examples combos.Filter(bats)
combos.Filter<-suppressWarnings(function(x){
  suppressMessages(correl<-cor(x))
  cor.data<-caret::findLinearCombos(na.zero(correl))
  cor.x<-x[,-cor.data$remove]
  return(cor.x)
})
