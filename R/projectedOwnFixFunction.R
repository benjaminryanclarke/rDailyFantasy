#' player Own Labs Model Fix [col]
#'
#' @param x dataframe name with p_own variable
#'
#' @return fixed ownership predictions
#' @export
#'
#' @examples dataset$Properties.p_own<-projectedOwnFix(dataset)
projectedOwnFix <- function(x){
  minOwn <- data.frame(do.call('rbind', strsplit(as.character(x$Properties.p_own),'-',fixed=TRUE)))
  x$Properties.p_own<-as.numeric(minOwn$X1)
  return(x$Properties.p_own)
}