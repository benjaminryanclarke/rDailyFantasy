#' Numeric Only DataFrame
#'
#' @param x
#'
#' @return data frame of numeric only type
#' @export
#'
#' @examples dfNums <- numericOnly(dF)
numericOnly <- function(x){
  x<- x[sapply(x,is.numeric)]
  return(x)
}
