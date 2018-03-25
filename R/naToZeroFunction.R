#' NAs to 0 function
#'
#' @param x anything to replace NAs with
#'
#' @return no NA Dataframe x
#' @export
#'
#' @examples na.zero(x)
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
