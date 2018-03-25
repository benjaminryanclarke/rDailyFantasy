#' NA to Zero
#'
#' @param x data
#' @param what what to change NAs to
#'
#' @return no NA Dataframe x
#' @export
#'
#' @examples na.zero(x,1)
na.What <- function (x,what=0) {
  x[is.na(x)] <- what
  return(x)
}
