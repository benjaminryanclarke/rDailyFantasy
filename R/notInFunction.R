#' Not
#'
#' @param x var1
#' @param y var2
#'
#' @return opposite return of in
#' @export
#'
#' @examples not
"%!in%" <- function(x,y){
  !("%in%"(x,y))
}