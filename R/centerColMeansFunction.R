#' colmeans centering
#'
#' @param x data
#'
#' @return mean-centered data for analysis
#' @export
#'
#' @examples center_colmeans(batsOnly)
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}
