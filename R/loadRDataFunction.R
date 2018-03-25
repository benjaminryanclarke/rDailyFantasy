#' load RData saved objects function
#'
#' @param fileName mlbHitterAdvancedLm1
#'
#' @return inside of saved files
#' @export
#'
#' @examples loadRData("~/Desktop/MLB_Daily/mlbHitterAdvancedLm1.rda")
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

