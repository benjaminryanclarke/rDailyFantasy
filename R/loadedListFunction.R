#' get loaded Packages in a dataframe column sorted alphabetically for easy skim thru
#'
#' @return loaded package names
#' @export
#'
#' @examples loadedList()
loadedList<-function(){
  require(dplyr,quietly = TRUE)
  list<-data.frame(Packages=loadedNamespaces()) %>% arrange(Packages)
  return(list)
}