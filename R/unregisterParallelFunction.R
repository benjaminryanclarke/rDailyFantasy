#' Unregister doParallel Cluster
#'
#' @return NULL
#' @export
#'
#' @examples unregister()
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
