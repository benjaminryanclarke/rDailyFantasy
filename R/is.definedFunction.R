#' is object defined Function
#'
#' @param sym object to check for
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples is.defined(cookies)
is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}