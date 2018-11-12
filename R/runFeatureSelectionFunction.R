#' run Feature Selection Function
#'
#' @param num.iters iterations
#' @param feature.vars variables
#' @param class.var targetVariable
#'
#' @return rfe
#' @export
#'
#' @examples run.feature.selection(feature.vars=qbData$train, class.var=ActualPoints)
run.feature.selection <- function(num.iters=20, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs,
                        method = "cv",
                        verbose = FALSE,
                        returnResamp = "all",
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var, sizes = variable.sizes, rfeControl = control)
  return(results.rfe)
  }
