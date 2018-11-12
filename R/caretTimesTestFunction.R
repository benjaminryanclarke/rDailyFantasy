#' caret Times Test Function
#'
#' @param model caretModel
#'
#' @return times
#' @export
#'
#' @examples caretTimesTest(nbaZeroOwnrangerOOB)
caretTimesTest<-function(model){
  if(model$times$everything[3][[1]]<60){
    paste0(model$method," fitted with ",model$control$method," of number ",model$control$number," and ",model$control$repeats," repeats"," took ",model$times$everything[3][[1]]," seconds to complete")
    }else{
  paste0(model$method," fitted with ",model$control$method," of number ",model$control$number," and ",model$control$repeats," repeats"," took ",model$times$everything[3][[1]]/60," minutes to complete")
  }
}


