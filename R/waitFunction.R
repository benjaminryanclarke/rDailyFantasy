#' Slow function/script down
#'
#' @param x time to sleep (pause/wait)
#'
#' @return NULL
#' @export
#'
#' @examples waitFor(4.4)
waitFor <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
}
