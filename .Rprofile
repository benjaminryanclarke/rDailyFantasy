options(prompt = "$$$$> ")
options(stringsAsFactors = FALSE)
options(scipen=10)
options(continue="... ")

.First<-function(){
sshhh <- function(a.package){
  suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only=TRUE)))
}

auto.loads <-c("tidyverse","foreach", "doSNOW", "RSelenium", "qdapRegex", "caret", "skimr", "caretEnsemble", "coach", "ROI", "ROI.plugin.glpk", "ompr", "ompr.roi", "rDailyFantasy")

if(interactive()){
  invisible(sapply(auto.loads, sshhh))
}

.env <- new.env()

.env$unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

.env$unfactor <- function(df){
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
}

#attach(.env,warn.conflicts = F)

#if(interactive()){
#invisible(initCookies <- rDailyFantasy::labsCookies())
#invisible(nbaCookies <- initCookies$labs)
#invisible(nflCookies <- initCookies$labs)
#invisible(mlbCookies <- initCookies$labs)
#invisible(pgaCookies <- initCookies$labs)
#invisible(dkCookies <- initCookies$dk)
#invisible(monsterCookies <- rDailyFantasy::getBBMCookies())
#}

}
.Last <- function(){
  cat("\n*** Shake N Bake ***\n")
}
