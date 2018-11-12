options(prompt = "$$$$> ")
options(stringsAsFactors = FALSE)
options(scipen=10)
options(continue="... ")

sshhh <- function(a.package){
  suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only=TRUE)))
}

auto.loads <-c("tidyverse","foreach", "doSNOW", "RSelenium", "qdapRegex", "caret", "rDailyFantasy")

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

suppressMessages(if(interactive()){
invisible(initCookies <- suppressMessages(rDailyFantasy::labsCookies()))
invisible(nbaCookies <- initCookies$labs)
invisible(nflCookies <- initCookies$labs)
invisible(mlbCookies <- initCookies$labs)
invisible(pgaCookies <- initCookies$labs)
invisible(dkCookies <- initCookies$dk)
invisible(monsterCookies <- suppressMessages(rDailyFantasy::getBBMCookies()))
})

message("\n*** Lets makes some fucking money BRO ***\n")

.Last <- function(){
  cat("\n*** Shake N Bake ***\n")
}
