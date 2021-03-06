% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nflCoreFunction.R
\name{nflCoreFun}
\alias{nflCoreFun}
\title{nflCoreFun}
\usage{
nflCoreFun(mY = 2015:2017, mW = 1:17, partitionSize = 0.8,
  labModel = "2018", style = "projections")
}
\arguments{
\item{mY}{modelYears}

\item{mW}{modelWeeks}

\item{partitionSize}{0-1}

\item{labModel}{modelName}

\item{style}{research/projections}
}
\value{
researchData or projectionData
}
\description{
nflCoreFun
}
\examples{
mlbCoreFun(mY,mW,partitionSize,labModel,style)
}
