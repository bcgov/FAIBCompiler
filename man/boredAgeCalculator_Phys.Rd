% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boredAgeCalculator.R
\docType{methods}
\name{boredAgeCalculator_Phys}
\alias{boredAgeCalculator_Phys}
\alias{boredAgeCalculator_Phys,numeric-method}
\title{Derive bored age using physiological age}
\usage{
boredAgeCalculator_Phys(physAge)

\S4method{boredAgeCalculator_Phys}{numeric}(physAge)
}
\arguments{
\item{physAge}{numeric, Pysiological age.}
}
\value{
bored age
}
\description{
This function is to derive bore age based on physiological age (\code{physAge}). The function is one of the four functions that derive bored age using
different method. The rests are \code{\link{boredAgeCalculator_Bore}}, \code{\link{boredAgeCalculator_Total}} and
\code{\link{boredAgeCalculator_Prorated}}.
}
\seealso{
\code{\link{boredAgeCalculator_Total}}
\code{\link{boredAgeCalculator_Phys}}
\code{\link{boredAgeCalculator_Prorated}}
}
\author{
Yong Luo
}
