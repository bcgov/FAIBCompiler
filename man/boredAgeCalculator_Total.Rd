% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boredAgeCalculator.R
\docType{methods}
\name{boredAgeCalculator_Total}
\alias{boredAgeCalculator_Total}
\alias{boredAgeCalculator_Total,numeric-method}
\title{Derive bored age using total age}
\usage{
boredAgeCalculator_Total(totalAge)

\S4method{boredAgeCalculator_Total}{numeric}(totalAge)
}
\arguments{
\item{totalAge}{numeric, Total tree age, ie., age at height of 0.}
}
\value{
bored age
}
\description{
This function is to derive bore age based on total age (\code{totalAge}).The function is one of the four functions that derive bored age using
different method. The rests are \code{\link{boredAgeCalculator_Bore}}, \code{\link{boredAgeCalculator_Phys}} and
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
