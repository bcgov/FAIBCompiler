% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regBA_WSV.R
\docType{methods}
\name{regBA_WSV}
\alias{regBA_WSV}
\title{To fit basal area and whole stem volume regression using mixed effect model}
\usage{
regBA_WSV(inputData, needCombs)
}
\arguments{
\item{inputData}{data.table, The data for fitting regression.}

\item{needCombs}{data.table, The combinations of BEC+SP0+LV_D are needed to derive coefficients.}
}
\value{
coefficient table
}
\description{
This function takes the selected data for fitting regression model between
basal area and whole stem volume using mixed effect model. The models are
fitted based on strata of BEC+SP0+LV_D
}
\author{
Yong Luo
}
