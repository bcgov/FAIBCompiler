% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PSPInit_lossFactor.R
\docType{methods}
\name{PSPInit_lossFactor}
\alias{PSPInit_lossFactor}
\title{Load and select trees that have loss factor information-VRI specific}
\usage{
PSPInit_lossFactor(fullMeasuredTrees, dataSourcePath)
}
\arguments{
\item{fullMeasuredTrees}{data.table, Selected trees in vi_c, which includes full, enhanced and H-enhanced trees.
An output of \code{\link{VRIInit_measuredTree}}.}

\item{dataSourcePath}{character, Specifies the path that directs to the VRI original data soruce, i.e.,
\code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.}
}
\value{
A data table that contains loss factor data. A log file documents the detailed process
}
\description{
This function loads and selects trees that have loss factor information (\code{vi_d}, cardd) based on
selected trees from vi_c.
}
\author{
Yong Luo
}
