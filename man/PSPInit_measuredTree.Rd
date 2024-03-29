% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PSPInit_measuredTree.R
\docType{methods}
\name{PSPInit_measuredTree}
\alias{PSPInit_measuredTree}
\title{Load and select fully measured tree data for PSP compilation}
\usage{
PSPInit_measuredTree(clusterplotHeader, dataSourcePath)
}
\arguments{
\item{clusterplotHeader}{data.table, Cluster and plot-level attributes, an output from \code{\link{VRIInit_clusterplot}}.}

\item{dataSourcePath}{character, Specifies the path that directs to the VRI original data soruce, i.e.,
\code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.}
}
\value{
A data table that contains tree-level information. A log file that describes the detailed process.
}
\description{
This function selects the tree-level data from vi_c (cardc) based on selected cluster/plot headers.
Additonally, the function calculates basal area and tree per ha factor.
}
\author{
Yong Luo
}
