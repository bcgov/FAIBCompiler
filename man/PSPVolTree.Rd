% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PSPVolTree.R
\docType{methods}
\name{PSPVolTree}
\alias{PSPVolTree}
\title{Calcualte tree volume-PSP specific}
\usage{
PSPVolTree(
  treeData,
  equation,
  logMinLength,
  stumpHeight,
  breastHeight,
  UTOPDIB,
  HTEstimateMethod,
  htDBHCoeff
)
}
\arguments{
\item{treeData}{data.table, Tree data}

\item{equation}{character, Specifies which taper equation form will be used to calculate
diameter inside bark for a given height.
Must be either KBEC or KFIZ3. If missing, default is KBEC}

\item{stumpHeight}{numeric, Specifies stump height. If missing, 0.3 m will be used.}

\item{breastHeight}{numeric, Specifies breast height. 1.3 m will be used when this arguement is missing.}

\item{UTOPDIB}{numeric, Specifies minimum merchantable inside bark diameter. 10 cm is used as a default.}

\item{bestHeightModels}{data.table, External table that contains the best height/DBH model and
coefficients by becsubzone and species.}

\item{HTBTOPModel}{character, Specifies whether the height estimate for broken top trees either
from \code{taper} or from \code{height}.}
}
\value{
A data table
}
\description{
This function use BEC(or FIZ) and species-specific taper equation to
calculate tree volume. Before calculation, the function adjusts height
for broken top trees: scenario 1 (D scenario): availability of DBH, DIB at broken height;
scenario 2 (H scenario): availability of projected tree height in the field.
This functions also assigns the volume multiplier adjustment.
}
\author{
Yong Luo
}
