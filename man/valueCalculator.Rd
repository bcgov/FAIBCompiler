% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valueCalculator.R
\docType{methods}
\name{valueCalculator}
\alias{valueCalculator}
\alias{valueCalculator,character,data.table,data.table,data.table-method}
\title{Calcualte tree value-VRI specific}
\usage{
valueCalculator(species, grossVolMatrix, grossMerchVolMatrix, callGradeMatrix)

\S4method{valueCalculator}{character,data.table,data.table,data.table}(species, grossVolMatrix, grossMerchVolMatrix, callGradeMatrix)
}
\arguments{
\item{species}{character, Species codes in BC inventory system.}

\item{grossVolMatrix}{data.table, Calculated gross volume for each log. The first column of this table is
the volume for the stump. If missing, the function calculates the total
net merchantable volume.}

\item{grossMerchVolMatrix}{data.table, Calculated gross merchantable volume for each log. If missing, all the merchantable
volume is assigned as 0.}

\item{callGradeMatrix}{data.table, Ground call grading table.}
}
\value{
Data table that contains net value (\code{VAL_NET}) and net merchantable value (\code{VAL_MER}).
}
\description{
This function calculates tree value for each tree
based on ground called grade. From the second column to the last column,
the \code{grossVolMatrix} table should have same dimensions (i.e.,
number of rows and columns) of \code{callGradeMatrix} and \code{grossMerchVolMatrix} if
they are provided. Furthermore, be aware of the correspondingness among the matrix.
In the function, two lookup table are hardcoded (i.e., \code{spv_spc} and \code{sp_cost}).
This function is part of the \code{log_valu_2017.sas}.
}
\author{
Yong Luo
}
