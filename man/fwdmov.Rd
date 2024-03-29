% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mse.R
\name{fwdmov}
\alias{fwdmov}
\title{Forward Movement Simulation}
\usage{
fwdmov(object, control, rates, time = 0)
}
\arguments{
\item{object}{The fishery model object to be simulated.}

\item{control}{A data frame or matrix containing control measures for each year.}

\item{rates}{A list or vector of rates used in the simulation.}

\item{time}{An optional time parameter, default is 0.}
}
\value{
The updated fishery model object after applying the forward movement simulation.
}
\description{
This function performs a forward movement simulation on a fishery model object
using specified control measures and rates over time.
}
