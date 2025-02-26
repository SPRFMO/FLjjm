% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{readFLRPsjjm}
\alias{readFLRPsjjm}
\title{Create an FLPar contaning reference points from a JJMS model run directory}
\usage{
readFLRPsjjm(name, path, stock = 1)
}
\arguments{
\item{name}{Name of the ctl model file, \emph{character}.}

\item{path}{Path ot the model folder structure, \emph{character}.}

\item{stock=1}{Stock to extract, of relevance on 2 stocks model runs, \emph{numeric}.}
}
\value{
An object of class FLPar.
}
\description{
An object of class \emph{FLPar} is created from the information in the output
files in a \emph{jjms} folder structure. See \emph{buildFLRPsjjm} to check what
reference points are being extracted from the inputs outputs of \emph{jjms}.
}
\examples{
rps <- readFLRPsjjm(name="h1_1.07",
  path=system.file("ext-data", "single_stock", package="FLjjm"))
summary(rps)
}
\seealso{
\link[jjmR:readJJM]{jjmR::readJJM} \link[FLCore:FLBiol]{FLCore::FLBiol} buildFLBjjm
}
