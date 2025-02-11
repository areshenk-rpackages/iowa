% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatSimulationOutput.R
\name{formatSimulationOutput}
\alias{formatSimulationOutput}
\title{Organize simulation generated quantities}
\usage{
formatSimulationOutput(draws, paramValues, deck, scale)
}
\arguments{
\item{draws}{A draws_array object returned by a simulate_igt model}

\item{paramValues}{A draws_array object returned by createStanParamsForSimulation()}

\item{deck}{A deck object}

\item{scale}{The scale factor used for simulation}
}
\value{
A named list containing simulated IGT data.
}
\description{
Organize simulation generated quantities
}
