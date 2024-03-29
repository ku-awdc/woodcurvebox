% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_wilmink.R
\name{simulate_wilmink}
\alias{simulate_wilmink}
\title{Simulate data from a Wilmink curve}
\usage{
simulate_wilmink(herd, animals, interval, loga, b, k, d, resid)
}
\arguments{
\item{herd}{herd identifier}

\item{animals}{number of animals in the herd}

\item{interval}{interval between observations in days}

\item{loga}{parameter 'a' in the Wilmink curve (log-scale)}

\item{b}{parameter 'b' in the Wilmink curve}

\item{k}{parameter 'k' in the Wilmink curve}

\item{d}{parameter 'd' in the Wilmink curve}

\item{resid}{standard deviation of the random residuals}
}
\value{
simulated data on herd level with an interval of 28 DIM between observations
}
\description{
Simulate data from a Wilmink curve
}
\examples{
data <- simulate_wilmink()

}
