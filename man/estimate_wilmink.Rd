% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_wilmink.R
\name{estimate_wilmink}
\alias{estimate_wilmink}
\title{Title}
\usage{
estimate_wilmink(data)
}
\arguments{
\item{data}{}
}
\value{

}
\description{
Title
}
\examples{
# One animal:
data <- simulate_woods(5, 0.01, 0.001)
estimate_wilmink(data)

# Multiple animals:
library(tidyverse)
data <- lapply(1:5, function(x) simulate_woods(cowID=x)) \%>\% bind_rows()
estimate_wilmink(data)

}