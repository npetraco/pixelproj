% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeling_utilities.R
\name{prob.pattern}
\alias{prob.pattern}
\title{Compute the probability of pattern given theta fit from a set of patterns}
\usage{
prob.pattern(pattern.mat, a.dressed.fit.crf.obj, a.logZ)
}
\arguments{
\item{XX}{The XX}
}
\value{
The function will XX
}
\description{
Compute the probability of pattern given a fit and dressed CRF object.
Instead of recomputing Z each time this is call, we just pass in logZ
}
