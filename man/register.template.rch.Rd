% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register_utilities.R
\name{register.template.rch}
\alias{register.template.rch}
\title{Register (best align) a template to a reference. Return more detail than above.}
\usage{
register.template.rch(
  template.img,
  reference.img,
  h.min.overlap = 1,
  w.min.overlap = 1,
  method = "SQDIFF",
  fill.typ = "opposite",
  return.imgQ = F,
  printQ = F,
  plotQ = F
)
}
\arguments{
\item{XX}{The XX}
}
\value{
The function will XX
}
\description{
Register (best align) a template to a reference. Return more detail than above.
For experiments.
}
\details{
The function depends on the Rvision library which must be installed
}
