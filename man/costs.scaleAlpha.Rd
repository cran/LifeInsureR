% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InsuranceParameters.R
\name{costs.scaleAlpha}
\alias{costs.scaleAlpha}
\title{Helper function to modify alpha costs of an insurance contract individually}
\usage{
costs.scaleAlpha(scale)
}
\arguments{
\item{scale}{The scale for  alpha / Zillmer cost}
}
\description{
Returns a function that modifies alpha (and Zillmer) costs by the given scale,
but otherwise uses the full costs defined by the Costs parameter.
}
\details{
This function can be set as adjustCosts or adjustMinCosts hook parameters
for a tariff or contract and can be used to apply cost adjustments on a
per-contract basis.
}
