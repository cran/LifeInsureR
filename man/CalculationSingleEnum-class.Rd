% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\docType{class}
\name{CalculationSingleEnum-class}
\alias{CalculationSingleEnum-class}
\alias{CalculationEnum}
\title{Enum to define how much of a contract needs to be calculated automatically.}
\description{
Enum to define how much of a contract needs to be calculated automatically.
}
\details{
When an \link{InsuranceContract} object is created, all time series are immediately
calculated. However, sometimes, one only needs part of the values, so it
would be a waste of resources to calculate e.g. all future reserves and
profit participation, if only premiums are of interest.

Possible values are:
\itemize{
\item "all"
\item "probabilities"
\item "cashflows"
\item "presentvalues"
\item "premiums"
\item "absvalues"
\item "reserves"
\item "premiumcomposition"
\item "profitparticipation"
\item "history"
}
}
