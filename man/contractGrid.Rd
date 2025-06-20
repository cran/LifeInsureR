% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contractGrid.R
\name{contractGrid}
\alias{contractGrid}
\alias{contractGridPremium}
\title{Create a grid of InsuranceContract objects or premiums with each axis representing one varying parameter}
\usage{
contractGrid(
  axes = list(age = seq(20, 60, 10), policyPeriod = seq(5, 35, 5)),
  YOB = NULL,
  observationYear = NULL,
  ...
)

contractGridPremium(
  contractGrid = NULL,
  premium = "written",
  .fun = function(cntr) {
     cntr$Values$premiums[[premium]]
 },
  ...
)
}
\arguments{
\item{axes}{List of paramters spanning the dimensions of the grid.}

\item{YOB}{optional year of bith. If missing, the \code{observationYear} and the contract's age}

\item{observationYear}{The observation year, for which the grid shall be calculated. If given, the YOB is calculated from it, otherwise the contract's YOB is used}

\item{...}{In \code{contractGrid}: Additional parameters to be passed to \ifelse{html}{\href{../../LifeInsureR/html/InsuranceContract.html#method-new}{\code{InsuranceContract$new()}}}{\code{InsuranceContract$new()()}}; In \code{contractGridPremium}: Additional parameters to be passed to \code{contractGrid}.}

\item{contractGrid}{(optional) existing contract grid from which to derive
premiums. If not given, \link{contractGrid} is called with all parameters, so
\code{...} should contain an \code{axes} argument in that case.}

\item{premium}{The type of premium to derive (key of the \code{contract$Values$premiums} list.}

\item{.fun}{The function to extract the desired premium from a contract
object. By default it accesses the premium vector and extracts the
type of premium given in the \code{premium} parameter. One can,
however pass any other extractor function to access e.g. reserves,
cash flows etc. at any desired time.}
}
\value{
multi-dimensional array of insurance contracts (dimensions defined by the axes argument)

a array of premiums (or other contract-specific value) for the grid defined by the \code{axes} argument to \code{contractGrid}
}
\description{
The function \code{contractGrid} creates a (two- or multi-dimensional) grid
of InsuranceContract objects, where each axis represents one of the insurance
parameters varying as given in the \code{axes} argument (as a named list).

The function \code{contractGridPremium} returns a grid of premiums as requested in
the \code{premium} parameter rather than the full InsuranceContract objects.
It is a convenience wrapper around \code{contractGrid} and is recommended if
one is only interested in a grid of one particular value (typically some
kind of premium).
The function \code{contractGridPremium} can also be used on an existing
\code{contractGrid}-generated grid of contracts to extract grid of numerical
values of the specified premiums. If no contract grid is passed to
\code{contractGridPremium}, \code{contractGrid} will be called to create it.
}
\details{
The function \code{contractGrid} will return the full \link{InsuranceContract}
objects, so apply can later be used to extract premiums, reserves and other
values to display in a grid. For this feature, one can also use the convenience
function \code{contractGridPremium}.

The \code{axes} list describing the parameters changing along the axes of the
resulting grid is internally expanded with \code{\link[=expand.grid]{expand.grid()}}. The resulting flat
list of parameter (together with the fixed parameters passed as \code{...})
is then passed to the \ifelse{html}{\href{../../LifeInsureR/html/InsuranceContract.html#method-new}{\code{InsuranceContract$new()}}}{\code{InsuranceContract$new()()}} call to create the corresponding
contract object.

To create the human-readable row-/columnnames of the resulting array,
the function \code{\link[=makeContractGridDimname]{makeContractGridDimname()}} for each value of the axes, allowing
human-readable representations e.g. of a tariff or a mortality table as
the dimension label.
}
\examples{
library("MortalityTables")
mortalityTables.load("Austria_Annuities_AVOe2005R")
# A trivial deferred annuity tariff with no costs, premiums during whole
# deferral period, 30 years annuity payments:
tariff = InsuranceTarif$new(name="Test Annuity", type="annuity", tarif = "Annuity 1A",
    mortalityTable = AVOe2005R.unisex, i=0.01,
    deferralPeriod = function(params, ...) { params$ContractData$premiumPeriod },
    policyPeriod = function(params, ...) { params$ContractData$premiumPeriod + 30 }
)
contractGrid(
    axes = list(
        age = seq(20, 60, 10),
        premiumPeriod = seq(5,30, 5)
    ),
    tarif = tariff,
    sumInsured = 1000,
    contractClosing = as.Date("2023-11-01")
)

library("MortalityTables")
mortalityTables.load("Austria_Annuities_AVOe2005R")
# A trivial deferred annuity tariff with no costs, premiums during whole
# deferral period, 30 years annuity payments:
tariff = InsuranceTarif$new(name="Test Annuity", type="annuity", tarif = "Annuity 1A",
    mortalityTable = AVOe2005R.unisex, i=0.01,
    deferralPeriod = function(params, ...) { params$ContractData$premiumPeriod },
    policyPeriod = function(params, ...) { params$ContractData$premiumPeriod + 30 }
)
contractGridPremium(
    axes = list(
        age = seq(20, 60, 10),
        premiumPeriod = seq(5,30, 5)
    ),
    tarif = tariff,
    sumInsured = 1000,
    contractClosing = as.Date("2023-11-01")
)


}
