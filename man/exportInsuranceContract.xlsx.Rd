% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportInsuranceContract_xlsx.R
\name{exportInsuranceContract.xlsx}
\alias{exportInsuranceContract.xlsx}
\title{Export an insurance contract (object of class \link{InsuranceContract}) to an 'Excel' file}
\usage{
exportInsuranceContract.xlsx(contract, filename)
}
\arguments{
\item{contract}{The insurance contract to export}

\item{filename}{Target 'Excel' file name for export}
}
\value{
None
}
\description{
Export an insurance contract (object of class \link{InsuranceContract}) to an 'Excel' file
}
\details{
The function \code{exportInsuranceContract.xlsx} exports an object
of class \link{InsuranceContract} to an 'Excel' file. All basic data, as well as
the time series of (absolute and unit) cash flows, reserves, premiums, premium
composition and all profit participation scenarios are exported to the file
in nicely looking tables.

No new calculations are done in this function. It only prints out the values
stored in \code{contract$Values}.
}
\examples{
library("MortalityTables")
mortalityTables.load("Austria_Annuities_AVOe2005R")
# A trivial deferred annuity tariff with no costs:
tariff = InsuranceTarif$new(name = "Test Annuity", type = "annuity", tarif = "Annuity 1A",
    mortalityTable = AVOe2005R.unisex, i=0.01)
contract = InsuranceContract$new(
    tariff,
    age = 35, YOB = 1981,
    policyPeriod = 30, premiumPeriod = 15, deferralPeriod = 15,
    sumInsured = 1000,
    contractClosing = as.Date("2016-10-01")
);
\donttest{exportInsuranceContract.xlsx(contract, tempfile("Example_annuity_contract", fileext = ".xlsx"))}
}
