## ----setup, echo = FALSE, message=FALSE---------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(knitr)
library(kableExtra)
library(LifeInsureR)
library(dplyr)
library(tibble)
library(lubridate)

library(pander)

## ----CalculatePortfolio,echo=TRUE,eval=FALSE----------------------------------
#  results = NULL;
#  results = calculate_portfolio(bestandinfos.all,
#      tarif = c("ProdName1", "ProdName2"), GV = c("123"), debug =TRUE)
#  openxlsx::write.xlsx(results, outfile("Prods-1-2"),
#      asTable = TRUE, overwrite = TRUE, sheetName = "Vergleichsrechnung")
#  openXL(outfile("Prods-1-2"))

