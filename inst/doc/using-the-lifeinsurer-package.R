## ----setup, echo = FALSE, message=FALSE---------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(knitr)
library(kableExtra)
library(LifeInsureR)
library(dplyr)
library(tibble)
library(lubridate)

library(pander)

panderOptions('round', 2)
panderOptions('digits', 12)
panderOptions('keep.trailing.zeros', TRUE)
panderOptions('table.split.table', 120)

kableTable = function(grd, ...) {
  grd %>% 
    kable(...) %>% 
    add_header_above(header = c(1, dim(grd)[[2]]) %>% `names<-`(names(dimnames(grd))), align = "c") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
    column_spec(1, bold = T, border_right = T)
}


## ----SimpleExampleRiskTarif, warning=F, results="hide", message = F-----------
library(magrittr)
library(MortalityTables)
library(LifeInsureR)
mortalityTables.load("Austria_Census")

Tarif.L71U = InsuranceTarif$new(
    name = "L71-U",
    type = "wholelife",
    tarif = "DeathPlus - Short Term Life Insurance",
    desc = "Term Life insurance (5 years) with constant sum insured and regular premiums",
    policyPeriod = 5, premiumPeriod = 5,  # premiumPeriod not needed, defaults to maturity

    mortalityTable = mortalityTable.mixed(
      table1 = mort.AT.census.2011.male, weight1 = 0.65, 
      table2 = mort.AT.census.2011.female, weight2 = 0.35
    ),
    i = 0.005, 
    tax = 0.04, 
    costs = initializeCosts(alpha = 0.05, gamma = 0.01, gamma.paidUp = 0.01, unitcosts = 10),
    surrenderValueCalculation = function(surrenderReserve, params, values) { 
      surrenderReserve * 0.9 
    }
);

## ----SimpleExampleRiskContract------------------------------------------------
contract.L71U  = InsuranceContract$new(
  Tarif.L71U, 
  age = 35, 
  contractClosing = as.Date("2020-08-18"), 
  sumInsured = 100000);

## ----SimpleExampleRiskValuesPremCode, eval=F----------------------------------
# contract.L71U$Values$premiums

## ----SimpleExampleRiskValuesPremCodeOut, echo=F-------------------------------
contract.L71U$Values$premiums %>% kable

## ----SimpleExampleRiskValuesResCode, eval=F-----------------------------------
# contract.L71U$Values$reserves

## ----SimpleExampleRiskValuesResOut, echo=F------------------------------------
contract.L71U$Values$reserves %>% pander()

## ----SimpleExampleRiskCFCode, eval=F------------------------------------------
# contract.L71U$Values$cashFlows

## ----SimpleExampleRiskCFOut, echo=F-------------------------------------------
contract.L71U$Values$cashFlows %>% select(starts_with('premiums'), starts_with('death'), -death_Refund_past ) %>% pander()

## ----SimpleExampleRiskCFCostCode, eval=F--------------------------------------
# contract.L71U$Values$cashFlowsCosts[,,,"survival"]

## ----SimpleExampleRiskCFCostOut, echo=F, results="asis"-----------------------
for (base in dimnames(contract.L71U$Values$cashFlowsCosts)[[3]]) {
  cat("* ,,\"", base, "\"\n", sep = "")
  cat(contract.L71U$Values$cashFlowsCosts[,,base, "survival"] %>% pander(round = 4, style = "rmarkdown"))
}

## ----SimpleExampleRiskPVCode, eval=F------------------------------------------
# contract.L71U$Values$presentValues

## ----SimpleExampleRiskPVOut, echo=F-------------------------------------------
contract.L71U$Values$presentValues %>% as.data.frame() %>% select(starts_with('premiums'), starts_with('death'), -death_Refund_past ) %>% pander(round=5)

## ----SimpleExampleRiskPVCostCode, eval=F--------------------------------------
# contract.L71U$Values$presentValuesCosts

## ----SimpleExampleRiskPVCostOut, echo=F, results="asis"-----------------------
for (base in dimnames(contract.L71U$Values$presentValuesCosts)[[3]]) {
  cat("* ,,\"", base, "\"\n", sep = "")
  cat(contract.L71U$Values$presentValuesCosts[,,base,"survival" ] %>% pander(round = 4, style = "rmarkdown"))
}

## ----SimpleExampleRiskPVPremCode, eval=F--------------------------------------
# contract.L71U$Values$premiums

## ----SimpleExampleRiskPVPremOut, echo=F---------------------------------------
contract.L71U$Values$premiums %>% data.frame() %>% pander()

## ----SimpleExampleRiskPremiumsCode, eval=F------------------------------------
# contract.L71U$Values$reserves

## ----SimpleExampleRiskPremiumsOut, echo=F-------------------------------------
contract.L71U$Values$reserves %>% pander(digits=2)

## ----SimpleExampleRiskPremiumCompositionCode, eval=F--------------------------
# contract.L71U$Values$premiumComposition

## ----SimpleExampleRiskPremiumCompositionOut, echo=F---------------------------
contract.L71U$Values$premiumComposition %>% as.data.frame() %>% select(-loading.frequency, -rebate.premium, -rebate.partner, -profit.advance, -rebate.sum, -charge.noMedicalExam, -premium.risk.actual, -premium.risk.security, -risk.disease, -premium.risk.disease.actual, -premium.risk.disease.security, -starts_with('Zillmer')) %>% pander()

## ----SimpleExampleRiskConversionCode, eval=F----------------------------------
# contract.L71U.prf = contract.L71U$premiumWaiver(t = 3)
# contract.L71U.prf$Values$reserves

## ----SimpleExampleRiskConversionOut, echo=F-----------------------------------
contract.L71U.prf = contract.L71U$premiumWaiver(t = 3)
contract.L71U.prf$Values$reserves %>% pander()

## ----SimpleExampleRiskPremiumGrid, eval=T, results="hide"---------------------
grd = contractGridPremium(
  axes = list(age = seq(20, 80, 5), policyPeriod = seq(10, 40, 5)),
  tarif = Tarif.L71U, 
  contractClosing = as.Date("2020-08-18"), 
  sumInsured = 100000
)
grd

## ----SimpleExampleRiskPremiumGridOut, echo = F--------------------------------
grd %>% kableTable(digits = 2)


## ----SimpleExampleRiskPremiumGrid3D, results = "hide"-------------------------
grd = contractGridPremium(
  axes = list(age = seq(20, 80, 10), policyPeriod = seq(10, 40, 10), sumInsured = c(10000, 50000, 100000)),
  tarif = Tarif.L71U, 
  contractClosing = as.Date("2020-08-18")
)
grd

## ----SimpleExampleRiskPremiumGrid3DOut, echo=F, results="asis"----------------
for (d in dimnames(grd)[[3]]) {
  cat("\n", "* , , ", names(dimnames(grd))[[3]], "=",  d, "\n\n", sep = "")
  # cat(grd[,,d ] %>% as.data.frame() %>% rownames_to_column("age \\|  policyPeriod") %>% pander(digits = 7, round = 2, style = "rmarkdown"))
  cat(grd[,,d ] %>% kableTable(digits = 2), "\n")
}


## ----SimpleExampleRiskPremiumGridLifeTables, results = "hide"-----------------
grd = contractGridPremium(
  axes = list(mortalityTable = mort.AT.census["m", ], age = seq(20, 80, 10)),
  tarif = Tarif.L71U, 
  sumInsured = 100000,
  contractClosing = as.Date("2020-08-18")
) 
grd

## ----SimpleExampleRiskPremiumGridLifeTablesOUT, echo = F----------------------
grd %>% pander(round=1, digits=15, keep.trailing.zeros = T)

## -----------------------------------------------------------------------------
str(InsuranceContract.ParameterDefaults)

## ----results="asis"-----------------------------------------------------------
# pandoc.listRK(InsuranceContract.ParameterDefaults)

## ----TarifDefinition, message = F---------------------------------------------
Tarif.PureEnd = InsuranceTarif$new(
  name = "Example Tariff - Pure Endowment",
  type = "pureendowment",
  tarif = "PE1-RP",
  desc = "A pure endowment with regular premiums (standard tariff)",

  mortalityTable = mort.AT.census.2011.unisex,
  i = 0.005,
  # Costs: 4% acquisition, where 2.5% are zillmered, 5\% of each premium as beta costs, 
  #        1%o administration costs of the sum insured over the whole contract period
  costs = initializeCosts(alpha = 0.04, Zillmer = 0.025, beta = 0.05, gamma.contract = 0.001, gamma.paidUp = 0.001),
  unitcosts = 10,

  # Yearly premiums get no surcharge, monthly premiums add +4%
  premiumFrequencyLoading = list("1" = 0, "12" = 0.04),
  premiumRefund = 1,  # Full gross premium refund upon death
  tax = 0.04,         # 4% insurance tas

  surrenderValueCalculation = function(surrenderReserve, params, values) {
    n = params$ContractData$policyPeriod
    # Surrender Penalty is 10% at the beginning and decreases linearly to 0%
    surrenderReserve * (0.9 + 0.1 * (0:n)/n)
  }
)

## ----TarifDefinitionSP--------------------------------------------------------
Tarif.PureEnd.SP = Tarif.PureEnd$createModification(
  name = "Example Tariff - Pure Endowment (SP)",
  tarif = "PE1-SP",
  desc = "A pure endowment with single premiums",
  premiumPeriod = 1
)

## ----TarifDefinitions.All,message = F-----------------------------------------
library(MortalityTables)
mortalityTables.load("Austria_Census")
mortalityTables.load("Austria_Annuities_AVOe2005R")
  # Costs: 4% acquisition, where 2.5% are zillmered, 5\% of each premium as beta costs, 
  #        1%o acquisition costs of the sum insured over the whole contract period
example.Costs = initializeCosts(
  alpha = 0.04, Zillmer = 0.025, 
  beta = 0.05, 
  gamma.contract = 0.001, gamma.paidUp = 0.001
)
example.Surrender = function(surrenderReserve, params, values) {
    n = params$ContractData$policyPeriod
    # Surrender Penalty is 10% at the beginning and decreases linearly to 0%
    surrenderReserve * (0.9 + 0.1 * (0:n)/n)
}

## ----TarifDefinitions.All.End-------------------------------------------------
Tarif.Endowment = InsuranceTarif$new(
  name = "Example Tariff - Endowment",
  type = "endowment",
  tarif = "EN1",
  desc = "An endowment with regular premiums",

  mortalityTable = mort.AT.census.2011.unisex,
  i = 0.005,
  costs = example.Costs,
  unitcosts = 10,
  tax = 0.04,         # 4% insurance tax
  surrenderValueCalculation = example.Surrender
)

## ----TarifDefinitions.All.Life------------------------------------------------
Tarif.Life = InsuranceTarif$new(
  name = "Example Tariff - Whole/Term Life",
  type = "wholelife",
  tarif = "Life1",
  desc = "A whole or term life insurance with regular premiums",

  mortalityTable = mort.AT.census.2011.unisex,
  i = 0.005,
  costs = example.Costs,
  unitcosts = 10,
  tax = 0.04,         # 4% insurance tax
  surrenderValueCalculation = example.Surrender
)

## ----TarifDefinitions.All.ImmAnnuity------------------------------------------
Tarif.ImmAnnuity = InsuranceTarif$new(
  name = "Example Tariff - Immediate Annuity",
  type = "annuity",
  tarif = "Ann1",
  desc = "An annuity with single-premium",
  premiumPeriod = 1,

  mortalityTable = AVOe2005R.unisex,
  i = 0.005,
  costs = example.Costs,
  tax = 0.04         # 4% insurance tax
)

## ----TarifDefinitions.All.DefAnnuity------------------------------------------
# Premium periods and deferral periods can also be given as a function of other
# contract parameters (like the age at contract inception, etc.)
Tarif.DefAnnuity = InsuranceTarif$new(
  name = "Example Tariff - Deferred Annuity",
  type = "annuity",
  tarif = "Life1",
  desc = "A deferred annuity (life-long payments start at age 65) with reg. premiums",

  policyPeriod = function(params, values) { 120 - params$ContractData$age},
  deferralPeriod = function(params, values) { 65 - params$ContractData$age},
  premiumPeriod = function(params, values) { 65 - params$ContractData$age},
    
  mortalityTable = AVOe2005R.unisex,
  i = 0.005,
  costs = example.Costs,
  tax = 0.04,         # 4% insurance tax
  surrenderValueCalculation = example.Surrender
)

## ----TarifDefinitions.All.DD--------------------------------------------------
# An example dread-disease tariff, morbidity is assumed linearly increasing with age
ddTable = mortalityTable.period(name = "Linear dread-disease table", 
                                ages = 0:100, deathProbs = 0:100/500)
Tarif.DreadDisease = InsuranceTarif$new(
  name = "Example Tariff - Dread-Disease",
  type = "dread-disease",
  tarif = "DD1",
  desc = "A dread disease insurance with a lump-sum payment upon diagnosis",

  sumInsured = 50000,
  mortalityTable = mort.AT.census.2011.unisex,
  invalidityTable = ddTable,
  i = 0.005,
  costs = example.Costs,
  unitcosts = 10,
  tax = 0.04,         # 4% insurance tax
  surrenderValueCalculation = example.Surrender
)

## ----Contract-----------------------------------------------------------------
contract.PureEnd = InsuranceContract$new(
    Tarif.PureEnd,
    age = 50, policyPeriod = 20, 
    premiumFrequency = 12,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01")
  )

## ----Contract.premiums,eval=F-------------------------------------------------
# contract.PureEnd$Values$premiums

## ----Contract.premiumsOUT, echo = F-------------------------------------------
contract.PureEnd$Values$premiums %>% kable(digits=4)

## ----Contract.premiumComposition,eval=F---------------------------------------
# contract.PureEnd$Values$premiumComposition

## ----Contract.premiumCompositionOUT, echo = F---------------------------------
contract.PureEnd$Values$premiumComposition %>% as.data.frame() %>% rowid_to_column("t") %>% mutate(t = t-1) %>% select(t, charged, tax, loading.frequency, gross, gamma, beta, alpha, alpha.noZillmer, alpha.Zillmer,  Zillmer, net, risk, savings) %>% pander

## ----ContractNoRefund---------------------------------------------------------
contract.PureEnd.NoRefund = InsuranceContract$new(
    Tarif.PureEnd,
    age = 50, policyPeriod = 20, 
    premiumFrequency = 12,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01"),
    premiumRefund = 0
  )


## ----Contract.premiumsCode, eval = F------------------------------------------
# cbind(`With refund` = contract.PureEnd$Values$premiums, `Without refund` = contract.PureEnd.NoRefund$Values$premiums)

## ----Contract.premiumsOut, echo = F-------------------------------------------
cbind(`With refund` = contract.PureEnd$Values$premiums, `Without refund` = contract.PureEnd.NoRefund$Values$premiums) %>% pander

## ----Contract.riskpremiumsCode, eval = F--------------------------------------
# cbind(
#   `Gross premium with refund` = contract.PureEnd$Values$premiumComposition[,"gross"],
#   `Gross premium w/o refund` = contract.PureEnd.NoRefund$Values$premiumComposition[,"gross"],
#   `Risk premium with refund` = contract.PureEnd$Values$premiumComposition[,"risk"],
#   `Risk premium w/o refund` = contract.PureEnd.NoRefund$Values$premiumComposition[,"risk"]
# )
# 

## ----Contract.riskpremiumsOut, echo = F---------------------------------------
cbind(
  `Gross premium with refund` = contract.PureEnd$Values$premiumComposition[,"gross"],
  `Gross premium w/o refund` = contract.PureEnd.NoRefund$Values$premiumComposition[,"gross"],
  `Risk premium with refund` = contract.PureEnd$Values$premiumComposition[,"risk"],
  `Risk premium w/o refund` = contract.PureEnd.NoRefund$Values$premiumComposition[,"risk"]
) %>% as_tibble() %>% rowid_to_column("t") %>% mutate(t = t-1) %>% pander

## ----Contract.SP--------------------------------------------------------------
contract.PureEnd.SP1 = InsuranceContract$new(
    Tarif.PureEnd,
    age = 40, policyPeriod = 45, premiumPeriod = 1,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01")
  )
contract.PureEnd.SP2 = InsuranceContract$new(
    Tarif.PureEnd.SP,
    age = 40, policyPeriod = 45, # premiumPeriod already set by tariff!
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01")
  )

all_equal(contract.PureEnd.SP1$Values$reserves, contract.PureEnd.SP2$Values$reserves)


## ----PrescribePremium---------------------------------------------------------
# Premium calculated from sumInsured
contract.End = InsuranceContract$new(
  Tarif.Endowment, age = 35, policyPeriod = 10,
  contractClosing = as.Date("2020-08-18"), 
  sumInsured = 10000);

# sumInsured derived from written premium
contract.End.premium = InsuranceContract$new(
  Tarif.Endowment, age = 35, policyPeriod = 10,
  contractClosing = as.Date("2020-08-18"), 
  premium = 1139.06);

contract.End.premiumBeforeTax = InsuranceContract$new(
  Tarif.Endowment, age = 35, policyPeriod = 10,
  contractClosing = as.Date("2020-08-18"), 
  premium = c(written_beforetax = 1095.25));

contract.End.premiumGross = InsuranceContract$new(
  Tarif.Endowment, age = 35, policyPeriod = 10,
  contractClosing = as.Date("2020-08-18"), 
  premium = c(gross = 1085.25));


## ----PrescribePremiumOUTPUT,echo = FALSE--------------------------------------
bind_rows(
  c(Contract = "contract.End", contract.End$Values$premiums[c("net", "Zillmer", "gross", "written_beforetax", "written")], sumInsured = contract.End$Parameters$ContractData$sumInsured),
  c(Contract = "contract.End.premium", contract.End.premium$Values$premiums[c("net", "Zillmer", "gross", "written_beforetax", "written")], sumInsured = contract.End.premium$Parameters$ContractData$sumInsured),
  c(Contract = "contract.End.premiumBeforeTax", contract.End.premiumBeforeTax$Values$premiums[c("net", "Zillmer", "gross", "written_beforetax", "written")], sumInsured = contract.End.premiumBeforeTax$Parameters$ContractData$sumInsured),
  c(Contract = "contract.End.premiumGross", contract.End.premiumGross$Values$premiums[c("net", "Zillmer", "gross", "written_beforetax", "written")], sumInsured = contract.End.premiumGross$Parameters$ContractData$sumInsured)
)

## ----InitialCapital-----------------------------------------------------------
# Contract with initial capital of 5.000 EUR
contract.Endow.initialCapital = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  initialCapital = 5000,
  age = 40, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)
# For comparison: Contract without initial capital of 5.000 EUR
contract.Endow = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  age = 40, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)

## ----InitialCapitalOUTPUT-----------------------------------------------------
data.frame(
  `Premium with initialCapital`= contract.Endow.initialCapital$Values$premiumComposition[,"charged"], 
  `Premium without initialCapital`= contract.Endow$Values$premiumComposition[,"charged"], 
  `Res.with initialCapital`= contract.Endow.initialCapital$Values$reserves[,"contractual"], 
  `Res.without initialCapital`= contract.Endow$Values$reserves[,"contractual"]
)

## ----Contract.PureEndPrf, results="hide"--------------------------------------
contract.PureEnd.NoRefund.Prf = contract.PureEnd.NoRefund$clone()$premiumWaiver(t = 7)
contract.PureEnd.NoRefund.Prf$Values$reserves

## ----Contract.PureEndPrfOUT, echo=F-------------------------------------------
contract.PureEnd.NoRefund.Prf$Values$reserves %>% pander

## ----costStructureDimensions--------------------------------------------------
initializeCosts() %>% dimnames

## ----costExample, eval=F------------------------------------------------------
# initializeCosts(alpha = 0.04, Zillmer = 0.025, beta = 0.05, gamma.contract = 0.001)
# 
# # the above is the short form of:
# costs.Bsp = initializeCosts()
# costs.Bsp[["alpha", "SumPremiums", "once"]] = 0.04
# costs.Bsp[["Zillmer", "SumPremiums", "once"]] = 0.025 # German Zillmer maximum
# costs.Bsp[["beta", "GrossPremium", "PremiumPeriod"]] = 0.05
# costs.Bsp[["gamma", "SumInsured", "PolicyPeriod"]] = 0.001

## ----costCashFlowsCode, eval=F------------------------------------------------
# contract.PureEnd.NoRefund$Values$absCashFlows

## ----costCashFlows, echo=F----------------------------------------------------
contract.PureEnd.NoRefund$Values$absCashFlows[1:11,] %>% select(alpha, Zillmer, beta, gamma, gamma_nopremiums, unitcosts) %>% pander()

## ----FrequencyCharges---------------------------------------------------------
Tarif.Life.FrequencyLoading = Tarif.Life$createModification(
  name = "Term life (frequency loading)",
  premiumFrequencyLoading = list("1" = 0.0, "2" = 0.01, "4" = 0.015, "12" = 0.02)
)
Tarif.Life.FrequencyApprox1 = Tarif.Life$createModification(
  name = "Term life (k-th yearly, approx. 1.Ord.)",
  premiumFrequencyOrder = 1
)
Tarif.Life.FrequencyApprox2 = Tarif.Life$createModification(
  name = "Term life (k-th yearly, approx. 2.Ord.)",
  premiumFrequencyOrder = 2
)
Tarif.Life.FrequencyApprox3 = Tarif.Life$createModification(
  name = "Term life (k-th yearly, exact)",
  premiumFrequencyOrder = Inf
)

Tarif.Life.FrequencyExpense = Tarif.Life$createModification(
  name = "Term life (modified gamma costs)",
  costs = function(params, values) {
    switch (toString(params$ContractData$premiumFrequency),
        "12" = initializeCosts(alpha = 0.04, Zillmer = 0.025,  beta = 0.05,  gamma.contract = 0.00127, gamma.paidUp = 0.001),
        "4" = initializeCosts(alpha = 0.04, Zillmer = 0.025,  beta = 0.05,  gamma.contract = 0.00119, gamma.paidUp = 0.001),
        "2" = initializeCosts(alpha = 0.04, Zillmer = 0.025,  beta = 0.05,  gamma.contract = 0.0011, gamma.paidUp = 0.001),
        initializeCosts(alpha = 0.04, Zillmer = 0.025,  beta = 0.05,  gamma.contract = 0.001, gamma.paidUp = 0.001)
    )
  }
)

## ----FrequencyCharges.Grid----------------------------------------------------
contractGridPremium(
  axes = list(tarif = c(Tarif.Life.FrequencyLoading, Tarif.Life.FrequencyApprox1, 
                        Tarif.Life.FrequencyApprox2, Tarif.Life.FrequencyApprox3, 
                        Tarif.Life.FrequencyExpense),
              premiumFrequency = c(1, 2, 4, 12)),
  age = 40, policyDuration = 20,
  sumInsured = 100000,
  contractClosing = as.Date("2020-09-01")
) %>% kableTable

## ----Protection.Security------------------------------------------------------
contractGridPremium(
  axes = list(age = seq(30, 60, 10), security = 10*(0:5)/100),
  tarif = Tarif.Life,
  policyDuration = 20,
  sumInsured = 100000,
  contractClosing = as.Date("2020-09-01")
) %>% kableTable(digits = 2)

## ----RoundingHelper-----------------------------------------------------------
# Define three different rounding IDs / instances: "raw" with rounding to
# the nearest integer, "hundred" with rounding to the nearest multiple of 
# 100 and "accurate" rounded to the nearest multiple of 0.0001:
rounding = RoundingHelper$new(raw = 0, hundred = -2, accurate = 4)

# The rounding IDs are used as first argument in the rounding$round function:
rounding$round("raw", c(1234.567891, 0.00012345, 1234))
rounding$round("hundred", c(1234.567891, 0.00012345, 1234))
rounding$round("accurate", c(1234.567891, 0.00012345, 1234))

# If the given spec does not exist, no rounding it applied
rounding$round("non-existing", c(1234.567891, 0.00012345, 1234))

# Add a new spec with different settings:
rounding$setRounding("non-existing", 1)
rounding$round("non-existing", c(1234.567891, 0.00012345, 1234))

## ----RoundingHelper.Contract--------------------------------------------------
Tarif.EndowmentSI = InsuranceTarif$new(
    type = "pureendowment",
    tarif = "Endow1",
    age = 40, policyPeriod = 20,
    premiumRefund = 1,

    mortalityTable = mort.AT.census.2011.unisex,
    cost = initializeCosts(alpha = 0.04, gamma.contract = 0.0005),
    i = 0.03,
    sumInsured = 10000,
    contractClosing = as.Date("2020-09-01")
)
Tarif.EndowmentSI.rounded = Tarif.EndowmentSI$createModification(
    Rounding = list("Premium gross unit" = 3, "Premium net unit" = 6, "Premium net" = 2)
)
Contract.sumInsured = InsuranceContract$new(tarif = Tarif.EndowmentSI)
Contract.sumInsured.rounded = InsuranceContract$new(tarif = Tarif.EndowmentSI.rounded)


# premiums of the original tariff:
Contract.sumInsured$Values$premiums[c("unit.net", "-1et", "unit.gross", "gross")]

## ----eval=TRUE,echo=FALSE,result='asis'---------------------------------------
src = list.files(path = file.path(getwd(), "R"), pattern = ".R$", full.names = TRUE)
filecontents = unlist(lapply(src, readLines, warn = FALSE), use.names = FALSE)
results = stringr::str_extract(filecontents, "\\$round\\(\"([^\"]*)\"", group = 1)
results = results[!is.na(results)]
results = results[!(results %in% c("raw", "hundred", "accurate", "non-existing"))] %>% sort
cat(paste('-', results), sep = '\n')

## ----Grid.Endowment.compare, results = "hide"---------------------------------
grd = contractGridPremium(
  axes = list(tarif = c(Tarif.PureEnd, Tarif.Endowment, Tarif.PureEnd.SP), premiumRefund = c(0, 0.5, 1)),
  age = 50, policyPeriod = 20,
  sumInsured = 10000,
  contractClosing = as.Date("2020-09-01")
)
grd

## ----Grid.Endowment.compareOUT, echo = F--------------------------------------
grd %>% kableTable

## ----Grid.Endowment.compareOther----------------------------------------------
grd = contractGrid(
  axes = list(tarif = c(Tarif.PureEnd, Tarif.Endowment, Tarif.PureEnd.SP), premiumRefund = c(0, 0.5, 1)),
  age = 50, policyPeriod = 20,
  sumInsured = 10000,
  contractClosing = as.Date("2020-09-01")
)

## ----Grid.Endowment.compareOtherG1, eval = F----------------------------------
# # Compare net premiums without loadings:
# contractGridPremium(grd, premium = "net")

## ----Grid.Endowment.compareOtherG1Out, echo = F-------------------------------
contractGridPremium(grd, premium = "net") %>% kableTable

## ----Grid.Endowment.compareOtherG2, eval = F----------------------------------
# # Compare premium sums over the whole contract period (all contracts have the same sumInsured)
# contractGridPremium(grd, .fun = function(c) {with(c$Values,
#      unitPremiumSum * premiums["written"])
# })

## ----Grid.Endowment.compareOtherG2Out, echo = F-------------------------------
# Compare premium sums over the whole contract period (all contracts have the same sumInsured)
contractGridPremium(grd, .fun = function(c) {with(c$Values, 
     unitPremiumSum * premiums["written"])
}) %>% kableTable(digits = 2)

## ----Grid.Endowment.compareOtherG3, eval = F----------------------------------
# # Compare risk premiums at time t=10 (the 11th row of the premium decomposition)
# contractGridPremium(grd, .fun = function(c) {c$Values$premiumComposition[11, "risk"]})

## ----Grid.Endowment.compareOtherG3Out, echo = F-------------------------------
# Compare risk premiums at time t=10 (the 11th row of the premium decomposition)
contractGridPremium(grd, .fun = function(c) {c$Values$premiumComposition[11, "risk"]}) %>% kableTable(digits = 2)

## ----Grid.Endowment.compareOtherG4, eval = F----------------------------------
# # Compare present value of all benefits and refunds (without costs) at time t=0
# contractGridPremium(grd, .fun = function(c) {c$Values$absPresentValues[1, "benefitsAndRefund"]})

## ----Grid.Endowment.compareOtherG4Out, echo = F-------------------------------
# Compare present value of all benefits and refunds (without costs) at time t=0
contractGridPremium(grd, .fun = function(c) {c$Values$absPresentValues[1, "benefitsAndRefund"]}) %>% kableTable(digits = 2)

## ----Grid.Protection, results ="hide"-----------------------------------------
grd = contractGridPremium(
  axes = list(mortalityTable = mort.AT.census["m", -(1:10)], i = c(0, 0.005, 0.01), age = c(30, 45, 60), policyPeriod = c(10, 20)),
  tarif = Tarif.Life,
  contractClosing = as.Date("2020-09-01"),
  sumInsured = 10000
)
grd

## ----Grid.ProtectionOUT, echo=F, results="asis"-------------------------------
for (a in dimnames(grd)[[3]]) {
for (d in dimnames(grd)[[4]]) {
  cat("\n", "* ", names(dimnames(grd))[[3]], "=",  a, ", ", names(dimnames(grd))[[4]], "=",  d, "\n\n", sep = "")
  # cat(grd[,,d ] %>% as.data.frame() %>% rownames_to_column("age \\|  policyPeriod") %>% pander(digits = 7, round = 2, style = "rmarkdown"))
  cat(grd[,, a, d] %>% kableTable(digits = 2), "\n")
}
}

## ----ExcelExport,eval=F-------------------------------------------------------
# contract.exportExample = contract.PureEnd.NoRefund$clone()$
#   addDynamics(t = 3, SumInsuredDelta = 10000)$
#   addDynamics(t = 5, SumInsuredDelta = 15000)$
#   addDynamics(t = 10, SumInsuredDelta = 15000)$
#   addDynamics(t = 14, SumInsuredDelta = 10000)
# exportInsuranceContract.xlsx(contract.exportExample, filename = "Example_PureEndowment_Dynamics.xlsx")

## ----VmGlgExample-------------------------------------------------------------
VMGL.contract = InsuranceContract$new(
    Tarif.PureEnd,
    age = 35, policyPeriod = 30, 
    premiumFrequency = 1,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01")
  )

showVmGlgExamples(VMGL.contract)

## ----contractLayers-----------------------------------------------------------
# Contract with initial capital of 5.000 EUR
ctr.dynInc = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  age = 40, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)$
  addDynamics(t = 1, SumInsuredDelta = 1000)$
  addDynamics(t = 5, NewSumInsured = 15000)$
  addDynamics(t = 8, SumInsuredDelta = 4000)


ctr.dynInc$Values$basicData

## ----contractLayers.blocks----------------------------------------------------
for (b in ctr.dynInc$blocks) {
  cat(paste0("Block: ", b$Parameters$ContractData$id, ", starts at t=", b$Parameters$ContractData$blockStart, ", policyPeriod=", b$Parameters$ContractData$policyPeriod, "\n"))
}

## ----contractLayers.blocks.data-----------------------------------------------
ctr.dynInc$blocks$Hauptvertrag$Values$basicData
ctr.dynInc$blocks$dyn1$Values$basicData
ctr.dynInc$blocks$dyn2$Values$basicData
ctr.dynInc$blocks$dyn3$Values$basicData

## ----addBlock.rider-----------------------------------------------------------
ctr.main = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  age = 40, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)
ctr.Rider = InsuranceContract$new(
  tarif = Tarif.L71U, 
  sumInsured = 100000, 
  age = 40, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)
ctr.main$addBlock(block = ctr.Rider)

ctr.withRider = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  age = 40, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)$
  addBlock(tarif = Tarif.L71U, sumInsured = 100000, 
           age = 40, policyPeriod = 10,
           contractClosing = as.Date("2020-09-01"))

## ----contractExtension--------------------------------------------------------
# original contract, expiring after 20 years
ContractA = InsuranceContract$new(
  tarif = Tarif.Endowment,
  age = 40, policyPeriod = 20,
  sumInsured = 10000,
  contractClosing = as.Date("2000-07-01")
)

# premium-free extension
ContractB = ContractA$clone()$
  addExtension(id = "Verlaengerung1", contractPeriod = 5, premiumPeriod = 0)
# sumInsured calculated from existing reserve:
ContractB$blocks$Verlaengerung1$Parameters$ContractData$sumInsured
ContractB$Values$basicData

# extension with given sumInsured resulting in 0 (gross) premiums
ContractC = ContractA$clone()$
  addExtension(id = "Verlaengerung1", contractPeriod = 5, sumInsured = 10723.07973354)
ContractC$blocks$Verlaengerung1$Values$premiums[["gross"]]
ContractC$Values$basicData

# extension with increased sumInsured: real premiums are charged, reserves start from the existing reserve:
ContractD = ContractA$clone()$
  addExtension(id = "Verlaengerung1", contractPeriod = 5, sumInsured = 20000)
ContractD$Values$basicData

# extension with regular premiums, which are given: sumInsured is calculated from it, reserves start from the existing reserve:
ContractD = ContractA$clone()$
  addExtension(id = "Verlaengerung1", contractPeriod = 5, premium = 597.8771)
ContractD$Values$basicData

## ----PremiumIncrease.Endowment, results = "hide"------------------------------
# For comparison: Contract with constant premiums
contract.Endow.Constant = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  age = 50, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)
# Contract with 4% yearly premium increase and same sum insured
contract.Endow.PremInc = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  premiumIncrease = 1.04,
  age = 50, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)
premium.comparison = data.frame(
  `Sum Insured` = contract.Endow.Constant$Values$basicData[,"SumInsured"],
  `Constant Premium` = contract.Endow.Constant$Values$basicData[,"Premiums"],
  `4% Yearly Increase` = contract.Endow.PremInc$Values$basicData[,"Premiums"],
  check.names = F
  )

## ----PremiumIncrease.EndowmentOut, results = "asis"---------------------------
premium.comparison %>% pander

## ----FixedSumIncrease.WholeLife, results = "hide"-----------------------------
# For comparison: Contract with constant premiums
contract.TermLife.Constant = InsuranceContract$new(
  tarif = Tarif.Life,
  sumInsured = 10000,
  age = 50, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)
# Contract with 4% yearly increase in sum insured (final survival benefit is 10.000)
contract.TermLife.SumInc = InsuranceContract$new(
  tarif = Tarif.Life,
  sumInsured = 10000,
  deathBenefit = (1.04)^(0:20),
  age = 50, policyPeriod = 10,
  contractClosing = as.Date("2020-09-01")
)
premium.comparison = data.frame(
  `Const S.I.` = contract.TermLife.Constant$Values$absCashFlows[,"death"],
  `Const. Premium` = contract.TermLife.Constant$Values$absCashFlows[,"premiums_advance"],
  `4% sum increase` = contract.TermLife.SumInc$Values$absCashFlows[,"death"],
  `Premium w. sum increase` = contract.TermLife.SumInc$Values$absCashFlows[,"premiums_advance"],
  check.names = F
  )
premium.comparison

## ----FixedSumIncrease.WholeLifeOut, results = "asis", echo=F------------------
premium.comparison %>% pander

## ----FixedSumIncrease.Annuity, results = "hide"-------------------------------
# For comparison: Contract with constant annuity
contract.Annuity.Constant = InsuranceContract$new(
  tarif = Tarif.DefAnnuity,
  sumInsured = 1200,
  age = 55, 
  policyPeriod = 10,
  deferralPeriod = 5,
  premiumPeriod = 5,
  contractClosing = as.Date("2020-09-01")
)
# Contract with 4% yearly increase in annuity benefits
contract.Annuity.Increasing = InsuranceContract$new(
  tarif = Tarif.DefAnnuity,
  sumInsured = 1200,
  annuityIncrease = 1.04,
  age = 55, 
  policyPeriod = 10,
  deferralPeriod = 5,
  premiumPeriod = 5,
  contractClosing = as.Date("2020-09-01")
)
# Contract with 4% yearly increase in premiums and in annuity payments
contract.Annuity.IncreasingBoth = InsuranceContract$new(
  tarif = Tarif.DefAnnuity,
  sumInsured = 1200,
  annuityIncrease = 1.04,
  premiumIncrease = 1.04,
  age = 55, 
  policyPeriod = 10,
  deferralPeriod = 5,
  premiumPeriod = 5,
  contractClosing = as.Date("2020-09-01")
)
premium.comparison = data.frame(
  `Const. Annuity` = contract.Annuity.Constant$Values$absCashFlows[,"survival_advance"],
  `Const. Premium` = contract.Annuity.Constant$Values$absCashFlows[,"premiums_advance"],
  `4% Annuity Increase` = contract.Annuity.Increasing$Values$absCashFlows[,"survival_advance"],
  `Premium w. Ann.Increase` = contract.Annuity.Increasing$Values$absCashFlows[,"premiums_advance"],
  `Inc.Premium w. Ann.Increase` = contract.Annuity.IncreasingBoth$Values$absCashFlows[,"premiums_advance"],
  check.names = F
  )

## ----FixedSumIncrease.AnnuityOut, results = "asis"----------------------------
premium.comparison %>% pander

## ----DynamicIncrease.Endowment------------------------------------------------
# For comparison: Contract with constant annuity
contract.Endowment.Dynamics = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  age = 40, 
  policyPeriod = 10,
  contractClosing = as.Date("2020-09-01"),
  id = "Initial contract"
)$
  addDynamics(t = 5, NewSumInsured = 11000, id = "Dynamic at 5")$
  addDynamics(t = 7, NewSumInsured = 12000, id = "Dynamic at 7")$
  addDynamics(t = 8, NewSumInsured = 13500, id = "Dynamic at 8")

# Over-all contract sum insured and premiums for all blocks combined
contract.Endowment.Dynamics$Values$basicData[,c("SumInsured", "Premiums")] %>% pander

## ----DynamicIncrease.EndowmentOut, results = "asis", echo = F-----------------
blk = c(list(`Over-all contract` = contract.Endowment.Dynamics), contract.Endowment.Dynamics$blocks)

padArray = function(arr = NULL, pad = 0, len = 0) {
  padEnd = max(0, len - pad - NROW(arr)) # if len is too short, return an array containing at least the arr
  nrcols = ifelse(is.null(arr), 0, NCOL(arr))
  rbind(
    array(0, dim = c(pad, nrcols)) %>% `colnames<-`(colnames(arr)),
    arr,
    array(0, dim = c(padEnd, nrcols)) %>% `colnames<-`(colnames(arr))
  ) %>% `colnames<-`(colnames(arr))
}

lapply(blk, function(b) {
  basic = padArray(b$Values$basicData, pad = b$Parameters$ContractData$blockStart)
  basic[,"SumInsured"]
}) %>% 
  bind_cols() %>% 
  rowid_to_column("t") %>% 
  mutate(t = t-1) %>% 
  pander(caption = "Sum Insured for the over-all contract and each of the blocks")

lapply(blk, function(b) {
  basic = padArray(b$Values$basicData, pad = b$Parameters$ContractData$blockStart)
  basic[,"Premiums"]
}) %>% 
  bind_cols() %>% 
  rowid_to_column("t") %>% 
  mutate(t = t-1) %>% 
  pander(caption = "Premium time series for the over-all contract and each of the blocks")


## ----AdvanceProfitExample-----------------------------------------------------
profit.Advance.V1 = ProfitParticipation$new(
    name = "Profit Scheme for advance profit participation, V 1.0",
    advanceProfitParticipation = 0.38
);

Tarif.Life.withPP = Tarif.Life$createModification(
  name = "Example Tariff - Whole/Term Life with profit sharing",
  tarif = "Life1PP",
  profitParticipationScheme = profit.Advance.V1
)

contract.LifePP = InsuranceContract$new(
  tarif = Tarif.Life.withPP,
  age = 40, policyPeriod = 10,
  sumInsured = 100000,
  contractClosing = as.Date("2019-09-01")
)

## ----advanceProfitExample.PremiumComposition, eval=F--------------------------
# contract.LifePP$Values$premiumComposition

## ----advanceProfitExample.PremiumCompositionOUT, echo=F-----------------------
contract.LifePP$Values$premiumComposition[,c("charged", "tax", "unitcosts", "profit.advance", "gross", "net")] %>% as.data.frame() %>% rowid_to_column("t") %>% mutate(t = t-1) %>% pander

## ----Example.ProfitParticipation----------------------------------------------
ProfitScheme.example = ProfitParticipation$new(
  name = "Example Profit Scheme, V 1.0",
  profitComponents = c("interest", "risk", "expense", "sum", "TBF"),

  getInterestOnProfits    = PP.rate.interestProfitPlusGuarantee,
  getInterestProfitBase   = PP.base.meanContractualReserve,
  getRiskProfitBase       = PP.base.ZillmerRiskPremium,
  getExpenseProfitBase    = PP.base.sumInsured,
  getSumProfitBase        = PP.base.sumInsured,
  getTerminalBonusFundBase = PP.base.totalProfitAssignment,
  
  mortalityProfitRate = 0.15,
  expenseProfitRate = 0.01,
  sumProfitRate = function(params, ...) {if (params$ContractData$sumInsured > 1000000) 0.005 else 0;},
  terminalBonusFundRate = 0.3,
  
  calculateSurvivalBenefit      = PP.benefit.ProfitPlusTerminalBonusReserve,
  
  calculateDeathBenefitAccrued  = PP.benefit.ProfitPlusInterestMinGuaranteeTotal,
  calculateDeathBenefitTerminal = PP.benefit.TerminalBonus,
  calculateSurrenderBenefitAccrued = PP.benefit.ProfitPlusHalfInterestMinGuaranteeTotal,
  calculateSurrenderBenefitTerminal = function(profits, ...) {  profits[, "TBF"] / 2 },
  calculatePremiumWaiverBenefitAccrued = PP.benefit.Profit,
  calculatePremiumWaiverBenefitTerminal = function(profits, ...) {  profits[, "TBF"] / 2 },
  
  profitClass = NULL
)

## ----Example.PP.Endowment-----------------------------------------------------
contract.Endow.PP = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  deathBenefit = 5,
  age = 50, policyPeriod = 15,
  
  profitParticipationScheme = ProfitScheme.example,
  contractClosing = as.Date("2020-09-01")
)

## ----ExamplePP.Endowment.addScenario------------------------------------------
contract.Endow.PP$
  addProfitScenario(id = "Current total credited rate", guaranteedInterest = 0.005, interestProfitRate = 0.02, totalInterest = 0.025)$
  addProfitScenario(id = "Current TCR-1%", guaranteedInterest = 0.005, interestProfitRate = 0.01, totalInterest = 0.015)$
  addProfitScenario(id = "Current TCR+1%", guaranteedInterest = 0.005, interestProfitRate = 0.03, totalInterest = 0.035)
  

## ----ExamplePP.Endowment.Scenarios--------------------------------------------
contract.Endow.PP$Values$profitScenarios$`Current total credited rate` %>%
  as.data.frame() %>%
  select(ends_with("Base"), ends_with("Interest"), ends_with("Rate"), -TBFRate, -TBFBase, -totalInterest) %>%
  rowid_to_column("t") %>% mutate(t = t - 1) %>% kable()

## ----ExPP.End.reserve, echo = F-----------------------------------------------
contract.Endow.PP$Values$reserves %>% as.data.frame() %>% 
  rownames_to_column("t") %>% 
  select(t, SumInsured, Zillmer) %>% 
  mutate(AvgZillmer = rollingmean(c(0,Zillmer))) %>%
  pander()

## ----ExamplePP.Endowment.ScenariosAttib---------------------------------------
contract.Endow.PP$Values$profitScenarios$`Current total credited rate` %>%
  as.data.frame() %>%
  select(ends_with("Profit"), totalProfitAssignment, -totalProfit) %>%
  rowid_to_column("t") %>% mutate(t = t - 1) %>%
  pander


## ----ExamplePP.Endowment.ScenariosTBFTotal------------------------------------
contract.Endow.PP$Values$profitScenarios$`Current total credited rate` %>%
  as.data.frame() %>%
  select(TBFBase, TBFRate, TBFBonusAssignment, regularBonusAssignment, TBF, regularBonus, totalProfit) %>%
  rowid_to_column("t") %>% mutate(t = t - 1) %>%
  pander

## ----ExamplePP.Endowment.ScenariosBenefits------------------------------------
contract.Endow.PP$Values$profitScenarios$`Current total credited rate` %>%
  as.data.frame() %>%
  select(survival, deathAccrued, death, surrenderAccrued, surrender, premiumWaiverAccrued, premiumWaiver) %>%
  rowid_to_column("t") %>% mutate(t = t - 1) %>%
  pander

## ----ExamplePP.Endowment.Scenario.Decr----------------------------------------
contract.Endow.PP$
  addProfitScenario(id = "decreasing TCR", guaranteedInterest = 0.005, 
                    interestProfitRate = (15:0)/15 * 0.02, 
                    expenseProfitRate = c(rep(0.01, 5), rep(0.005, 5), rep(0, 6)))

contract.Endow.PP$Values$profitScenarios$`decreasing TCR` %>%
  as.data.frame() %>%
  select(interestBase, expenseBase, interestProfitRate, expenseProfitRate, interestOnProfitRate, interestProfit, expenseProfit, totalProfit) %>%
  rowid_to_column("t") %>% mutate(t = t - 1) %>%
  kable

## ----WaitingPeriod.Hook-------------------------------------------------------
contract.Endow.Waiting = InsuranceContract$new(
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  age = 50, policyPeriod = 15,
  
  contractClosing = as.Date("2020-09-01"),
  adjustCashFlows = function(x, ...) { x[1:3, "death_SumInsured"] = 0; x }
)

contract.Endow.Waiting$Values$cashFlows[,c("premiums_advance", "survival_advance", "death_SumInsured")] %>% pander

contractGridPremium(
  axes = list(age = seq(20, 80, 10), adjustCashFlows = c(function(x, ...) x, function(x, ...) { x[1:3, "death_SumInsured"] = 0; x })),
  tarif = Tarif.Endowment,
  sumInsured = 10000,
  policyPeriod = 15,
  
  contractClosing = as.Date("2020-09-01")
) %>% `colnames<-`(c("Full benefit", "Waiting period"))


## ----termfix.Zillmeradjust.Hook, eval=FALSE-----------------------------------
#   costs = initializeCosts(alpha = 0.04, Zillmer = 0.035, gamma = 0.0015, gamma.fullcontract = 0.001),
#   adjustPremiumCoefficients = function(coeff, type, premiums, params, values, premiumCalculationTime) {
#     if (type == "Zillmer") {
#       coeff[["SumInsured"]][["costs"]]["gamma", "SumInsured", "guaranteed"] = 1
#     }
#     coeff
#   },

