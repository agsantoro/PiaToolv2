library(readxl)
# parameters
parameters = readxl::read_xlsx("models/hpv/xlsx/definitiva.xlsx", sheet = "Parameters")
#load("hpv/data/parameters.RData")


# mortality and incidence data
mortall = readxl::read_xlsx("models/hpv/xlsx/definitiva.xlsx", sheet = "mortall")
#load("hpv/data/mortall.RData")

mortcecx = readxl::read_xlsx("models/hpv/xlsx/definitiva.xlsx", sheet = "mortcecx")
#load("hpv/data/mortcex.RData")
#mortcecx[is.na(mortcecx)] = 0

incidence = readxl::read_xlsx("models/hpv/xlsx/definitiva.xlsx", sheet = "incidence")
#load("hpv/incidence.RData")

cohortSizeAcVac = readxl::read_xlsx("models/hpv/xlsx/definitiva.xlsx", sheet = "EDAD_VACUNA")
names(cohortSizeAcVac)[1]="age"
#load("hpv/cohortSizeAcVac.RData")
cohortSizeAcVac = cohortSizeAcVac %>% pivot_longer(cols = 2:ncol(cohortSizeAcVac), names_to = "country")

source("models/hpv/createLifetable.R", encoding = "UTF-8")

# model function

getPrime = function (
    input,
    country,
    coverageBase,
    #birthCohortSizeFemale,
    cohortSizeAtVaccinationAgeFemale,
    coverageAllDosis,
    vaccineEfficacyVsHPV16_18,
    targetAgeGroup,
    vaccinePricePerFIG,
    vaccineDeliveryCostPerFIG,
    totalVaccineCostPerFIG,
    cancerTreatmentCostPerEpisodeOverLifetime,
    DALYsForCancerDiagnosis,
    DALYsForNonTerminalCancerSequelaePperYear,
    DALYsForTerminalCancer,
    discountRate,
    costoProg,
    proportionOfCervicalCancerCasesThatAreDueToHPV16_18,
    #GDPPerCapita,
    mortall,
    mortcecx,
    incidence,
    #dalys,
    parameters) {
  
  coverageAllDosis = coverageAllDosis/100
  vaccineEfficacyVsHPV16_18 = vaccineEfficacyVsHPV16_18/100
  discountRate = discountRate/100
  coverageBase = coverageBase/100
  proportionOfCervicalCancerCasesThatAreDueToHPV16_18 = proportionOfCervicalCancerCasesThatAreDueToHPV16_18/100
  
  GDPPerCapita = as.numeric(parameters[parameters$Country==input$country,20])
  
  
  # lifetable
  lifeTable = createLifetable(country)
  
  # model
  age = 0:100
  
  standardisedCohortSize_brith = lifeTable$lx
  standardisedCohortSize_vacAge = rep(NA,101) 
  standardisedCohortSize_vacAge[age<targetAgeGroup] = 0
  standardisedCohortSize_vacAge[age>=targetAgeGroup] = standardisedCohortSize_brith[age>=targetAgeGroup]/standardisedCohortSize_brith[age==12]
  
  
  # for (i in (length(standardisedCohortSize_vacAge[age<targetAgeGroup])):(length(standardisedCohortSize_vacAge))) {
  #   browser()
  #   standardisedCohortSize_vacAge[i] = standardisedCohortSize_brith[i]/standardisedCohortSize_brith[age==targetAgeGroup]
  # }
  
  discountFactor = rep(NA,101) 
  discountFactor[age<=targetAgeGroup] = 1
  discountFactor[age>targetAgeGroup] = 1/((1+discountRate)^(age[age>targetAgeGroup]-targetAgeGroup))
  
  cumDiscFactor = c(0,cumsum(discountFactor))[1:101]
  
  standardisedCohortSizeDisc = standardisedCohortSize_vacAge * discountFactor
  
  ##### PRE VACCINATION #####
  
  ceCx16_18IncidencePreVac = as.numeric(incidence[incidence$`Country ¦ Age [10]`==country,-1]) * proportionOfCervicalCancerCasesThatAreDueToHPV16_18
  ceCx16_18IncidencePreVac[is.na(ceCx16_18IncidencePreVac)] = 0
  
  ceCx16_18IncidencePreVacDisc = ceCx16_18IncidencePreVac * discountFactor
  ceCx16_18IncidencePreVacDisc[is.na(ceCx16_18IncidencePreVacDisc)] = 0
  
  ceCx16_18MortalityPreVac = as.numeric(mortcecx[mortcecx$`Country ¦ Age [11]`==country,-1]) * proportionOfCervicalCancerCasesThatAreDueToHPV16_18
  ceCx16_18MortalityPreVac[is.na(ceCx16_18MortalityPreVac)] = 0
  
  ceCx16_18MortalityPreVacDisc = ceCx16_18MortalityPreVac * discountFactor
  
  lifeTable$ExDisc = c()
  for (i in 1:101) {
    ageInt = floor(lifeTable$Ex[i])
    lifeTable$ExDisc[i]=cumDiscFactor[age==ageInt] + (lifeTable$Ex[i] - floor(lifeTable$Ex[i])) * discountFactor[age==ageInt]
  }
  lifeYearsLostPreVac = ceCx16_18MortalityPreVac * lifeTable$Ex
  lifeYearsLostPreVacDisc = ceCx16_18MortalityPreVacDisc * lifeTable$ExDisc
  
  cancerDisabilitiesPreVac = ceCx16_18MortalityPreVac * lifeTable$Ex
  
  cancerDisabilitiesPreVacDisc = ceCx16_18MortalityPreVacDisc * lifeTable$ExDisc
  
  dalysNonFatal = (DALYsForCancerDiagnosis+parameters$`DALYs for cancer sequelae year`[parameters$Country==country]*4)
  dalysFatal = (DALYsForTerminalCancer+DALYsForCancerDiagnosis)
  dalysNonFatalDisc = DALYsForCancerDiagnosis+DALYsForNonTerminalCancerSequelaePperYear*(1/(1+discountRate)+1/(1+discountRate)^2+1/(1+discountRate)^3+1/(1+discountRate)^4)
  dalysFatalDisc = DALYsForCancerDiagnosis+DALYsForTerminalCancer*1/(1+discountRate)
  
  cancerDisabilitiesLostPreVac = (ceCx16_18IncidencePreVac - ceCx16_18MortalityPreVac) * dalysNonFatal + ceCx16_18MortalityPreVac * dalysFatal
  
  ############ ver dalys
  
  cancerDisabilitiesLostPreVacDisc = ((ceCx16_18IncidencePreVac - ceCx16_18MortalityPreVac) * dalysNonFatalDisc + ceCx16_18MortalityPreVac * dalysFatalDisc) / (1 + discountRate) ^ (age - input$targetAgeGroup)
  ############
  cancerCareCostsPreVac = ceCx16_18IncidencePreVac * cancerTreatmentCostPerEpisodeOverLifetime
  
  cancerCareCostsPreVacDisc = cancerCareCostsPreVac / (1+discountRate) ^ (age - input$targetAgeGroup)
  
  
  ##### POST VACCINATION #####
  
  ceCx16_18IncidencePostVac = c()
  ceCx16_18IncidencePostVac[age<targetAgeGroup] = ceCx16_18IncidencePreVac[age<targetAgeGroup]
  ceCx16_18IncidencePostVac[age>=targetAgeGroup] = ceCx16_18IncidencePreVac[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  
  ceCx16_18IncidencePostVacDisc = c()
  ceCx16_18IncidencePostVacDisc[age<targetAgeGroup] = ceCx16_18IncidencePreVacDisc[age<targetAgeGroup]
  ceCx16_18IncidencePostVacDisc[age>=targetAgeGroup] = ceCx16_18IncidencePreVacDisc[age>=targetAgeGroup] * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)
  
  ceCx16_18MortalityPostVac = c()
  ceCx16_18MortalityPostVac[age<targetAgeGroup] = ceCx16_18MortalityPreVac[age<targetAgeGroup]
  ceCx16_18MortalityPostVac[age>=targetAgeGroup] = ceCx16_18MortalityPreVac[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  
  ceCx16_18MortalityPostVacDisc = c()
  ceCx16_18MortalityPostVacDisc[age<targetAgeGroup] = ceCx16_18MortalityPreVacDisc[age<targetAgeGroup]
  ceCx16_18MortalityPostVacDisc[age>=targetAgeGroup] = ceCx16_18MortalityPreVacDisc[age>=targetAgeGroup] * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)
  
  lifeYearsLostPostVac = c()
  lifeYearsLostPostVac[age<targetAgeGroup] = lifeYearsLostPreVac[age<targetAgeGroup]
  lifeYearsLostPostVac[age>=targetAgeGroup] = lifeYearsLostPreVac[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  
  lifeYearsLostPostVacDisc = c()
  lifeYearsLostPostVacDisc[age<targetAgeGroup] = lifeYearsLostPreVacDisc[age<targetAgeGroup]
  lifeYearsLostPostVacDisc[age>=targetAgeGroup] = lifeYearsLostPreVacDisc[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  
  cancerDisabilitiesLostPostVac = c()
  cancerDisabilitiesLostPostVac[age<targetAgeGroup] = cancerDisabilitiesLostPreVac[age<targetAgeGroup]
  cancerDisabilitiesLostPostVac[age>=targetAgeGroup] = cancerDisabilitiesLostPreVac[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  
  cancerDisabilitiesLostPostVacDisc = c()
  cancerDisabilitiesLostPostVacDisc[age<targetAgeGroup] = cancerDisabilitiesLostPreVacDisc[age<targetAgeGroup]
  cancerDisabilitiesLostPostVacDisc[age>=targetAgeGroup] = cancerDisabilitiesLostPreVacDisc[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  
  cancerCareCostsPostVac = c()
  cancerCareCostsPostVac[age<targetAgeGroup] = cancerCareCostsPreVac[age<targetAgeGroup]
  cancerCareCostsPostVac[age>=targetAgeGroup] = cancerCareCostsPreVac[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  cancerCareCostsPostVacDisc = c()
  cancerCareCostsPostVacDisc[age<targetAgeGroup] = cancerCareCostsPreVacDisc[age<targetAgeGroup]
  cancerCareCostsPostVacDisc[age>=targetAgeGroup] = cancerCareCostsPreVacDisc[age>=targetAgeGroup] * (1 - coverageBase * vaccineEfficacyVsHPV16_18)
  
  
  ##### difference after vaccination #####
  
  ceCx16_18IncidenceDif = ceCx16_18IncidencePreVac - ceCx16_18IncidencePostVac
  
  ceCx16_18IncidenceDifDisc = ceCx16_18IncidencePreVacDisc - ceCx16_18IncidencePostVacDisc
  
  ceCx16_18MortalityDif = ceCx16_18MortalityPreVac - ceCx16_18MortalityPostVac
  
  ceCx16_18MortalityDifDisc = ceCx16_18MortalityPreVacDisc - ceCx16_18MortalityPostVacDisc
  
  lifeYearsLostDif = lifeYearsLostPreVac - lifeYearsLostPostVac
  
  lifeYearsLostDifDisc = lifeYearsLostPreVacDisc - lifeYearsLostPostVacDisc
  
  cancerDisabilitiesLostDif = cancerDisabilitiesLostPreVac - cancerDisabilitiesLostPostVac
  
  cancerDisabilitiesLostDifDisc = cancerDisabilitiesLostPreVacDisc - cancerDisabilitiesLostPostVacDisc
  
  cancerCareCostsDif = cancerCareCostsPreVac - cancerCareCostsPostVac
  
  cancerCareCostsDifDisc = cancerCareCostsPreVacDisc - cancerCareCostsPostVacDisc

  ##### model DF #####
  model = data.frame(
    age,
    cancerCareCostsDif,
    cancerCareCostsDifDisc,
    cancerCareCostsPostVac,
    cancerCareCostsPostVacDisc,
    cancerCareCostsPreVac,
    cancerCareCostsPreVacDisc,
    cancerDisabilitiesLostDif,
    cancerDisabilitiesLostDifDisc,
    cancerDisabilitiesLostPostVac,
    cancerDisabilitiesLostPostVacDisc,
    cancerDisabilitiesLostPreVac,
    cancerDisabilitiesLostPreVacDisc,
    cancerDisabilitiesPreVac,
    cancerDisabilitiesPreVacDisc,
    ceCx16_18IncidenceDif,
    ceCx16_18IncidenceDifDisc,
    ceCx16_18IncidencePostVac,
    ceCx16_18IncidencePostVacDisc,
    ceCx16_18IncidencePreVac,
    ceCx16_18IncidencePreVacDisc,
    ceCx16_18MortalityDif,
    ceCx16_18MortalityDifDisc,
    ceCx16_18MortalityPostVac,
    ceCx16_18MortalityPostVacDisc,
    ceCx16_18MortalityPreVac,
    ceCx16_18MortalityPreVacDisc,
    cumDiscFactor,
    discountFactor,
    lifeYearsLostDif,
    lifeYearsLostDifDisc,
    lifeYearsLostPostVac,
    lifeYearsLostPostVacDisc,
    lifeYearsLostPreVac,
    lifeYearsLostPreVacDisc,
    standardisedCohortSize_brith,
    standardisedCohortSize_vacAge,
    standardisedCohortSizeDisc
  )
  
  ##### incidence plot #####
  
  y1 = ceCx16_18IncidencePreVac * 100000
  #y1 = c(rep(NA,(targetAgeGroup+1)),y1[1:(length(y1)-(targetAgeGroup+1))])
  y1[age>75] = NA
  y2 = 100000 * ceCx16_18IncidencePreVac * (1-input$coverageAllDosis/100*input$vaccineEfficacyVsHPV16_18/100) 
  #y2=c(rep(NA,(targetAgeGroup+1)),y2[1:(length(y1)-(targetAgeGroup+1))])
  y2[age>75] = NA
  
  dataPlot = data.frame(
    x=0:100,
    y1,
    y2)
  
  plot =
    highchart() %>%
    hc_add_series(data = dataPlot, name="Antes de la vacunación", type = "line", hcaes(x = x, y = y1)) %>% hc_xAxis(min = 0, max = 80) %>%
    hc_add_series(data = dataPlot, name = "Después de la vacunación", type = "line", hcaes(x = x, y = y2)) %>% hc_xAxis(min = 0, max = 80) %>%
    hc_title(
      text = "Efecto de la vacunación en la incidencia del cáncer de cuello de útero por edad",
      margin = 20,
      align = "center",
      style = list(color = "black", useHTML = TRUE)
    ) %>% hc_xAxis(
        title = list(
        text="Edad"
      )
    ) %>% hc_yAxis(
        title = list(
        text="Incidencia cáncer de cuello uterino relacionado con HPV-16/18"
      )
    )
  # undisc = c(
  #   "Cohort size at birth (female)"=input$birthCohortSizeFemale,
  #   "Cohort size at vaccination age (female)"=input$cohortSizeAtVaccinationAgeFemale,
  #   "Cost of vaccination"=input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis,
  #   "Treatment costs saved"=sum(model$cancerCareCostsDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Net cost"=-(sum(model$cancerCareCostsDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis),
  #   "Cervical cancers prevented"=sum(model$ceCx16_18IncidenceDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Deaths prevented"=sum(model$ceCx16_18MortalityDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Life years saved"=sum(model$lifeYearsLostDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Nonfatal DALYs prevented"=sum(model$cancerDisabilitiesLostDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Incremental cost per cervical cancer prevented"=(-(sum(model$cancerCareCostsDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(sum(model$ceCx16_18IncidenceDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale),
  #   "Incremental cost per life saved"=(-(sum(model$cancerCareCostsDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(sum(model$ceCx16_18MortalityDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale),
  #   "Incremental cost per life year saved"=(-(sum(model$cancerCareCostsDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(sum(model$lifeYearsLostDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale),
  #   "Incremental cost perDALY prevented"=(-(sum(model$cancerCareCostsDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/
  #     ((sum(model$lifeYearsLostDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(sum(model$cancerDisabilitiesLostDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)),
  #   "GDP per capita"=input$GDPPerCapita,
  #   "ROI" = ((sum(model$cancerCareCostsDif*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)-(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis)*100
  # )
  # 
  # disc = c(
  #   "Cohort size at birth (female)"=input$birthCohortSizeFemale,
  #   "Cohort size at vaccination age (female)"=input$cohortSizeAtVaccinationAgeFemale,
  #   "Cost of vaccination"=input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis,
  #   "Treatment costs saved"=sum(model$cancerCareCostsDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Net cost"=-(sum(model$cancerCareCostsDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis),
  #   "Cervical cancers prevented"=sum(model$ceCx16_18IncidenceDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Deaths prevented"=sum(model$ceCx16_18MortalityDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Life years saved"=sum(model$lifeYearsLostDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Nonfatal DALYs prevented"=sum(model$cancerDisabilitiesLostDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale,
  #   "Incremental cost per cervical cancer prevented"=(-(sum(model$cancerCareCostsDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(sum(model$ceCx16_18IncidenceDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale),
  #   "Incremental cost per life saved"=(-(sum(model$cancerCareCostsDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(sum(model$ceCx16_18MortalityDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale),
  #   "Incremental cost per life year saved"=(-(sum(model$cancerCareCostsDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(sum(model$lifeYearsLostDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale),
  #   "Incremental cost perDALY prevented"=(-(sum(model$cancerCareCostsDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/
  #     ((sum(model$lifeYearsLostDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)+(sum(model$cancerDisabilitiesLostDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)),
  #   "GDP per capita"=input$GDPPerCapita,
  #   "ROI" = ((sum(model$cancerCareCostsDifDisc*model$standardisedCohortSize_vacAge)*input$cohortSizeAtVaccinationAgeFemale)-(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis))/(input$totalVaccineCostPerFIG*input$cohortSizeAtVaccinationAgeFemale*input$coverageAllDosis)*100
  # )
  undisc = c(
    "Tamaño de la cohorte de nacimiento (mujeres)"=input$birthCohortSizeFemale,
    "Tamaño de la cohorte en edad de vacunación (mujeres)"=input$cohortSizeAtVaccinationAgeFemale,
    "Costo total de la intervención (USD)"= costoProg + (input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis)),
    "Costos evitados atribuibles a la intervención (USD)" = cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVac) - ((model$cancerCareCostsPreVac) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge),
    "Diferencia de Costos respecto al escenario basal (USD)"= (input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVac) - ((model$cancerCareCostsPreVac) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)),
    "Cánceres de cuello uterino evitados (n)"=input$cohortSizeAtVaccinationAgeFemale * (sum(((model$ceCx16_18IncidencePreVac*(1-coverageBase * vaccineEfficacyVsHPV16_18))-(model$ceCx16_18IncidencePreVac*(1-coverageAllDosis * vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))),
    "Muertes evitadas (n)"=(sum(((model$ceCx16_18MortalityPostVac)-((model$ceCx16_18MortalityPreVac) * (1*(1-coverageAllDosis*vaccineEfficacyVsHPV16_18))))*(model$standardisedCohortSize_vacAge))) * cohortSizeAtVaccinationAgeFemale,
    "Años de vida salvados (AVS)"= (sum(((model$lifeYearsLostPostVac) - ((model$lifeYearsLostPreVac) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))) * input$cohortSizeAtVaccinationAgeFemale,
    "Años de Vida Ajustados por Discapacidad evitados (AVAD)"=sum(((model$cancerDisabilitiesLostPostVac) - ((model$cancerDisabilitiesLostPreVac) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge)) * input$cohortSizeAtVaccinationAgeFemale,
    "Razón de costo-efectividad incremental por cáncer de cuello uterino prevenido (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVac) - ((model$cancerCareCostsPreVac) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/(input$cohortSizeAtVaccinationAgeFemale * (sum(((model$ceCx16_18IncidencePreVac*(1-coverageBase * vaccineEfficacyVsHPV16_18))-(model$ceCx16_18IncidencePreVac*(1-coverageAllDosis * vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge)))),
    "Razón de costo-efectividad incremental por vida salvada (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVac) - ((model$cancerCareCostsPreVac) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/((sum(((model$ceCx16_18MortalityPostVac)-((model$ceCx16_18MortalityPreVac) * (1*(1-coverageAllDosis*vaccineEfficacyVsHPV16_18))))*(model$standardisedCohortSize_vacAge))) * cohortSizeAtVaccinationAgeFemale),
    "Razón de costo-efectividad incremental por Año de Vida Salvado (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVac) - ((model$cancerCareCostsPreVac) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/((sum(((model$lifeYearsLostPostVac) - ((model$lifeYearsLostPreVac) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))) * input$cohortSizeAtVaccinationAgeFemale),
    "Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitados (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVac) - ((model$cancerCareCostsPreVac) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/(((sum(((model$lifeYearsLostPostVac) - ((model$lifeYearsLostPreVac) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))) * input$cohortSizeAtVaccinationAgeFemale)+(sum(((model$cancerDisabilitiesLostPostVac) - ((model$cancerDisabilitiesLostPreVac) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge)) * input$cohortSizeAtVaccinationAgeFemale)),
    "PIB per Caápita"=GDPPerCapita,
    "Retorno de Inversión (%)" = ((cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVac) - ((model$cancerCareCostsPreVac) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge))-(input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis)))/(input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))*100
  )

  disc = c(
    "Tamaño de la cohorte de nacimiento (mujeres)"=input$birthCohortSizeFemale,
    "Tamaño de la cohorte en edad de vacunación (mujeres)"=input$cohortSizeAtVaccinationAgeFemale,
    "Costo total de la vacunación (USD)"=costoProg + (input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis)),
    "Costos de tratamiento ahorrados (USD)"= cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVacDisc) - ((model$cancerCareCostsPreVacDisc) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge),
    "Costo neto (USD)"=(input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVacDisc) - ((model$cancerCareCostsPreVacDisc) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)),
    "Cánceres de cuello uterino evitados (n)"=input$cohortSizeAtVaccinationAgeFemale * (sum(((model$ceCx16_18IncidencePreVac*(1-coverageBase * vaccineEfficacyVsHPV16_18))-(model$ceCx16_18IncidencePreVac*(1-coverageAllDosis * vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))),
    "Muertes evitadas (n)"=(sum(((model$ceCx16_18MortalityPostVac)-((model$ceCx16_18MortalityPreVac) * (1*(1-coverageAllDosis*vaccineEfficacyVsHPV16_18))))*(model$standardisedCohortSize_vacAge))) * cohortSizeAtVaccinationAgeFemale,
    "Años de vida salvados"=(sum(((model$lifeYearsLostPostVacDisc) - ((model$lifeYearsLostPreVacDisc) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))) * input$cohortSizeAtVaccinationAgeFemale,
    "Años de Vida Ajustados por Discapacidad evitados"=sum(((model$cancerDisabilitiesLostPostVacDisc) - ((model$cancerDisabilitiesLostPreVacDisc) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge)) * input$cohortSizeAtVaccinationAgeFemale,
    "Razón de costo-efectividad incremental por cáncer de cuello uterino prevenido (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVacDisc) - ((model$cancerCareCostsPreVacDisc) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/(input$cohortSizeAtVaccinationAgeFemale * (sum(((model$ceCx16_18IncidencePreVac*(1-coverageBase * vaccineEfficacyVsHPV16_18))-(model$ceCx16_18IncidencePreVac*(1-coverageAllDosis * vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge)))),
    "Razón de costo-efectividad incremental por vida salvada (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVacDisc) - ((model$cancerCareCostsPreVacDisc) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/((sum(((model$ceCx16_18MortalityPostVac)-((model$ceCx16_18MortalityPreVac) * (1*(1-coverageAllDosis*vaccineEfficacyVsHPV16_18))))*(model$standardisedCohortSize_vacAge))) * cohortSizeAtVaccinationAgeFemale),
    "Razón de costo-efectividad incremental por Año de Vida Salvado (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVacDisc) - ((model$cancerCareCostsPreVacDisc) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/((sum(((model$lifeYearsLostPostVacDisc) - ((model$lifeYearsLostPreVacDisc) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))) * input$cohortSizeAtVaccinationAgeFemale),
    "Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitados (USD)"=((input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))-(cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVacDisc) - ((model$cancerCareCostsPreVacDisc) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge)))/(((sum(((model$lifeYearsLostPostVacDisc) - ((model$lifeYearsLostPreVacDisc) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge))) * input$cohortSizeAtVaccinationAgeFemale)+(sum(((model$cancerDisabilitiesLostPostVacDisc) - ((model$cancerDisabilitiesLostPreVacDisc) * (1-coverageAllDosis*vaccineEfficacyVsHPV16_18)))*(model$standardisedCohortSize_vacAge)) * input$cohortSizeAtVaccinationAgeFemale)),
    "PIB per Caápita"=GDPPerCapita,
    "Retorno de inversión" = ((cohortSizeAtVaccinationAgeFemale * sum(((model$cancerCareCostsPostVacDisc) - ((model$cancerCareCostsPreVacDisc) * (1 - coverageAllDosis * vaccineEfficacyVsHPV16_18)))*standardisedCohortSize_vacAge))-(input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis)))/(input$totalVaccineCostPerFIG * input$cohortSizeAtVaccinationAgeFemale *(-coverageBase + coverageAllDosis))*100
  )
  
  names(disc) = names(undisc)
  
  outcomes = data.frame(
    outcomes = names(disc),
    undisc = undisc,
    disc = disc
  )
  
  rownames(outcomes) = c(1:nrow(outcomes))
  
  inputsNames = c(
    #"Tamaño de cohorte al nacimiento (mujeres)",
    "Tamaño de cohorte a la edad de vacunación (mujeres)",
    "Cobertura (todas las dosis)",
    "Eficacia de la vacuna frente al HPV 16/18",
    "Grupo de edad objetivo",
    "Precio de la vacuna por FIG",
    "Costo de entrega de la vacuna por FIG",
    "Costo total de la vacuna por FIG",
    "Costo del tratamiento del cáncer (por episodio, durante toda la vida)",
    "DALYs por diagnóstico de cáncer",
    "DALYs por secuelas no terminales del cáncer (por año)",
    "DALYs por cáncer terminal",
    "Tasa de descuento",
    #"Proporción de casos de cáncer cervical debidos al HPV 16/18",
    #"PIB per cápita",
    "Cobertura target (todas las dosis)"
  )
  
  inputsValues = c(
    #input$birthCohortSizeFemale,
    input$cohortSizeAtVaccinationAgeFemale,
    input$coverageAllDosis,
    input$vaccineEfficacyVsHPV16_18,
    input$targetAgeGroup,
    input$vaccinePricePerFIG,
    input$vaccineDeliveryCostPerFIG,
    input$totalVaccineCostPerFIG,
    input$cancerTreatmentCostPerEpisodeOverLifetime,
    input$DALYsForCancerDiagnosis,
    input$DALYsForNonTerminalCancerSequelaePperYear,
    input$DALYsForTerminalCancer,
    input$discountRate,
    #input$proportionOfCervicalCancerCasesThatAreDueToHPV16_18,
    #input$GDPPerCapita,
    input$coverageBase
  )
  
  inputsTable = data.frame(
    Input = inputsNames,
    Value = inputsValues
  )
  
  return(
    list(
      model=model,
      dataPlot=dataPlot,
      plot=plot,
      outcomes=outcomes,
      inputsTable = inputsTable
    )
  )
}

# resultados= getPrime(
#   country,
#   birthCohortSizeFemale,
#   cohortSizeAtVaccinationAgeFemale,
#   coverageAllDosis,
#   vaccineEfficacyVsHPV16_18,
#   targetAgeGroup,
#   vaccinePricePerFIG,
#   vaccineDeliveryCostPerFIG,
#   totalVaccineCostPerFIG,
#   cancerTreatmentCostPerEpisodeOverLifetime,
#   DALYsForCancerDiagnosis,
#   DALYsForNonTerminalCancerSequelaePperYear,
#   DALYsForTerminalCancer,
#   discountRate,
#   proportionOfCervicalCancerCasesThatAreDueToHPV16_18,
#   GDPPerCapita,
#   mortall,
#   mortcecx,
#   incidence,
#   dalys,
#   parameters
# )
