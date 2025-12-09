library(dplyr)
library(tidyr)

##### carga baseline #####
base_line = read.xlsx("models/estimaTool/inputs/inputs2025.xlsx", sheet = "parameters") %>% 
  dplyr::filter(category == "base_line") %>% dplyr::select(parameter, country, value) %>%
  pivot_wider(names_from = parameter) %>% as.data.frame()

##### carga targets default #####
targets_default = read.xlsx("models/estimaTool/inputs/inputs2025.xlsx", sheet = "parameters") %>% 
  dplyr::filter(category == "targets_default") %>% dplyr::select(parameter, country, value) %>%
  pivot_wider(names_from = parameter) %>% as.data.frame()

##### carga poblacion #####
population = read.xlsx("models/estimaTool/inputs/inputs2025.xlsx", sheet = "parameters") %>% 
  dplyr::filter(category == "population") %>% dplyr::select(parameter, country, value) %>%
  pivot_wider(names_from = parameter) %>% as.data.frame()

##### carga costos #####
costs = read.xlsx("models/estimaTool/inputs/inputs2025.xlsx", sheet = "parameters") %>% 
  dplyr::filter(category == "costs") %>% dplyr::select(parameter, country, value) %>% as.data.frame()

##### carga fatality data #####
fatality_data = read.xlsx("models/estimaTool/inputs/inputs2025.xlsx", sheet = "table_fatality_data")[1:4]

##### carga life exp #####
life_exp = read.xlsx("models/estimaTool/inputs/inputs2025.xlsx", sheet = "table_life_exp")

##### carga mortality data #####
mortality_data = read.xlsx("models/estimaTool/inputs/inputs2025.xlsx", sheet = "table_mortality_data")


VA = function(tasa_descuento_anual,num_periodos) {
  va <- ((1 - (1 + tasa_descuento_anual) ^ (-num_periodos+1)) / tasa_descuento_anual)
  va <- (va + 1) * -1
  return(va)
  
}


epi_model <- function(run, x1, x2, country) {
  if (x1 >= 0 && x1 <= 100 && x2 >= 0 && x2 <= 100) {
    valores_x <- data.frame(prevalencia = c(x1, x2))  # Crea un nuevo dataframe con los valores de x
    y_pronosticado_modelo1 <- as.data.frame(exp((-0.0294 * valores_x) +5.5305))
    y_pronosticado_modelo2 <- as.data.frame(exp((-0.0240177 * valores_x) + 4.57206))
    resultados <- data.frame(
      Modelo1_x1 = y_pronosticado_modelo1[1,1],
      Modelo1_x2 = y_pronosticado_modelo1[2,1],
      Modelo2_x1 = y_pronosticado_modelo2[1,1],
      Modelo2_x2 = y_pronosticado_modelo2[2,1]
    )
    resultados$dif_1 <- resultados[,1]-resultados[,2]
    resultados$dif_2 <- resultados[,3]-resultados[,4]
    pob <- as.numeric(run[[country]]$population)
    resultados$dif_1_pob <- resultados$dif_1*pob/100000 
    resultados$dif_2_pob <- resultados$dif_2* pob/100000 
    return(resultados)
  } else {
    stop("Los valores de x deben estar entre 0 y 100")
  }
}

estimaToolCosts = function(
    country = input$country,
    Population,
    `BASELINE_Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad`,
    `TARGET_Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad`,
    `BASELINE_Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad`,
    `TARGET_Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad`,
    `BASELINE_Tratamiento entre los diagnosticados (%)`,
    `TARGET_Tratamiento entre los diagnosticados (%)`,
    `BASELINE_Control de la hipertensión entre los tratados (%)`,
    `TARGET_Control de la hipertensión entre los tratados (%)`,
    `Costo farmacológico anual por paciente promedio (**)`,
    `Evento de enfermedad cardiaca isquemica promedio  (***)`,
    `Costo anual de consulta médica en paciente promedio (*)`
) {
  country = str_to_title(country)
  
  # adding both sexes as rows
  mortality_data = 
    union_all(
      mortality_data,
      mortality_data %>% group_by(location,sex="Both",age,cause,metric,year) %>%
        summarise(val=sum(val))
    )
  
  # summarizing metrics
  mortality_data = mortality_data %>%
    left_join(
      mortality_data %>% group_by(location, sex, cause) %>%
        summarise(sum = sum(val))
    ) %>% mutate(proporcion = val/sum)
  
  
  ##### FATALITY DATA #####
  
  # wrangling data to calculated weighted fatality ratios 
  fatality_data = fatality_data %>% pivot_longer(names_to = "gender", cols = 3:4)
  
  male_ihd = mortality_data$val[mortality_data$location=="Brazil" & mortality_data$sex=="Male" & mortality_data$cause=="Ischemic heart disease"][-1]
  male_ihd = c(rep(male_ihd[1:9], each = 5),last(male_ihd))
  male_ihd_deaths = male_ihd
  male_ihd = male_ihd/sum(male_ihd)
  male_ihd = male_ihd * fatality_data$value[fatality_data$cause=="Ischemic heart disease" & fatality_data$gender=="male" & fatality_data$age %in% 35:80]
  
  female_ihd = mortality_data$val[mortality_data$location=="Brazil" & mortality_data$sex=="Female" & mortality_data$cause=="Ischemic heart disease"][-1]
  female_ihd = c(rep(female_ihd[1:9], each = 5),last(female_ihd))
  female_ihd_deaths = female_ihd
  female_ihd = female_ihd/sum(female_ihd)
  female_ihd = female_ihd * fatality_data$value[fatality_data$cause=="Ischemic heart disease" & fatality_data$gender=="female" & fatality_data$age %in% 35:80]
  
  male_stroke = mortality_data$val[mortality_data$location=="Brazil" & mortality_data$sex=="Male" & mortality_data$cause=="Stroke"][-1]
  male_stroke = c(rep(male_stroke[1:9], each = 5),last(male_stroke))
  male_stroke_deaths = male_stroke
  male_stroke = male_stroke/sum(male_stroke)
  male_stroke = male_stroke * fatality_data$value[fatality_data$cause=="Stroke" & fatality_data$gender=="male" & fatality_data$age %in% 35:80]
  
  female_stroke = mortality_data$val[mortality_data$location=="Brazil" & mortality_data$sex=="Female" & mortality_data$cause=="Stroke"][-1]
  female_stroke = c(rep(female_stroke[1:9], each = 5),last(female_stroke))
  female_stroke_deaths = female_stroke
  female_stroke = female_stroke/sum(female_stroke)
  female_stroke = female_stroke * fatality_data$value[fatality_data$cause=="Stroke" & fatality_data$gender=="female" & fatality_data$age %in% 35:80]
  
  fatality_weighted = cbind(
    age = 35:80,
    cause = c(rep("Ischemic heart disease", length(male_ihd) * 2), rep("Stroke", length(male_ihd) * 2)),
    gender = c(rep("Male", length(male_ihd)),rep("Female", length(male_ihd)),rep("Male", length(male_ihd)),rep("Female", length(male_ihd))),
    fatality_weighted = c(male_ihd,female_ihd,male_stroke,female_stroke),
    deaths = c(male_ihd_deaths,female_ihd_deaths,male_stroke_deaths,female_stroke_deaths)
  ) %>% as.data.frame()
  
  #### ESTIMA TOOL RUN #####
  run = list()
  run[[country]] = list()
  run[[country]]$population$`Población total` = as.numeric(Population)
  run[[country]]$baseline = list()
  run[[country]]$target = list()
  run[[country]]$baseline$`Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` = `BASELINE_Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` * base_line$modificador[base_line$country==country]
  run[[country]]$baseline$`Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` = `BASELINE_Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad`
  run[[country]]$baseline$`Tratamiento entre los diagnosticados (%)` = `BASELINE_Tratamiento entre los diagnosticados (%)`
  run[[country]]$baseline$`Control de la hipertensión entre los tratados (%)` = `BASELINE_Control de la hipertensión entre los tratados (%)`
  run[[country]]$target$`Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` = `TARGET_Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad`
  run[[country]]$target$`Tratamiento entre los diagnosticados (%)` = `TARGET_Tratamiento entre los diagnosticados (%)`
  run[[country]]$target$`Control de la hipertensión entre los tratados (%)` = `TARGET_Control de la hipertensión entre los tratados (%)`
  run[[country]]$baseline$`Prevalence of treatment (taking medicine) for hypertension among adults aged 30-79 with hypertension, age-standardized` = run[[country]]$baseline$`Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` * run[[country]]$baseline$`Tratamiento entre los diagnosticados (%)`
  run[[country]]$baseline$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized` = run[[country]]$baseline$`Prevalence of treatment (taking medicine) for hypertension among adults aged 30-79 with hypertension, age-standardized` * run[[country]]$baseline$`Control de la hipertensión entre los tratados (%)`
  run[[country]]$target$`Prevalence of treatment (taking medicine) for hypertension among adults aged 30-79 with hypertension, age-standardized` = run[[country]]$target$`Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` * run[[country]]$target$`Tratamiento entre los diagnosticados (%)`
  run[[country]]$target$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized` = run[[country]]$target$`Prevalence of treatment (taking medicine) for hypertension among adults aged 30-79 with hypertension, age-standardized` * run[[country]]$target$`Control de la hipertensión entre los tratados (%)`
  
  valor_x1 <- run[[country]]$baseline$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`*100
  valor_x2 <- run[[country]]$target$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`*100
  
  outcomesEpi = epi_model(
    run,
    round(valor_x1,1),
    round(valor_x2,1),
    country)
  
  run[[country]]$baseline$`Muertes por Eventos Coronarios por cada 100.000 habitantes que podrían evitarse` = outcomesEpi$dif_1
  run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios` = outcomesEpi$dif_1_pob
  run[[country]]$baseline$`Muertes por Accidente Cerebrovascular por cada 100.000 habitantes que podrían evitarse` = outcomesEpi$dif_2
  run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular` = outcomesEpi$dif_2_pob
  run[[country]]$baseline$`Población de adultos entre 30 y 79 años con hipertensión que inició tratamiento` = run[[country]]$population$`Población total` * run[[country]]$baseline$`Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` * run[[country]]$baseline$`Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` * run[[country]]$baseline$`Tratamiento entre los diagnosticados (%)`
  run[[country]]$baseline$`Población (N) de hipertensos controlados entre adultos de 30-79 años con hipertensión` = run[[country]]$baseline$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized` * run[[country]]$baseline$`Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` * run[[country]]$population$`Población total`
  run[[country]]$baseline$`Eventos Coronarios evitados` = run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios` / (sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  +  sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))) + sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  + sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))))
  run[[country]]$baseline$`Tasa de Eventos Coronarios evitados cada 100.000 habitantes` = run[[country]]$baseline$`Muertes por Eventos Coronarios por cada 100.000 habitantes que podrían evitarse` / (sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  +  sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))) + sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  + sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))))
  run[[country]]$baseline$`Accidente Cerebrovascular evitados` = run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular` / (sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"]))  +  sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"]))) + sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"]))  + sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"]))))
  run[[country]]$baseline$`Tasa de accidente cerebrovascular evitados cada 100.000 habitantes` = run[[country]]$baseline$`Muertes por Accidente Cerebrovascular por cada 100.000 habitantes que podrían evitarse` / (sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"]))  +  sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"]))) + sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Male"]))  + sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Stroke" & fatality_weighted$gender == "Female"]))))
  run[[country]]$target$`Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` = run[[country]]$baseline$`Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad`
  run[[country]]$target$`Muertes por Eventos Coronarios por cada 100.000 habitantes que podrían evitarse` = run[[country]]$baseline$`Muertes por Eventos Coronarios por cada 100.000 habitantes que podrían evitarse`
  run[[country]]$target$`Muertes evitadas por Eventos Coronarios` = run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios`
  run[[country]]$target$`Muertes por Accidente Cerebrovascular por cada 100.000 habitantes que podrían evitarse` = run[[country]]$baseline$`Muertes por Accidente Cerebrovascular por cada 100.000 habitantes que podrían evitarse`
  run[[country]]$target$`Muertes evitadas por Accidente Cerebrovascular` = run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular`
  run[[country]]$target$`Accidente Cerebrovascular evitados` = run[[country]]$baseline$`Accidente Cerebrovascular evitados`
  run[[country]]$target$`Tasa de accidente cerebrovascular evitados cada 100.000 habitantes` = run[[country]]$baseline$`Tasa de accidente cerebrovascular evitados cada 100.000 habitantes`
  run[[country]]$target$`Población de adultos entre 30 y 79 años con hipertensión que inició tratamiento` = run[[country]]$population$`Población total` * run[[country]]$target$`Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` * run[[country]]$target$`Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` * run[[country]]$target$`Tratamiento entre los diagnosticados (%)`
  run[[country]]$target$`Población (N) de hipertensos controlados entre adultos de 30-79 años con hipertensión` = run[[country]]$target$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized` * run[[country]]$target$`Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` * run[[country]]$population$`Población total`
  run[[country]]$target$`Eventos Coronarios evitados` = run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios` / (sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  +  sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))) + sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  + sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))))
  run[[country]]$target$`Tasa de Eventos Coronarios evitados cada 100.000 habitantes` = run[[country]]$target$`Muertes por Eventos Coronarios por cada 100.000 habitantes que podrían evitarse` / (sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  +  sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))) + sum(as.numeric(fatality_weighted$fatality_weighted[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) * sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"])) / (sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Male"]))  + sum(as.numeric(fatality_weighted$deaths[fatality_weighted$cause == "Ischemic heart disease" & fatality_weighted$gender == "Female"]))))
  run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` = run[[country]]$target$`Población de adultos entre 30 y 79 años con hipertensión que inició tratamiento` - run[[country]]$baseline$`Población de adultos entre 30 y 79 años con hipertensión que inició tratamiento`
  run[[country]]$target$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` =   run[[country]]$target$`Población (N) de hipertensos controlados entre adultos de 30-79 años con hipertensión` - run[[country]]$baseline$`Población (N) de hipertensos controlados entre adultos de 30-79 años con hipertensión`
  
  ###### DISABILITY WEIGHT #####
  
  disability_weight = list(
    "Ischemic heart disease"=0.0791335,
    "Stroke"=0.1057003333)
  
  
  ##### EXPECTATION OF LIFE #####
  
  
  ##### averted events #####
  
  averted_by_age = data.frame()
  
  for ( e in c("Deaths", "Events")) {
    for (c in c("Ischemic heart disease","Stroke")) {
      for (i in unique(mortality_data$age)) {
        cause = c 
        country = country
        age=i
        gender="Male"
        event = e
        
        if (e == "Deaths") {
          absolute = if (c=="Stroke") {"Muertes evitadas por Accidente Cerebrovascular"} else {"Muertes evitadas por Eventos Coronarios"}
        } else {
          absolute = if (c=="Stroke") {"Accidente Cerebrovascular evitados"} else {"Eventos Coronarios evitados"}
        }
        
        prop = list(
          Male = sum(mortality_data$val[mortality_data$location==country & mortality_data$sex == "Male" & mortality_data$cause == cause]) / (sum(mortality_data$val[mortality_data$location==country & mortality_data$sex == "Male" & mortality_data$cause == cause]) + sum(mortality_data$val[mortality_data$location==country & mortality_data$sex == "Female" & mortality_data$cause == cause])),
          Female = sum(mortality_data$val[mortality_data$location==country & mortality_data$sex == "Female" & mortality_data$cause == cause]) / (sum(mortality_data$val[mortality_data$location==country & mortality_data$sex == "Male" & mortality_data$cause == cause]) + sum(mortality_data$val[mortality_data$location==country & mortality_data$sex == "Female" & mortality_data$cause == cause]))
        )
        
        dist = mortality_data$proporcion[mortality_data$location==country & mortality_data$sex == gender & mortality_data$cause == cause & mortality_data$age == age]
        deaths_esi_averted_male = run[[country]]$baseline[[absolute]] * prop[[gender]] * dist
        
        gender="Female"
        dist = mortality_data$proporcion[mortality_data$location==country & mortality_data$sex == gender & mortality_data$cause == cause & mortality_data$age == age]
        deaths_esi_averted_female = run[[country]]$baseline[[absolute]] * prop[[gender]] * dist
        
        add = data.frame(
          event,
          age= i,
          cause,
          Male = deaths_esi_averted_male,
          Female = deaths_esi_averted_female
        )
        averted_by_age = rbind(averted_by_age,add)
      }
    }
  }
  
  
  averted_by_age = averted_by_age %>% pivot_longer(names_to = "gender", cols = c("Male","Female")) %>% arrange(event,cause,gender)
  
  dalys_by_age = averted_by_age[averted_by_age$event=="Deaths",] %>% left_join(life_exp[life_exp$location==country,]) %>%
    mutate(yll=value*lex,
           deaths = value) %>%
    dplyr::select(cause,age,gender,deaths, yll) %>%
    left_join(averted_by_age[averted_by_age$event=="Events",]) %>%
    #left_join(averted_by_age[averted_by_age$event=="Deaths",]) %>%
    mutate(
      dalys=
        case_when(cause == "Ischemic heart disease" ~ (value-deaths)*disability_weight[[1]],
                  cause == "Stroke" ~ (value-deaths)*disability_weight[[2]]),
      events=value) %>% 
    dplyr::select(cause,age,gender,deaths, events,yll,dalys) %>% 
    arrange(cause,gender,age)
  
  dalys_overall = dalys_by_age %>% group_by(age,gender) %>%
    summarise(yll=sum(yll),dalys=sum(dalys)) %>%
    mutate(dalys_overall=yll+dalys) %>% arrange(gender,age)
  
  dalys_by_age_disc =dalys_by_age %>% left_join(
    life_exp[life_exp$location==country,]
  )
  
  dalys_by_age_disc$disc = dalys_by_age_disc$deaths *  VA(0.05,dalys_by_age_disc$lex) * -1
  
  
  epi_outcomes = data.frame(
    outcome = "Años de vida ajustados por discapacidad evitados",
    value = sum(dalys_overall$dalys_overall)
  )
  
  
  ##### epi-model params #####
  
  epi_model_y = c(
    y1 = run[[country]]$baseline$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`,
    y2 = run[[country]]$target$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`
  )
  
  ##### COSTS #####
  
  
  costs = costs[costs$country==country,]
  
  
  for (i in 1:nrow(costs)) {
    eval(parse(text = paste0("`",costs$parameter[i],"`=",costs$value[i])))
  }
  
  # costs_outcomes = list(
  #   `Costo anual de la intervención por paciente promedio  hipertenso tratado` = `Costo anual de consulta médica en paciente promedio (*)` + `Costo farmacológico anual por paciente promedio (**)`,
  #   `Costos totales anuales de la intervención` = run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`,
  #   `Costos médicos directos evitados por evento cardiovasculares (ECI y ACV)` = run[[country]]$baseline$`Eventos Coronarios evitados` * costs$value[costs$parameter=="Evento de enfermedad cardiaca isquemica promedio  (***)"] + costs$value[costs$parameter=="Evento de accidente cerebrovascular"] * run[[country]]$target$`Accidente Cerebrovascular evitados`,
  #   `Diferencia de costos` = (run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`)-(run[[country]]$baseline$`Eventos Coronarios evitados` * round(`Evento de enfermedad cardiaca isquemica promedio  (***)`,3) + round(`Evento de accidente cerebrovascular`,3) * run[[country]]$baseline$`Accidente Cerebrovascular evitados`),
  #   `Razon incremental de costo por evento de ECI evitado` = ((run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`)-(run[[country]]$baseline$`Eventos Coronarios evitados` * round(`Evento de enfermedad cardiaca isquemica promedio  (***)`,3) + round(`Evento de accidente cerebrovascular`,3) * run[[country]]$baseline$`Accidente Cerebrovascular evitados`)) / (run[[country]]$baseline$`Eventos Coronarios evitados`),
  #   `Razon incremental de costo por muerte de ECI evitada`= ((run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`)-(run[[country]]$baseline$`Eventos Coronarios evitados` * round(`Evento de enfermedad cardiaca isquemica promedio  (***)`,3) + round(`Evento de accidente cerebrovascular`,3) * run[[country]]$baseline$`Accidente Cerebrovascular evitados`)) / (run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios`),
  #   `Razon incremental de costo por evento de ACV evitado` = ((run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`)-(run[[country]]$baseline$`Eventos Coronarios evitados` * round(`Evento de enfermedad cardiaca isquemica promedio  (***)`,3) + round(`Evento de accidente cerebrovascular`,3) * run[[country]]$baseline$`Accidente Cerebrovascular evitados`)) / (run[[country]]$baseline$`Accidente Cerebrovascular evitados`),
  #   `Razon incremental de costo por muerte de ACV evitada` = ((run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`)-(run[[country]]$baseline$`Eventos Coronarios evitados` * round(`Evento de enfermedad cardiaca isquemica promedio  (***)`,3) + round(`Evento de accidente cerebrovascular`,3) * run[[country]]$baseline$`Accidente Cerebrovascular evitados`)) / (run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular`),
  #   `Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado` = ((run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`)-(run[[country]]$baseline$`Eventos Coronarios evitados` * round(`Evento de enfermedad cardiaca isquemica promedio  (***)`,3) + round(`Evento de accidente cerebrovascular`,3) * run[[country]]$baseline$`Accidente Cerebrovascular evitados`)) / (as.numeric(epi_outcomes$value[epi_outcomes$outcome == "Años de vida ajustados por discapacidad evitados"])),
  #   `Retorno de inversión` = ((run[[country]]$baseline$`Eventos Coronarios evitados` * round(`Evento de enfermedad cardiaca isquemica promedio  (***)`,3) + round(`Evento de accidente cerebrovascular`,3) * run[[country]]$baseline$`Accidente Cerebrovascular evitados`) - (run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`))/(run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`)*100
  # )
  
  costs_outcomes = list()
  costs_outcomes$`Costo anual de la intervención por paciente promedio  hipertenso tratado` = `Costo anual de consulta médica en paciente promedio (*)` + `Costo farmacológico anual por paciente promedio (**)`
  costs_outcomes$`Costos totales anuales de la intervención` = run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo anual de consulta médica en paciente promedio (*)` + run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados` * `Costo farmacológico anual por paciente promedio (**)`
  costs_outcomes$`Costos médicos directos evitados por evento cardiovasculares (ECI y ACV)` = run[[country]]$baseline$`Eventos Coronarios evitados` * costs$value[costs$parameter=="Evento de enfermedad cardiaca isquemica promedio  (***)"] + costs$value[costs$parameter=="Evento de accidente cerebrovascular"] * run[[country]]$target$`Accidente Cerebrovascular evitados`
  costs_outcomes$`Diferencia de costos` = costs_outcomes$`Costos totales anuales de la intervención`- costs_outcomes$`Costos médicos directos evitados por evento cardiovasculares (ECI y ACV)`
  costs_outcomes$`Razon incremental de costo por evento de ECI evitado` = costs_outcomes$`Diferencia de costos` / run[[country]]$baseline$`Eventos Coronarios evitados`
  costs_outcomes$`Razon incremental de costo por evento de ACV evitado` = costs_outcomes$`Diferencia de costos` / run[[country]]$baseline$`Accidente Cerebrovascular evitados`
  costs_outcomes$`Razon incremental de costo por muerte evitada` = costs_outcomes$`Diferencia de costos` / (run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios` + run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular`)
  costs_outcomes$`Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado` = costs_outcomes$`Diferencia de costos` /  sum(dalys_overall$dalys_overall)
  costs_outcomes$`Razon de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad descontado evitado` = costs_outcomes$`Diferencia de costos` / (sum(dalys_by_age_disc$disc) + sum(dalys_by_age_disc$dalys))
  costs_outcomes$`Razón de costo-efectividad incremental por año de vida salvado` = costs_outcomes$`Diferencia de costos` / sum(dalys_by_age$yll)
  costs_outcomes$`Razon de costo-efectividad incremental por año de vida salvado descontado` = costs_outcomes$`Diferencia de costos` / sum(dalys_by_age_disc$disc) 
  costs_outcomes$`Retorno de inversión (%)` = 100*(costs_outcomes$`Costos médicos directos evitados por evento cardiovasculares (ECI y ACV)`- costs_outcomes$`Costos totales anuales de la intervención`) / costs_outcomes$`Costos totales anuales de la intervención`
  
  resumen_resultados = 
    data.frame(
      indicador = c(
        'Eventos Coronarios evitados (n)',
        'Tasa de Eventos Coronarios evitados cada 100.000 habitantes',
        'Accidente Cerebrovascular evitados (n)',
        'Tasa de accidente cerebrovascular evitados cada 100.000 habitantes',
        'Muertes evitadas por Eventos Coronarios (n)',
        'Muertes por Eventos Coronarios por cada 100.000 habitantes que podrían evitarse',
        'Muertes evitadas por Accidente Cerebrovascular (n)',
        'Muertes por Accidente Cerebrovascular por cada 100.000 habitantes que podrían evitarse',
        'Muertes evitadas (n)',
        'Muertes totales por cada 100.000 habitantes que podrían evitarse',
        'Población de adultos entre 30 y 79 años con hipertensión que inició tratamiento (n)',
        'Años de vida ajustados por discapacidad evitados',
        'Años de vida salvados',
        'Costos totales de la intervención (USD)',
        'Costos evitados atribuibles a la intervención (USD)',
        'Diferencia de costos respecto al escenario basal (USD)',
        'Razón de costo-efectividad incremental por Año de Vida Salvado (USD)',
        'Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)',
        'Razón de costo-efectividad incremental por vida salvada (USD)',
        'Retorno de inversión (%)'  
      ),
      valor = c(
        run[[country]]$baseline$`Eventos Coronarios evitados`,
        run[[country]]$baseline$`Eventos Coronarios evitados` / Population * 100000,
        run[[country]]$baseline$`Accidente Cerebrovascular evitados`,
        run[[country]]$baseline$`Accidente Cerebrovascular evitados` / Population * 100000,
        run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios`,
        run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios` / Population * 100000,
        run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular`,
        run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular` / Population * 100000,
        run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular` + run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios`,
        (run[[country]]$baseline$`Muertes evitadas por Accidente Cerebrovascular` + run[[country]]$baseline$`Muertes evitadas por Eventos Coronarios`) / Population * 100000,
        run[[country]]$baseline$`Nueva población (N) tratada / Controlados actualmente previamente no controlados`,
        (sum(dalys_by_age_disc$disc) + sum(dalys_by_age_disc$dalys)),
        sum(dalys_by_age$yll),
        costs_outcomes$`Costos totales anuales de la intervención`,
        costs_outcomes$`Costos médicos directos evitados por evento cardiovasculares (ECI y ACV)`,
        costs_outcomes$`Diferencia de costos`,
        costs_outcomes$`Razón de costo-efectividad incremental por año de vida salvado`,
        costs_outcomes$`Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado`,
        costs_outcomes$`Razon incremental de costo por muerte evitada`,
        costs_outcomes$`Retorno de inversión (%)`
      )
    )
  
  estimaToolModel = list(
    country = country,
    mortality_data = mortality_data,
    fatality_data = fatality_data,
    run = run,
    life_exp = life_exp,
    fatality_data = fatality_data,
    fatality_weighted = fatality_weighted,
    averted_by_age = averted_by_age,
    dalys_by_age = dalys_by_age,
    dalys_overall = dalys_overall,
    epi_outcomes = epi_outcomes,
    costs_outcomes = costs_outcomes,
    epi_model_y = epi_model_y,
    epi_model_outcomes = outcomesEpi,
    resumen_resultados = resumen_resultados
  )
  return(estimaToolModel)
}

#resultado = estimaToolCosts()