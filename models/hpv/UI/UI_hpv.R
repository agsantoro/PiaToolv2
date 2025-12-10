
ui_hpv_basica = function (input,inputs_hpv, run_hearts, hpv_map_inputs) {
  inputs_names = c(
    'Porcentaje de cobertura objetivo (esquema completo) (%)',
    'Costo de vacunación (esquema completo)',
    'Grupo de edad objetivo',
    'Tamaño de la cohorte en edad de vacunación (mujeres) (n)',
    'Porcentaje de cobertura basal (esquema completo)',
    'Eficacia de la vacuna contra el VPH 16/18 (%)',
    'Porcentaje de casos de cáncer de cuello de útero debidos al VPH 16/18',
    'Años de vida ajustados por discapacidad por diagnóstico de cáncer de cuello uterino',
    'Años de vida ajustados por discapacidad por secuelas de cáncer de cuello uterino',
    'Años de vida ajustados por discapacidad por cáncer de cuello uterino terminal',
    'Costos administrativos de la vacuna (esquema completo)',
    'Costo total de vacunación (esquema completo)',
    'Costo del tratamiento del cáncer',
    'Tasa de descuento (%)',
    'Costo programático anual de la intervención (USD)'
  )
  
  inputs_hover = c(
    'El porcentaje esperado de niñas en el grupo de edad relevante que recibirán el esquema completo de la vacuna luego de la intervención',
    'Costo de vacunación completa por niña para abril de 2023 (USD oficial a tasa de cambio nominal de cada país)',
    'Edad a la que normalmente se administran las vacunas contra el VPH. Tenga en cuenta que PRIME solo es adecuado para evaluar las vacunas contra el HPV administradas a niñas en las edades recomendadas por la OMS, de 9 a 13 años',
    'Número de mujeres en el país correspondientes a la edad de vacunación de rutina, definida por el "grupo de edad objetivo"',
    'Se refiere al porcentaje de mujeres que actualmente reciben vacuna contra el VPH',
    'Indica la reducción del riesgo de infecciones persistentes y lesiones precancerosas por los tipos 16 y 18 del VPH',
    'Porcentaje de casos de cáncer de cuello uterino que son atribuibles a las cepas 16 y 18 del VPH',
    'Miden la carga total del cáncer de cuello uterino combinando años de vida perdidos por muerte prematura y años vividos con discapacidad. Se recomienda consultar a un economista de la salud antes de cambiar este parámetro',
    'Miden la carga total de las secuelas de cáncer de cuello uterino combinando años de vida perdidos por muerte prematura y años vividos con discapacidad. Se recomienda consultar a un economista de la salud antes de cambiar este parámetro',
    'Miden la carga total del cáncer de cuello uterino terminal combinando años de vida perdidos por muerte prematura y años vividos con discapacidad. Se recomienda consultar a un economista de la salud antes de cambiar este parámetro',
    'Costo de administración, entrega y almacenamiento de la vacuna por niña completamente inmunizada para abril de 2023 (USD oficial a tasa de cambio nominal de cada país)',
    'Costo total (precio de la vacuna más el costo administrativo) por niña completamente inmunizada para abril de 2023 (USD oficial a tasa de cambio nominal de cada país)',
    'Costo promedio del tratamiento de un cáncer cervical a lo largo de la vida, expresado en dólares de abril de 2023 (USD oficial a tasa de cambio nominal de cada país)',
    'Se utiliza para traer al presente los costos y beneficios en salud futuros',
    'Costo de implementar y sostener la intervención en un año (USD oficial a tasa de cambio nominal de cada país)'
    #'PIB per capita'
    
  )
  
  
  if (is.null(input$country) == F) {
    parametersReactive <- function () {
      paramsList = list(
        coverageAllDosis = as.numeric(parameters[parameters$Country==input$country,23]),
        vaccinePricePerFIG = as.numeric(parameters[parameters$Country==input$country,14]),
        targetAgeGroup = as.numeric(parameters[parameters$Country==input$country,13]),
        cohortSizeAtVaccinationAgeFemale = cohortSizeAcVac$value[cohortSizeAcVac$country==input$country & cohortSizeAcVac$age == as.numeric(parameters[parameters$Country==input$country,13])],
        coverageBase = as.numeric(parameters[parameters$Country==input$country,11]),
        vaccineEfficacyVsHPV16_18 = as.numeric(parameters[parameters$Country==input$country,12]),
        proportionOfCervicalCancerCasesThatAreDueToHPV16_18 = as.numeric(parameters[parameters$Country==input$country,19]),
        DALYsForCancerDiagnosis = 0.288,
        DALYsForNonTerminalCancerSequelaePperYear = as.numeric(parameters[parameters$Country==input$country,22]),
        DALYsForTerminalCancer = 0.54,
        vaccineDeliveryCostPerFIG = as.numeric(parameters[parameters$Country==input$country,15]),
        totalVaccineCostPerFIG = as.numeric(parameters[parameters$Country==input$country,14])+as.numeric(parameters[parameters$Country==input$country,15]),
        cancerTreatmentCostPerEpisodeOverLifetime = as.numeric(parameters[parameters$Country==input$country,16]),
        discountRate = as.numeric(parameters[parameters$Country==input$country,18]),
        costoProg = 0
      )
      return(paramsList)
    }
    
    
    
    i_names = c()
    for (i in 1:15) {
      i_names = c(i_names,names(parametersReactive()[i]))
    }
    
    i_labels = c()
    
    for (i in 1:15) {
      i_labels = c(i_labels,inputs_names[i])
    }
    
    
    
    addData = data.frame(
      intervencion = "Vacuna contra el HPV",
      i_names,
      i_labels
    )
    
    
    bsc = c(1:2)
    avz = setdiff(seq(1,nrow(isolate(addData))),bsc)
    prc = c(1,5,6,7,14)
    
    addData$avanzado = NA
    addData$avanzado[avz] = T
    addData$avanzado[bsc] = F
    
    hpv_map_inputs(addData)
    
  }
  
  
  renderUI({
    
    tagList(
      br(),
      #tags$style(getStyle()),
      lapply(bsc, function (i) {
        if (!i %in% prc) {
          numericInput(input=names(parametersReactive())[i],
                       tags$div(
                         inputs_names[i],
                         icon("circle-info",
                              "fa-1x",
                              title = inputs_hover[i])
                       ),
                       value = parametersReactive()[[i]])
        } else {
          sliderInput(input=names(parametersReactive())[i],
                      label=tags$div(
                        inputs_names[i],
                        icon("circle-info",
                             "fa-1x",
                             title = inputs_hover[i])
                      ),
                      min = 0,
                      max= 100,
                      value=parametersReactive()[[i]]*100)}
        
      }),
      br(),
      
      
      
      tags$div(
        tags$button(
          class = "btn btn-default btn-block",
          `data-toggle` = "collapse",
          `data-target` = "#collapseInputs_hpv",
          style = "text-align: center; margin-bottom: 10px; position: relative; border: 2px solid #DEE2E6; font-size: 13px; background-color: white; border-radius: 6px;",
          "Parámetros avanzados",
          tags$span(
            icon("caret-down"),
            style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%);"
          )
        ),
        
        tags$div(
          id = "collapseInputs_hpv",
          class = "collapse",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;",
      lapply(avz, function (i) {
        if (!i %in% prc) {
          if (i==3) {
            numericInput(input=names(parametersReactive())[i],
                         tags$div(
                           inputs_names[i],
                           icon("circle-info", 
                                "fa-1x",
                                title = inputs_hover[i])
                         )
                         ,
                         value = parametersReactive()[[i]],
                         min = 8,
                         max = 15)
          } else {
            numericInput(input=names(parametersReactive())[i],
                         tags$div(
                           inputs_names[i],
                           icon("circle-info", 
                                "fa-1x",
                                title = inputs_hover[i])
                         )
                         ,
                         value = parametersReactive()[[i]])
          }
          
            
          
        } else {
          
            sliderInput(input=names(parametersReactive())[i],
                        label=tags$div(
                          inputs_names[i],
                          icon("circle-info", 
                               "fa-1x",
                               title = inputs_hover[i])
                        ),
                        min = 0,
                        max= 100,
                        value=parametersReactive()[[i]]*100)
            
          
        }
      })),
      hr(),
      tags$div(
        
        column(12,
               actionButton(
                 "hpv_go",
                 icon("play")),
               align = "right")
        
      )
      )
      
      
      
    )
    
  }) 
}

ui_grafico_nuevo_hpv = function(run_hpv, input, output) {
  if (length(input$coverageBase)>0) {
    browser()
    table = run_hpv$outcomes
    
    indicadores = c(
      'Años de Vida Ajustados por Discapacidad evitados (AVAD)',
      'Costo total de la intervención (USD)',
      'Diferencia de Costos respecto al escenario basal (USD)',
      'Retorno de Inversión (%)',
      'Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitados (USD)'
    )
    
    table = table[table$outcomes %in% indicadores,c("outcomes", "undisc")]
    colnames(table) = c("indicador","valor")
    
    renderUI({
      graf_esc(table,output)
    })
  }
}

ui_grafico_hpv = function (run_hpv, input) {
  
  renderHighchart({
    if (length(input$coverageBase)>0) {
      run_hpv$plot
    } else {
      NULL
    }
  })
  
}



ui_tabla_hpv = function (run_hpv, input) {
  if (length(input$coverageBase)>0) {
    table = run_hpv$outcomes
    table$disc = format(round(table$disc,1), nsmall = 1,big.mark = ".", decimal.mark = ",", scientific = FALSE)
    table$undisc = format(round(table$undisc,1), nsmall = 1,big.mark = ".", decimal.mark = ",", scientific = FALSE)
    
    colnames(table) = c("Outcomes", "Undiscounted", "Discounted")
    
    cat_epi = c(5:8)
    cat_costos = c(2,3,4,9,10,11,12,14)
    
    table$cat=""
    table$cat[cat_epi] = "Resultados epidemiológicos"
    table$cat[cat_costos] = "Resultados económicos"
    
    table = table[table$cat!="",]
    table = rbind(
      table[table$cat=="Resultados epidemiológicos",],
      table[table$cat=="Resultados económicos",]
    )
    
    table = table[,c("cat", "Outcomes","Undiscounted")]
    
    reactable(
      table,
      groupBy = "cat",
      defaultExpanded = T,
      pagination = F,
      defaultColDef = colDef(
        align = "center",
        minWidth = 70,
        headerStyle = list(background = "#236292", color = "white")
      ),
      columns = list(
        cat = colDef(name = "Categoría", align = "left"),
        Outcomes = colDef(name = "Indicador", align = "left"),
        Undiscounted = colDef(name = "Valor", align = "right")
      ),
      bordered = TRUE,
      highlight = TRUE
    )
    
    
  } else {NULL}
  
}


