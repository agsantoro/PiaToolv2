ui_inputs_multiComp = function (input, saved_scenarios, current_page, getCountryCode) {
  if (get_page() != "multiComp") { return() }
  
  if (length(saved_scenarios) > 1 & length(current_page) > 0) {
    
    # 1. Extraer modelos únicos
    models = unique(unlist(lapply(saved_scenarios, function(i) i[["model"]])))
    nModels = length(models)
    
    # 2. Mapear nombres a códigos de país
    choices_by_model = split(names(saved_scenarios), 
                             unlist(lapply(saved_scenarios, function(i) i$model)))
    
    # 3. Crear la lista de opciones con HTML procesado
    choices_with_flags <- lapply(names(choices_by_model), function(model_key) {
      scenario_ids <- choices_by_model[[model_key]]
      
      # Crear vector: HTML_LABEL = ID_VALUE
      opciones <- sapply(scenario_ids, function(id) {
        country_raw = saved_scenarios[[id]]$country
        countryCode <- tolower(getCountryCode(country_raw))
        
        if (!is.null(countryCode) && countryCode != "" && nchar(countryCode) > 0) {
          bandera_url <- sprintf("https://flagcdn.com/w20/%s.png", countryCode)
          
          # HTML como STRING (sin HTML())
          label_html <- sprintf(
            '<img src="%s" width="20" style="margin-right:8px;vertical-align:middle;"><span>%s</span>',
            bandera_url, 
            htmltools::htmlEscape(id)
          )
          return(label_html)
        } else {
          return(id)
        }
      }, USE.NAMES = FALSE)
      
      # setNames( VALORES , ETIQUETAS_HTML )
      res <- setNames(scenario_ids, opciones)
      return(res)
    })
    
    # 4. Traducir nombres de los grupos (Modelos)
    model_titles <- sapply(names(choices_by_model), function(x) {
      switch(x,
             "hearts" = "Iniciativa HEARTS",
             "hpv"    = "Vacunación contra el VPH",
             "tbc"    = "VDOT para Tuberculosis",
             "hepC"   = "Tratamiento para la hepatitis C crónica",
             "hpp"    = "Uso de oxitocina para la prevención de la hemorragia post parto",
             "prep"   = "Profilaxis pre exposición (PrEP) para VIH",
             "sifilis"= "Tests rápidos en punto de cuidado para sífilis gestacional",
             "naat"   = "Pruebas de amplificación de ácidos nucleicos (NAAT) para TBC",
             x)
    }, USE.NAMES = FALSE)
    
    names(choices_with_flags) <- model_titles
    
    # 5. Renderizar el input
    dragDropInput(
      inputId = "selectScenariosMulti",
      label = "Seleccione los escenarios a comparar",
      choices = choices_with_flags,
      n_cuadros = nModels
    )
    
  } else {
    return()
  }
}


ui_resultados_multiComp = function(input,output,session,current_page, saved_scenarios, selectedScenarios) {
  
  if (get_page()!="multiComp") {return()} else {
    
    if (is.null(input$selectScenariosMulti)) {return()}
    
    
    SUMMARY = lapply(seq_along(saved_scenarios), function (i) {
      if (saved_scenarios[[i]]$model == "hearts") {
        
        data = saved_scenarios[[i]]$outputs
        list(
          "Name" = names(saved_scenarios[i]),
          "Country" = saved_scenarios[[i]]$country,
          "Model" = saved_scenarios[[i]]$model,
          "AVAD" = data$Valor[data$Indicador == "Años de vida ajustados por discapacidad evitados"],
          "COSTO_TOTAL" = data$Valor[data$Indicador == "Costos totales de la intervención (USD)"],
          "DIF_COSTO" = data$Valor[data$Indicador == "Diferencia de costos respecto al escenario basal (USD)"],
          "RCEI_AVAD" = data$Valor[data$Indicador == "Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)"],
          "ROI" = data$Valor[data$Indicador == "Retorno de inversión (%)"]
        )
        
      } else if (saved_scenarios[[i]]$model == "hpv") {
        data = saved_scenarios[[i]]$outputs
        list(
          "Name" = names(saved_scenarios[i]),
          "Country" = saved_scenarios[[i]]$country,
          "Model" = saved_scenarios[[i]]$model,
          "AVAD" = data$Undiscounted[data$Outcomes == "Años de Vida Ajustados por Discapacidad evitados (AVAD)"],
          "COSTO_TOTAL" = data$Undiscounted[data$Outcomes == "Costo total de la intervención (USD)"],
          "DIF_COSTO" = data$Undiscounted[data$Outcomes == "Diferencia de Costos respecto al escenario basal (USD)"],
          "RCEI_AVAD" = data$Undiscounted[data$Outcomes == "Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitados (USD)"],
          "ROI" = data$Undiscounted[data$Outcomes == "Retorno de Inversión (%)"])
      } else if (saved_scenarios[[i]]$model == "tbc") {
        data = saved_scenarios[[i]]$outputs
        
        list(
          "Name" = names(saved_scenarios[i]),
          "Country" = saved_scenarios[[i]]$country,
          "Model" = saved_scenarios[[i]]$model,
          "AVAD" = data$vDOT[data$Parametro == "Años de vida ajustados por discapacidad evitados"],
          "COSTO_TOTAL" = data$vDOT[data$Parametro == "Costo total de la intervención (USD)"],
          "DIF_COSTO" = data$vDOT[data$Parametro == "Diferencia de costos respecto al escenario basal (USD)"],
          "RCEI_AVAD" = data$vDOT[data$Parametro == "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido"],
          "ROI" = data$vDOT[data$Parametro == "Retorno de Inversión (%)"])
      } else if (saved_scenarios[[i]]$model == "hepC") {
        data = saved_scenarios[[i]]$outputs
        list(
          "Name" = names(saved_scenarios[i]),
          "Country" = saved_scenarios[[i]]$country,
          "Model" = saved_scenarios[[i]]$model,
          "AVAD" = data$Valor[data$Indicador == "Años de Vida Ajustados por Discapacidad evitados"],
          "COSTO_TOTAL" = data$Valor[data$Indicador == "Costo total de la intervención (USD)"],
          "DIF_COSTO" = data$Valor[data$Indicador == "Diferencia de costos respecto al escenario basal (USD)"],
          "RCEI_AVAD" = data$Valor[data$Indicador == "Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitada (USD)"],
          "ROI" = data$Valor[data$Indicador == "Retorno de Inversión (%)"])
      } else if (saved_scenarios[[i]]$model == "hpp") {
        data = saved_scenarios[[i]]$outputs
        
        list(
          "Name" = names(saved_scenarios[i]),
          "Country" = saved_scenarios[[i]]$country,
          "Model" = saved_scenarios[[i]]$model,
          "AVAD" = data$valor[data$indicador == "Años de vida ajustados por discapacidad evitados"],
          "COSTO_TOTAL" = data$valor[data$indicador == "Costo total de la intervención (USD)"],
          "DIF_COSTO" = data$valor[data$indicador == "Diferencia de costos respecto al escenario basal (USD)"],
          "RCEI_AVAD" = data$valor[data$indicador == "Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)"],
          "ROI" = data$valor[data$indicador == "Retorno de Inversión (%)"])
      } else if (saved_scenarios[[i]]$model == "prep") {
        data = saved_scenarios[[i]]$outputs
        
        list(
          "Name" = names(saved_scenarios[i]),
          "Country" = saved_scenarios[[i]]$country,
          "Model" = saved_scenarios[[i]]$model,
          "AVAD" = data$Valor[data$Parametro == "Años de vida ajustados por discapacidad evitados"],
          "COSTO_TOTAL" = data$Valor[data$Parametro == "Costo total de la intervención (USD)"],
          "DIF_COSTO" = data$Valor[data$Parametro == "Diferencia de costos respecto al escenario basal (USD)"],
          "RCEI_AVAD" = data$Valor[data$Parametro == "Razón de costo-efectividad incremental (RCEI) por Años de Vida Ajustados por Discapacidad (AVAD) Evitados"],
          "ROI" = data$Valor[data$Parametro == "Retorno de Inversión (ROI) (%)"])
      } else if (saved_scenarios[[i]]$model == "sifilis") {
        data = saved_scenarios[[i]]$outputs
        list(
          "Name" = names(saved_scenarios[i]),
          "Country" = saved_scenarios[[i]]$country,
          "Model" = saved_scenarios[[i]]$model,
          "AVAD" = data$Valor[data$Indicador == "Años de Vida Ajustados por Discapacidad Evitados"],
          "COSTO_TOTAL" = data$Valor[data$Indicador == "Costo Total de la Intervención (USD)"],
          "DIF_COSTO" = data$Valor[data$Indicador == "Diferencia de costos respecto al escenario basal (USD)"],
          "RCEI_AVAD" = data$Valor[data$Indicador == "Razón de costo-efectividad incremental por año de vida ajustado por discapacidad evitado (USD)"],
          "ROI" = data$Valor[data$Indicador == "Retorno de Inversión (%)"])
      }
    })
    
    summaryTable = bind_rows(SUMMARY)
    compa <- summaryTable
    colnames(compa)[1:3] = c("scenarioName","country","intervencion")
    compa = compa %>% pivot_longer(
      cols = 4:8,
      names_to = "indicador",
      values_to = "value"
    )
    
    compa$value = str_replace_all(compa$value, "\\$", "")
    
    compa = compa %>%
      mutate(Intervencion_escenario = paste0(intervencion,'<br>',"(",scenarioName,")" )) %>%
      dplyr::filter(scenarioName %in% input$selectScenariosMulti)
    
    compaBox = compa %>%
      dplyr::filter(!indicador %in% c("RCEI_AVAD","ROI"))
    
    compa$value = str_replace_all(compa$value, "\\.", "")
    compa$value = as.numeric(str_replace_all(compa$value, ",", "\\."))
    
    compa$indicador[compa$indicador=="AVAD"] = "Años de vida ajustados por discapacidad evitados"
    compa$indicador[compa$indicador=="COSTO_TOTAL"] = "Costo total de la intervención (USD)"
    compa$indicador[compa$indicador=="DIF_COSTO"] = "Diferencia de costos respecto al escenario basal (USD)"
    compa$indicador[compa$indicador=="ROI"] = "Retorno de Inversión (%)"
    compa$indicador[compa$indicador=="RCEI_AVAD"] = "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido"
    
    compaBox$indicador[compaBox$indicador=="AVAD"] = "Años de vida ajustados por discapacidad evitados"
    compaBox$indicador[compaBox$indicador=="COSTO_TOTAL"] = "Costo total de la intervención (USD)"
    compaBox$indicador[compaBox$indicador=="DIF_COSTO"] = "Diferencia de costos respecto al escenario basal (USD)"
    compaBox$indicador[compaBox$indicador=="ROI"] = "Retorno de Inversión (%)"
    compaBox$indicador[compaBox$indicador=="RCEI_AVAD"] = "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido"
    
    
    
    # Colores de fondo para cada gráfico
    background_colors <- c("#E3F2FD", "#E8F5E9", "#FFF3E0", "#F3E5F5", "#E0F2F1")
    unique_indicators <- unique(compa$indicador)
    
    #  gráficos
    list_of_plots <- lapply(seq_along(unique(compaBox$indicador)), function(idx) {
      indicador <- unique_indicators[idx]
      data_subset <- dplyr::filter(compaBox, indicador == !!indicador)
      data_subset$value = str_replace_all(data_subset$value,"\\.","") 
      data_subset$value = as.numeric(str_replace_all(data_subset$value,",","\\.") )
      #data_subset$value = as.numeric(gsub("\\.", "", gsub(",", ".", data_subset$value)))
      
      chart <- hchart(data_subset, "bar", hcaes(x = Intervencion_escenario, y = value, name = intervencion)) %>%
        hc_chart(backgroundColor = background_colors[idx %% length(background_colors) + 1]) %>% # Establecer color de fondo
        hc_title(text = paste("Indicador:", indicador),
                 style = list(fontSize = "14px")) %>%
        hc_plotOptions(series = list(
          color = '#596775', # Configurar el color de las barras a negro
          dataLabels = list(
            enabled = TRUE,
            format = '{point.y}',  # Usar el nombre de la opción de punto para la etiqueta
            color = 'black', # Cambiar el color del texto a negro
            align = 'right', # Alinear a la derecha (fuera de la barra)
            inside = FALSE, # Asegurar que la etiqueta esté fuera de la barra
            verticalAlign = 'middle', # Alinear verticalmente en el medio
            y = 0, # Ajustar posición vertical
            x = 5  # Ajustar posición horizontal (un poco a la derecha de la barra)
          )
        )) %>%
        hc_xAxis(title = list(text = "Escenario selecionado"),
                 categories=data_subset$Intervencion_escenario) %>%
        hc_yAxis(title = list(text = ""),
                 opposite = TRUE,
                 plotLines = list(list(
                   value = 0,
                   color = 'white',
                   width = 2 # Puedes ajustar el grosor de la línea aquí
                 )) )
      chart
    })
    #     
    #     # cuadrícula
    #     
    #     
    # hw_grid(list_of_plots, rowheight = 240, ncol=5, add_htmlgrid_css = F) %>%
    #   htmltools::browsable()
    # 
    #     
    #     
    output$grafico_multiple1 = renderHighchart({list_of_plots[[1]]})
    output$grafico_multiple2 = renderHighchart({list_of_plots[[2]]})
    output$grafico_multiple3 = renderHighchart({list_of_plots[[3]]})
    output$grafico_multiple4 = renderHighchart({list_of_plots[[4]]})
    output$grafico_multiple5 = renderHighchart({list_of_plots[[5]]})
    
    
    output$tabla_escenarios_guardados = renderReactable({
      table_data = data.frame(
        scenarioName = paste0(compa$scenarioName," (",compa$country," / ",compa$intervencion),
        indicador = compa$indicador,
        value = compa$value
      ) 
      
      reactable(
        table_data,
        defaultExpanded = T,
        groupBy = "scenarioName",
        pagination = F,
        columns = list(
          scenarioName = colDef(name = "Escenario guardado", align = "left"),
          indicador = colDef(name = "Indicador", align = "left"),
          value = colDef(name = "Valor", align = "right")
        ),
        defaultColDef = colDef(
          headerStyle = list(background = "#236292", color = "white", borderWidth = "0")
        )
        
      )
      
      
      
    })
    
    
    compaBox$value = str_replace_all(compaBox$value,"\\.","") 
    compaBox$value = as.numeric(str_replace_all(compaBox$value,",","\\.") )
    
    #output$infoBoxAVAD = renderUI({
    
    best = first(max(compaBox$value[compaBox$indicador=="Años de vida ajustados por discapacidad evitados"]))
    nombre_scn = first(compaBox$scenarioName[compaBox$indicador == "Años de vida ajustados por discapacidad evitados" & compaBox$value == best])
    hito = "Mayor cantidad de AVAD salvados"
    valor = format(round(best,1),big.mark=".",decimal.mark=",")
    intervencion = getModelName(first(compaBox$intervencion[compaBox$indicador == "Años de vida ajustados por discapacidad evitados" & compaBox$value == best]))
    
    ib1 = infoBox(
      nombre_scn = nombre_scn,
      hito = hito,
      valor = valor,
      intervencion = intervencion)
    
    #})
    
    #output$infoBoxCostoTotal = renderUI({
    best = first(min(compaBox$value[compaBox$indicador=="Costo total de la intervención (USD)"]))
    nombre_scn = first(compaBox$scenarioName[compaBox$indicador == "Costo total de la intervención (USD)" & compaBox$value == best])
    hito = "Menor costo total de la intervención (%)"
    valor = format(round(best,1),big.mark=".",decimal.mark=",")
    intervencion = getModelName(first(compaBox$intervencion[compaBox$indicador == "Costo total de la intervención (USD)" & compaBox$value == best]))
    
    ib2 = infoBox(
      nombre_scn = nombre_scn,
      hito = hito,
      valor = valor,
      intervencion = intervencion)
    
    #})
    
    #output$infoBoxDiferenciaCosto = renderUI({
    best = first(min(compaBox$value[compaBox$indicador=="Diferencia de costos respecto al escenario basal (USD)"]))
    nombre_scn = first(compaBox$scenarioName[compaBox$indicador == "Diferencia de costos respecto al escenario basal (USD)" & compaBox$value == best])
    hito = "Menor diferencia de costo respecto del escenario basal (%)"
    valor = format(round(best,1),big.mark=".",decimal.mark=",")
    intervencion = getModelName(first(compaBox$intervencion[compaBox$indicador == "Diferencia de costos respecto al escenario basal (USD)" & compaBox$value == best]))
    
    ib3 = infoBox(
      nombre_scn = nombre_scn,
      hito = hito,
      valor = valor,
      intervencion = intervencion)
    
    #})
    
    #output$infoBoxROI = renderUI({
    # best = max(compa$value[compa$indicador=="Retorno de Inversión (%)"])
    # nombre_scn = compa$scenarioName[compa$indicador == "Retorno de Inversión (%)" & compa$value == best]
    # hito = "Mayor retorno de inversión (%)"
    # valor = format(round(best,1),big.mark=".",decimal.mark=",")
    # intervencion = compa$intervencion[compa$indicador == "Retorno de Inversión (%)" & compa$value == best]
    # 
    # ib4 = infoBox(
    #   nombre_scn = nombre_scn,
    #   hito = hito,
    #   valor = valor,
    #   intervencion = intervencion)
    
    #})
    
    #output$infoBoxRCEIAVAD = renderUI({
    # best = min(compa$value[compa$indicador=="Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido"])
    # nombre_scn = compa$scenarioName[compa$indicador == "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido" & compa$value == best]
    # hito = "Menor razón de costo incremental por AVAD evitado (%)"
    # valor = format(round(best,1),big.mark=".",decimal.mark=",")
    # intervencion = compa$intervencion[compa$indicador == "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido" & compa$value == best]
    # 
    # ib5 = infoBox(
    #   nombre_scn = nombre_scn,
    #   hito = hito,
    #   valor = valor,
    #   intervencion = intervencion)
    
    #})
    
    
    infoboxes = list(
      ib1,ib2,ib3)
    
    
    output$carouselInfoBoxes = renderUI({
      infoBoxCarousel(infoboxes, carousel_id = "destacados_carousel")
    })
    
    
    
    
    
    shiny::tagList(
      fluidRow(
        column(12,
               br(),
               h4("Resumen de la comparación"),
               br(),
               reactableOutput("tabla_escenarios_guardados"), align="center") 
        
        
      ),
      br(),
      br(),
      h4("Gráficos de resumen"),
      br(),
      fluidRow(
        lapply(1:3, function (i) {
          column(4,
                 
                 div(
                   class = "comp-box",
                   style = "padding: 5px; !important",
                   highchartOutput(glue("grafico_multiple{i}")))
          )
        })
      ),
      br(),
      h4("Destacados"),
      br(),
      fluidRow(
        column(3),
        column(6,
               htmlOutput("carouselInfoBoxes")),
        column(3)
      )
    )
  }
}
  
  
    
      
      
      