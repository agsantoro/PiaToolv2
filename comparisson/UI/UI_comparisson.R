ui_comparisson = function (input, saved_scenarios, current_page, getCountryCode) {
  
  if (length(saved_scenarios)>1 & length(current_page)>0) {
    
    scenarios = saved_scenarios[sapply(saved_scenarios, function(i) i$model == current_page)]
    renderUI({
      
      nombres_originales = names(scenarios)
      
      opciones_con_bandera <- setNames(
        nombres_originales,  # <- Estos son los VALUES que devolverá el input
        lapply(nombres_originales, function(x) {  # <- Este es el HTML que se MUESTRA
          countryCode = getCountryCode(scenarios[[x]]$country)
          bandera_url <- glue("https://flagcdn.com/w20/{countryCode}.png")
          as.character(tags$span(
            tags$img(src = bandera_url, width = "20px", style = "margin-right: 10px;"), 
            x
          ))
        })
      )
      
      tagList(
        checkboxGroupButtons(
          inputId = "compScenariosNames",
          label = "Escenarios a comparar",
          choices = opciones_con_bandera,
          selected = nombres_originales
        )
      )
      
      
    })
  } else {
    return()
  }
  
  
  
  
    
}


ui_resultados_comparisson = function(input,output,session,resultados, current_page, saved_scenarios) {
  if (get_page()!="comparisson") {return()}
  #if (get_page=="comparisson" | input$compScenariosNames == "") {return()}
  resultados = resultados[sapply(resultados, function(i) i$model == current_page)]
  
  resultados = resultados[input$compScenariosNames]
  all_outputs = purrr::map_dfr(resultados, "outputs", .id = "scenarioName")
  
  if (nrow(all_outputs)==0) {return()}
  colnames(all_outputs) = c("scenarioName","cat", "Indicador", "Valor")
  all_outputs = all_outputs %>% pivot_wider(names_from = "scenarioName", values_from = "Valor") 
  
  output$table_comp_output = renderReactable({
    reactable(
      all_outputs,
      groupBy = "cat",
      defaultExpanded = T,
      pagination = F,
      defaultColDef = colDef(
        align = "right",
        headerStyle = list(background = "#236292", color = "white")
      ),
      columns = list(
        cat = colDef(name = "Categoría", align = "left"),
        Indicador = colDef(name = "Indicador", align = "left")
      ),
      bordered = TRUE,
      highlight = TRUE
    )
  })
  
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
    } else if (saved_scenarios[[i]]$model == "naat") {
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
    dplyr::filter(scenarioName %in% input$compScenariosNames)
  
  compaBox = compa %>%
    dplyr::filter(!indicador %in% c("RCEI_AVAD","ROI"))
  
  compa$value = str_replace_all(compa$value, "\\.", "")
  compa$value = str_replace_all(compa$value, ",", "\\.")
  
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
  
  
  unique_indicators <- unique(compa$indicador)
  
  # Crear gráficos con los nuevos estilos
  list_of_plots <- lapply(seq_along(unique(compaBox$indicador)), function(idx) {
    indicador <- unique_indicators[idx]
    data_subset <- dplyr::filter(compaBox, indicador == !!indicador)
    data_subset$value = str_replace_all(data_subset$value,"\\.","") 
    data_subset$value = as.numeric(str_replace_all(data_subset$value,",","\\."))
    
    chart <- hchart(data_subset, "bar", hcaes(x = Intervencion_escenario, y = value, name = indicador)) %>%
      hc_chart(
        backgroundColor = "#FFFFFF",  # Fondo blanco
        style = list(fontFamily = "Frutiger")  # Fuente Frutiger
      ) %>%
      hc_title(
        text = paste("Indicador:", indicador),
        style = list(fontSize = "14px", fontFamily = "Frutiger")
      ) %>%
      hc_plotOptions(series = list(
        color = '#1c98d6',  # Color de las columnas
        dataLabels = list(enabled = FALSE)  # Eliminar valores dentro de las columnas
      )) %>%
      hc_xAxis(
        title = list(text = "Escenario seleccionado", style = list(fontFamily = "Frutiger")),
        categories = data_subset$Intervencion_escenario,
        labels = list(style = list(fontFamily = "Frutiger"))
      ) %>%
      hc_yAxis(
        title = list(text = "", style = list(fontFamily = "Frutiger")),
        opposite = TRUE,
        labels = list(style = list(fontFamily = "Frutiger")),
        plotLines = list(list(
          value = 0,
          color = '#CCCCCC',
          width = 1
        ))
      ) %>%
      hc_tooltip(
        style = list(fontFamily = "Frutiger"),
        headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
        pointFormat = '<b>{series.name}:</b> {point.y}<br/>'  # Muestra el nombre del indicador en lugar de "Serie 1"
      ) %>%
      hc_legend(
        enabled = FALSE  # Ocultar leyenda ya que solo hay una serie por gráfico
      )
    
    chart
  })
  
  output$grafico_rapido1 = renderHighchart({list_of_plots[[1]]})
  output$grafico_rapido2 = renderHighchart({list_of_plots[[2]]})
  output$grafico_rapido3 = renderHighchart({list_of_plots[[3]]})
  
  tagList(
    fluidRow(
      column(12,
             reactableOutput("table_comp_output"))
    ),
    hr(),
    fluidRow(
      column(
        4,
        highchartOutput("grafico_rapido1")
      ),
      column(
        4,
        highchartOutput("grafico_rapido2")
      ),
      column(
        4,
        highchartOutput("grafico_rapido3")
      )
    )
  )
  
}


