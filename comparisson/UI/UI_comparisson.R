ui_comparisson = function (input, saved_scenarios, current_page, getCountryCode) {
  
  if (length(saved_scenarios)>1 & length(current_page)>0) {
    scenarios = saved_scenarios[sapply(saved_scenarios, function(i) i$model == current_page)]
    renderUI({
      
      nombres_originales = names(saved_scenarios)
      
      opciones_con_bandera <- setNames(
        nombres_originales, 
        lapply(nombres_originales, function(x) {
          # Aquí puedes definir qué bandera corresponde a cada nombre
          
          countryCode = getCountryCode(scenarios[[x]]$country)
          bandera_url <- glue("https://flagcdn.com/w20/{countryCode}.png") # Ejemplo para Argentina
          
          HTML(paste(
            tags$img(src = bandera_url, width = "20px", style = "margin-right: 10px;"),
            x
          ))
        })
      )
      
      tagList(
        checkboxGroupButtons(
          inputId = "compScenariosNames",
          label = "Escenarios a comparar",
          choices = opciones_con_bandera, # La lista con HTML
          selected = nombres_originales
        )
        
      )
      
      
    })
  } else {
    return()
  }
  
  
  
  
    
}


ui_resultados_comparisson = function(input,output,session,resultados, current_page) {
  if (get_page()!="comparisson") {return()}
  #if (get_page=="comparisson" | input$compScenariosNames == "") {return()}
  
  resultados = resultados[sapply(resultados, function(i) i$model == current_page)]
  
  resultados = resultados[input$compScenariosNames]
  
  all_outputs = purrr::map_dfr(resultados, "outputs", .id = "scenarioName")
  
  if (nrow(all_outputs)==0) {return()}
  colnames(all_outputs) = c("scenarioName","cat", "Indicador", "Valor")
  all_outputs = all_outputs %>% pivot_wider(names_from = "scenarioName", values_from = "Valor") 
  
  
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
  
  
}


