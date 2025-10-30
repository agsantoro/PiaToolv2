gapsChart = function (input,output,session,datosFiltrados,labels_provincia,labels_region,labels_sexo, cocienteTasas, diferenciaTasas, firstTimeGaps) {
  if (get_page() != "gaps" | is.null(input$causas)) {return()}
  
  if (input$geo == "Jurisdicciones") {
    geoVar = "provincia"
    area = input$area
  } else {
    geoVar = "region"
    area = input$area
  }
  # setea máximo eje y
  if (nrow(datosFiltrados())>0) {
    if (input$indicador %in% indicatorsList()[c(1,4,5,6)]) {
      valores = (datosFiltrados() %>% dplyr::filter(get(geoVar) %in% input$area & sexo %in% c(1,2)))[[input$indicador]]
      maxY = max(valores)
      min = 0
    } else {
      
      
      maxY = max(datosFiltrados()[[input$indicador]][datosFiltrados()[[geoVar]] %in% area])
      min = 0.95 * min(datosFiltrados()[[input$indicador]][datosFiltrados()[[geoVar]] %in% area])
      if (min < 0) {min = 0}
    }
    
    maxY = ceiling((maxY))
  }
  
  lapply(area, function (i) {
      datosGrafico = datosFiltrados()
      datosGrafico = datosGrafico[datosGrafico[geoVar] == i,]
      
      # probar agregar como serie en eje
      serieCocienteTasas = cocienteTasas(datosFiltrados = datosGrafico)
      serieDiferenciaTasas = diferenciaTasas(datosFiltrados = datosGrafico)
      
      # crea gráfico de área
      output[[glue("graficoArea_{i}")]] = renderHighchart({
        if (input$geo == "Jurisdicciones") {
          tituloArea = names(labels_provincia)[labels_provincia == i]
        } else {
          tituloArea = names(labels_region)[labels_region == i]
        } 
        
        tituloCausa = input$causas
        
        if (nrow(datosGrafico)==0) {
        } else {
          
          for (i in 1:nrow(datosGrafico)) {
            datosGrafico$sexo[i] = names(labels_sexo[labels_sexo==datosGrafico$sexo[i]])
          }
          
          #Gráfico doble eje y
          
          indicador = names(indicatorsList()[indicatorsList() == input$indicador])
          datosGrafico$values = datosGrafico[[input$indicador]]
          
          if (input$indicador %in% indicatorsList()[c(1,4,5,6)]) {
            hchart(type = "line", datosGrafico[datosGrafico$sexo %in% c("Varones","Mujeres"),], hcaes(x = ano, y = values, group = sexo)) %>%
              hc_tooltip(valueDecimals = 2) %>%
              hc_title(text = glue("{tituloArea} - {indicador} por {tituloCausa}"),
                       style = list(
                         fontSize = 14
                       )) %>%
              hc_credits(enabled = TRUE,
                         text = "Fuente: elaboración propia a partir de datos de la DEIS-MSAL.") %>%
              hc_exporting(enabled = F) %>% 
              hc_yAxis(
                max = maxY,
                title = list(text = indicador)
              )
          } else if (input$indicador == "tmape") {
            hchart(type = "line", datosGrafico, hcaes(x = ano, y = values, group = sexo)) %>%
              hc_add_series(data = serieCocienteTasas, yAxis = 1, visible = F, name = "Cociente de tasas") %>%
              hc_tooltip(valueDecimals = 2) %>%
              hc_yAxis_multiples(
                list(title = list(text = indicador),
                     opposite = FALSE,
                     min = min,
                     max = maxY),
                list(title = list(
                  text = "Cociente"),
                  opposite = TRUE,
                  min = 0)) %>%
              hc_xAxis(title = list(text = "Año")) %>%
              hc_title(text = glue("{tituloArea} - {indicador} por {tituloCausa}"),
                       style = list(
                         fontSize = 14
                       )) %>%
              hc_credits(enabled = TRUE,
                         text = "Fuente: elaboración propia a partir de datos de la DEIS-MSAL.") %>%
              hc_exporting(enabled = F)
            
          } else {
            hchart(type = "line", datosGrafico, hcaes(x = ano, y = values, group = sexo)) %>%
              hc_tooltip(valueDecimals = 2) %>%
              hc_xAxis(title = list(text = "Año")) %>%
              hc_title(text = glue("{tituloArea} - {indicador} por {tituloCausa}"),
                       style = list(
                         fontSize = 14
                       )) %>%
              hc_credits(enabled = TRUE,
                         text = "Fuente: elaboración propia a partir de datos de la DEIS-MSAL.") %>%
              hc_exporting(enabled = F)
            
          }
          
          
          
          
          
          
          
          # hchart(datosGrafico, "line", hcaes(x = ano, y = tmape, group = sexo)) %>%
          #   hc_yAxis_multiples(
          #     list(lineWidth = 3),
          #     list(showLastLabel = FALSE, opposite = TRUE)
          #   ) %>%
          #   hc_add_series(data = serieCocienteTasas, type = "line", yAxis = 1, visible = F)
          
        }
      })
      tagList({
        column(12,
               div(
                 highchartOutput(glue("graficoArea_{i}"))))
      })
      
    
    })
}
    


  