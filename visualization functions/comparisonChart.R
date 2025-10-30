comparisonChart = function (input,output,datosFiltrados,indicatorsList,labels_provincia,labels_region,labels_sexo, diferenciaTasas) {
  
  if (nrow(datosFiltrados())==0) {return()}
  
  pal = paletteer_d("ggsci::default_igv")
  
  if (input$geo == "Jurisdicciones") {
    geoVar = "provincia"
    area = input$area
    labelsArea = labels_provincia
  } else {
    geoVar = "region"
    area = input$area
    labelsArea = labels_region
  }
  
  datosGrafico = datosFiltrados()
  datosGrafico$geoVar = datosGrafico[[geoVar]]
  datosGrafico$values = datosGrafico[[input$indicador]]
  indicador = names(indicatorsList()[indicatorsList() == input$indicador])
  
  charts = list()
  
  for (sx in c(0,1,2)) {
    serie1 = datosGrafico[datosGrafico$sexo == sx & datosGrafico$geoVar == labelsArea[1],] %>% 
      dplyr::select(ano, values) %>% setNames(c("ano", "values"))
    
    chart = highchart() %>%
      hc_xAxis(
        categories = serie1$ano,
        title = list(text = "Año")
      ) %>% 
      hc_yAxis(
        title = list(text = names(indicatorsList()[indicatorsList() == input$indicador]))
      ) %>%
      hc_add_series(name = names(labelsArea[1]), data = serie1$values, color = pal[1]) %>%
      hc_legend(
        layout = "vertical",
        align = "right",
        verticalAlign = "top") %>% 
      hc_title(
          text = names(indicatorsList()[indicatorsList() == input$indicador]),
          align = "left"
          
          
        )
      
    
    for (i in labelsArea[!names(labelsArea) %in% c("Total país", "Sin especificar", "Otro país")]) {
      dataChart = datosGrafico[datosGrafico$sexo == sx & datosGrafico$geoVar == i,]
      dataChart$geoVar =names(labelsArea[labelsArea==i])
      
      index = (as.numeric(which(labelsArea[!names(labelsArea) %in% c("Total país", "Sin especificar", "Otro país")] == i))) + 1
      
      chart = chart %>% 
        hc_add_series(name = names(labelsArea[labelsArea == i]), data = dataChart$values, color = pal[index], visible = FALSE)
    }
    
    charts[[paste0("chart","_",sx)]] = chart
  }
  
  tagList(
    br(),
    br(),
    fluidRow(
      column(
        12,
        h4("Ambos sexos", style = "margin:0 !important"),
        br(),
        charts$chart_0
      )
    ),
    fluidRow(
      column(12,
             hr(style = "border-top: 1px solid #bdbdbd"))
    ),
    fluidRow(
      column(
        12,
        h4("Varones", style = "margin:0 !important"),
        br(),
        charts$chart_1
      )
    ),
    fluidRow(
      column(12,
             hr(style = "border-top: 1px solid #bdbdbd"))
    ),
    fluidRow(
      column(
        12,
        h4("Mujeres", style = "margin:0 !important"),
        br(),
        charts$chart_2
      )
    )
    
  )
  
  
  
  
  
}



