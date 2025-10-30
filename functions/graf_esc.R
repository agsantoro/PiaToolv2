graf_esc = function(table, output) {
  charts <- lapply(1:nrow(table), function(i) {
    output[[paste0("graf",i)]]  = renderHighchart({
      
      # Gráfico que se muestra para cada indicador
      
      
      hchart(object = table[i,], type = "bar", hcaes(x = indicador, y = valor)) %>%
        hc_colors(colors = c("#339BDF")) %>%
        
        hc_title(text = table[i,1], style = list(fontSize = '14px', fontFamily = 'sans-serif', fontWeight = 'bold')) %>%
        hc_xAxis(title = list(text = ""), labels = list(enabled = FALSE),
                 startOnTick = FALSE, endOnTick = FALSE) %>%
        hc_yAxis(title = list(text = "Valor"), labels = list(enabled = TRUE),
                 plotLines = list(list(color = "black", width = 1, value = 0)),
                 gridLineWidth = 0,  # Elimina las líneas de la cuadrícula del eje Y
                 minorGridLineWidth = 0,  # Elimina las líneas menores de la cuadrícula
                 plotBands = list(list(from = -0.1, to = 0.1, color = 'rgba(0,0,0,0)'))  # Oculta específicamente la línea en el valor 0
        ) %>%
        hc_plotOptions(series = list(pointWidth = 20, borderColor = 'black',
                                     borderWidth = 1,
                                     groupPadding = -1,  # Reduce el padding entre grupos de barras
                                     pointPadding = -4
        )) %>%
        hc_tooltip(pointFormat = 'Valor: {point.y:.1f}') %>%
        hc_size(height = 220) %>%
        hc_add_theme(hc_theme_hcrt())  # Aplica el tema 'hcrt'
      
      
      
      # titulo <- paste(table[i, "indicador"])
      # table$valor_formateado <- sapply(table$valor, function(x) {
      #   format(x, big.mark = ",", decimal.mark = ".", nsmall = 1)
      # })
      # 
      # 
      # fig <- plot_ly(data = table[i,],
      #                x = ~valor, y = ~indicador, type = 'bar', orientation = 'h',
      #                marker = list(color = '#339BDF',  # Establece el color de la barra
      #                              line = list(color = 'black', width = 1)),  # Establece el color y ancho del borde
      #                hoverinfo = 'text', text = ~paste(valor_formateado)) %>%
      #   layout(title = list(text = titulo, x = 0,  
      #                       font = list(size = 16, family="Isotok Web" )),  
      #          xaxis = list(title = ""),  
      #          yaxis = list(title = "", showticklabels = FALSE),  
      #          hovermode = 'closest',
      #          autosize = F,
      #          width = 400, height = 250,
      #          margin= list(
      #            l = 40,
      #            r = 40,
      #            b = 40,
      #            t = 40,
      #            pad = 4
      #          )) %>%  
      #   config(displayModeBar = FALSE)  
      # # Mostrar el gráfico
      # fig
      
      
      
    })
  })
  
  tagList(
    lapply(seq_along(charts), function(i) {
      column(4,highchartOutput(paste0("graf",i), height = "250px"))
    })
  )
  
}

