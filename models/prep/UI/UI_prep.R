ui_prep = function (input, prep_map_inputs) {
  renderUI({
    if (is.null(input$country) == F) {
      addData = data.frame(
        intervencion = "Profilaxis Pre Exposición VIH",
        i_names = names(get_prep_params(input$country)),
        i_labels = get_prep_params_labels()
      )

      bsc = 1:4
      avz = 5:nrow(addData)
      
      addData$avanzado = NA
      addData$avanzado[avz] = T
      addData$avanzado[bsc] = F

      rownames(addData) = 1:nrow(addData)

      prep_map_inputs(addData)
    }

    tagList(
      
      lapply(bsc, function(i) {
        numericInput(prep_map_inputs()$i_names[i],
                     prep_map_inputs()$i_labels[i],
                     get_prep_params(input$country)[[i]])
      }),
      
      
      tags$div(
        tags$button(
          class = "btn btn-default btn-block",
          `data-toggle` = "collapse",
          `data-target` = "#collapseInputs_prep",
          style = "text-align: center; margin-bottom: 10px; position: relative; border: 2px solid #DEE2E6; font-size: 13px; background-color: white; border-radius: 6px;",
          "Parámetros avanzados",
          tags$span(
            icon("caret-down"),
            style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%);"
          )
        ),
        
        tags$div(
          id = "collapseInputs_prep",
          class = "collapse",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;",

      lapply(avz, function(i) {
        numericInput(prep_map_inputs()$i_names[i],
                     prep_map_inputs()$i_labels[i],
                     get_prep_params(input$country)[[i]])
      })
    )),
    
    tags$div(
      column(12,
             actionButton(
               "prep_go",
               icon("play")),
             align = "right")
      
      
      
      
      
    ),
    tags$script(HTML("
    $('#collapseInputs_prep').on('show.bs.collapse', function () {
      $('[data-target=\"#collapseInputs_prep\"] span i').removeClass('fa-caret-down').addClass('fa-caret-up');
    });
    $('#collapseInputs').on('hide.bs.collapse', function () {
      $('[data-target=\"#collapseInputs_prep\"] span i').removeClass('fa-caret-up').addClass('fa-caret-down');
    });
  "))
    )
    
    
    
    
  })
}


ui_grafico_nuevo_prep = function(input,output,resultados) {
  table = resultados()
  table$Parametro = prep_outcomes_labels()
  
  if (length(table)>1) {
    table$Parametro = prep_outcomes_labels()
    colnames(table) = c("indicador","valor")
    
    indicadores = c(
      'Años de vida ajustados por discapacidad evitados',
      'Costo total de la intervención (USD)',
      'Diferencia de costos respecto al escenario basal (USD)',
      'Retorno de Inversión (ROI) (%)',
      'Razón de costo-efectividad incremental (RCEI) por Años de Vida Ajustados por Discapacidad (AVAD) Evitados'
    )
    table = table[table$indicador %in% indicadores,]
    renderUI({
      graf_esc(table,output)
    })
    
    
    
    
  }
  
  
}


ui_resultados_prep = function(input,output,resultados, prep_map_outputs) {
  
  prep_run = resultados()
  
  output$prep_summaryTable = renderReactable({
    
    if (length(prep_run)>1) {
      table = prep_run
      table$Parametro = prep_outcomes_labels()
      
      table$Valor = format(round(table$Valor,1),big.mark = ".",decimal.mark = ",")
      
      cat_epi = 1:6
      cat_costos = 7:nrow(table)
      
      table$cat=""
      table$cat[cat_epi] = "Resultados epidemiológicos"
      table$cat[cat_costos] = "Resultados económicos"
      
      # ocultamos descontados
      table = table[c(1,2,3,5,7,9,11,13,15,17,19),]
      
      prep_map_outputs(table %>% dplyr::select(cat, Parametro, Valor))
      
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
          Parametro = colDef(name = "Indicador", align = "left"),
          Valor = colDef(name = "Valor", align = "right")
        ),
        bordered = TRUE,
        highlight = TRUE
      )
    }
    
  })
  
  tagList(
    reactableOutput("prep_summaryTable")
  )
}





