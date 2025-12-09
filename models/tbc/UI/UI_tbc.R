ui_tbc = function (input, tbc_map_inputs) {
  renderUI({
    if (is.null(input$country) == F) {
      addData = data.frame(
        intervencion = "VDOT Tuberculosis",
        i_names = names(get_tbc_params(input=input)),
        i_labels = get_tbc_params_labels()
      )
      
      bsc = 1:3
      avz = 4:nrow(addData)
    
      addData$avanzado = NA
      addData$avanzado[bsc] = F
      addData$avanzado[avz] = T

      rownames(addData) = 1:nrow(addData)
      
      tbc_map_inputs(addData)


    }
    
    porcentajes = c(2,6,7,8,14,20,31)
    
    inputs_hover = get_tbc_hover()
    
    tagList(
      
      
      lapply(bsc, function(i) {
        if (i %in% porcentajes) {
          sliderInput(tbc_map_inputs()$i_names[i],
                      tags$div(
                        tbc_map_inputs()$i_labels[i],
                        icon("circle-info",
                             "fa-1x",
                             title = inputs_hover[i])
                      ),
                      
                      min=0, 
                      max=100,
                      step = 0.01,
                      value = get_tbc_params(input)[[i]]*100)
        } else {
          numericInput(tbc_map_inputs()$i_names[i],tags$div(
            tbc_map_inputs()$i_labels[i],
            icon("circle-info",
                 "fa-1x",
                 title = inputs_hover[i])
          ),get_tbc_params(input)[[i]])
        }
        
      }),
      hr(),
      
      tags$div(
        tags$button(
          class = "btn btn-default btn-block",
          `data-toggle` = "collapse",
          `data-target` = "#collapseInputs_tbc",
          style = "text-align: center; margin-bottom: 10px; position: relative; border: 2px solid #DEE2E6; font-size: 13px; background-color: white; border-radius: 6px;",
          "Parámetros avanzados",
          tags$span(
            icon("caret-down"),
            style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%);"
          )
        ),
        
        tags$div(
          id = "collapseInputs_tbc",
          class = "collapse",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;",

      lapply(avz, function(i) {
        if (i %in% porcentajes) {
          sliderInput(tbc_map_inputs()$i_names[i],tags$div(
            tbc_map_inputs()$i_labels[i],
            icon("circle-info",
                 "fa-1x",
                 title = inputs_hover[i])
          ),min=0, 
          max=100,
          step = 0.01,
          value = get_tbc_params(input)[[i]]*100)
        } else {
          numericInput(tbc_map_inputs()$i_names[i],tags$div(
            tbc_map_inputs()$i_labels[i],
            icon("circle-info",
                 "fa-1x",
                 title = inputs_hover[i])
          ),get_tbc_params(input)[[i]])
        }
        
      })),
      hr(),
      tags$div(
        
        column(12,
               actionButton(
                 "tbc_go",
                 icon("play")),
               align = "right")
        
      )
    ))
    
    
    
    
  })
  
  
  
  
}


ui_resultados_tbc = function(input,output,resultados) {
  
  tbc_run = resultados()[,c(1,4)]
  
  output$tbc_grafico = renderUI({
    if (length(tbc_run)>1) {

      indicadores = c(
        'Años de vida ajustados por discapacidad evitados',
        'Costo total de la intervención (USD)',
        'Diferencia de costos respecto al escenario basal (USD)',
        'Retorno de Inversión (%)',
        'Razon de costo-efectividad incremental por año de vida salvado'
      )
      
      table = tbc_run[tbc_run$Parametro %in% indicadores,]
      colnames(table) = c("indicador","valor")
      
      
      graf_esc(table, output)
      
    }
  })
  
  output$tbc_summaryTable = renderReactable({
    
    if (length(tbc_run)>1) {
      table = tbc_run
      table$vDOT = format(round(table$vDOT,1),big.mark = ".",decimal.mark = ",")
      
      cat_epi = 1:4
      cat_costos = 5:nrow(table)
      
      table$cat=""
      table$cat[cat_epi] = "Resultados epidemiológicos"
      table$cat[cat_costos] = "Resultados económicos"
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
          vDOT = colDef(name = "Valor", align = "right")
        ),
        bordered = TRUE,
        highlight = TRUE
      )
    }
    
  })
  
  tagList(
    
    fluidRow(class="shadow-xl ring-1 ring-gray-900/5 my-6 py-8",
      column(12,
             reactableOutput("tbc_summaryTable")
             )
    )
    
    
  )
  
  
}


