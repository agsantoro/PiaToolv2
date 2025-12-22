UI_sifilis = function (input, sifilis_map_inputs) {
  renderUI({
    if (is.null(input$country) == F) {
      inputs = sifilisInputList() %>% dplyr::filter(sifilisInputList()$tipo != "No Incluido")
      
      addData = data.frame(
        intervencion = "Sifilis",
        i_names = inputs$var,
        i_labels = inputs$label
      )
      
      bsc = which(inputs$tipo == "Basico")
      avz = which(inputs$tipo == "Avanzado")
      
      addData$avanzado = NA
      addData$avanzado[bsc] = F
      addData$avanzado[avz] = T
      
      rownames(addData) = 1:nrow(addData)
      
      sifilis_map_inputs(addData)
      
      
    }
    
    porcentajes = which(inputs$porc == T)
    
    inputs_hover = inputs$label
    
    values = cargar(input$country)
    
    tagList(
      lapply(bsc, function(i) {
        if (i %in% porcentajes) {
          
          sliderInput(sifilis_map_inputs()$i_names[i],
                      tags$div(
                        sifilis_map_inputs()$i_labels[i],
                        icon("circle-info",
                             "fa-1x",
                             title = inputs_hover[i])
                      ),
                      
                      min=0, 
                      max=1,
                      step = 0.01,
                      value = values[[sifilis_map_inputs()$i_names[i]]])
        } else {
          numericInput(sifilis_map_inputs()$i_names[i],
                       tags$div(
                         sifilis_map_inputs()$i_labels[i],
            icon("circle-info",
                 "fa-1x",
                 title = inputs_hover[i])
          ),values[[sifilis_map_inputs()$i_names[i]]])
        }
        
      }),
      hr(),
      
      tags$div(
        tags$button(
          class = "btn btn-default btn-block",
          `data-toggle` = "collapse",
          `data-target` = "#collapseInputs_sifilis",
          style = "text-align: center; margin-bottom: 10px; position: relative; border: 2px solid #DEE2E6; font-size: 13px; background-color: white; border-radius: 6px;",
          "Parámetros avanzados",
          tags$span(
            icon("caret-down"),
            style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%);"
          )
        ),
        
        tags$div(
          id = "collapseInputs_sifilis",
          class = "collapse",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;",
          
          lapply(avz, function(i) {
            if (i %in% porcentajes) {
              sliderInput(sifilis_map_inputs()$i_names[i],
                          tags$div(
                            sifilis_map_inputs()$i_labels[i],
                            icon("circle-info",
                                 "fa-1x",
                                 title = inputs_hover[i])
                          ),
                          
                          min=0, 
                          max=1,
                          step = 0.01,
                          value = values[[sifilis_map_inputs()$i_names[i]]])
            } else {
              numericInput(sifilis_map_inputs()$i_names[i],
                           tags$div(
                             sifilis_map_inputs()$i_labels[i],
                             icon("circle-info",
                                  "fa-1x",
                                  title = inputs_hover[i])
                           ),values[[sifilis_map_inputs()$i_names[i]]])
            }
            
          })),
        hr(),
        tags$div(
          
          column(12,
                 actionButton(
                   "sifilis_go",
                   icon("play")),
                 align = "right")
          
        ),
        
        tags$script(HTML("
    $('#collapseInputs_sifilis').on('show.bs.collapse', function () {
      $('[data-target=\"#collapseInputs_sifilis\"] span i').removeClass('fa-caret-down').addClass('fa-caret-up');
    });
    $('#collapseInputs').on('hide.bs.collapse', function () {
      $('[data-target=\"#collapseInputs_sifilis\"] span i').removeClass('fa-caret-up').addClass('fa-caret-down');
    });
  "))
      ))
    
    
    
    
  })
  
  
  
  
}


ui_resultados_sifilis = function(input,output,resultados, sifilis_map_outputs) {
  
  output$sifilis_summaryTable = renderReactable({
    if (length(resultados)>1) {
      table = resultados$tablaMain %>% as.data.frame()
      
      cat_epi = 1:8
      cat_costos = 8:nrow(table)

      table$cat=""
      table$cat[cat_epi] = "Resultados epidemiológicos"
      table$cat[cat_costos] = "Resultados económicos"
      
      sifilis_map_outputs(table %>% dplyr::select(cat, Indicador, Valor))
      
      reactable(
        table,
        groupBy = "cat",
        defaultExpanded = T,
        pagination = F,
        defaultColDef = colDef(
          
          minWidth = 70,
          headerStyle = list(background = "#236292", color = "white")
        ),
        columns = list(
          cat = colDef(name = "Categoría", align = "left"),
          Parametro = colDef(name = "Indicador", align = "left"),
          Valor = colDef(name = "Valor", align = "right"),
          `Valor Descontado` = colDef(name = "Valor Descontado", align = "right")
        ),
        bordered = TRUE,
        highlight = TRUE
      )
    }
    
  })
  
  tagList(
    
    fluidRow(class="shadow-xl ring-1 ring-gray-900/5 my-6 py-8",
             column(12,
                    reactableOutput("sifilis_summaryTable")
             )
    )
    
    
  )
  
  
}


