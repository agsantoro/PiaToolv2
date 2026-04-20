UI_naat = function (input, naat_map_inputs) {
  renderUI({
    if (is.null(input$country) == F) {
      inputs_naat = isolate(naatInputList())
      inputs_naat = inputs_naat[is.na(inputs_naat$tipo) == F,]
      inputs_naat = inputs_naat[inputs_naat$tipo != "No incluido",]
      
      addData = data.frame(
        intervencion = "naat",
        i_names = inputs_naat$var,
        i_labels = inputs_naat$label
      )
      
      bsc = which(inputs_naat$tipo == "Basico")
      avz = which(inputs_naat$tipo == "Avanzado")
      
      addData$avanzado = NA
      addData$avanzado[bsc] = F
      addData$avanzado[avz] = T
      
      rownames(addData) = 1:nrow(addData)
      addData$i_names = paste0(addData$i_names,"_naat")
      
      valuesInputs = cargar_naat()[[input$country]]
      valuesInputs[valuesInputs>1] = lapply(valuesInputs[valuesInputs>1], round, digits = 2)
      
      valuesInputs = valuesInputs[paste0(names(valuesInputs),"_naat") %in% addData$i_names]
      valuesInputs = data.frame(
        i_names = paste0(names(valuesInputs),"_naat"),
        Valor = unname(unlist(valuesInputs))
      )
      
      naat_map_inputs(
        addData %>% left_join(valuesInputs) %>% dplyr::select(Input = i_labels, Valor)
      )
      
      
    }
    
    porcentajes = which(inputs_naat$porc == T)
    
    inputs_hover = inputs_naat$label
    
    values = cargar_naat()[[input$country]]
    values[values>1] = lapply(values[values>1], round, digits = 2)
    
    names(values) = paste0(names(values),"_naat")
    
    tagList(
      lapply(bsc, function(j) {
        if (j %in% porcentajes) {
          sliderInput(addData$i_names[j],
                      tags$div(
                        addData$i_labels[j],
                        icon("circle-info",
                             "fa-1x",
                             title = inputs_hover[j])
                      ),
                      
                      min=0, 
                      max=1,
                      step = 0.01,
                      value = values[[addData$i_names[j]]])
        } else {
          numericInput(addData$i_names[j],
                       tags$div(
                         addData$i_labels[j],
                         icon("circle-info",
                              "fa-1x",
                              title = inputs_hover[j])
                       ),values[[addData$i_names[j]]])
        }
        
      }),
      hr(),
      
      tags$div(
        tags$button(
          class = "btn btn-default btn-block",
          `data-toggle` = "collapse",
          `data-target` = "#collapseInputs_naat",
          style = "text-align: center; margin-bottom: 10px; position: relative; border: 2px solid #DEE2E6; font-size: 13px; background-color: white; border-radius: 6px;",
          "Parámetros avanzados",
          tags$span(
            icon("caret-down"),
            style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%);"
          )
        ),
        
        tags$div(
          id = "collapseInputs_naat",
          class = "collapse",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;",
          
          lapply(avz, function(i) {
            if (i %in% porcentajes) {
              sliderInput(addData$i_names[i],
                          tags$div(
                            addData$i_labels[i],
                            icon("circle-info",
                                 "fa-1x",
                                 title = inputs_hover[i])
                          ),
                          
                          min=0, 
                          max=1,
                          step = 0.01,
                          value = values[[addData$i_names[i]]])
            } else {
              numericInput(addData$i_names[i],
                           tags$div(
                             addData$i_labels[i],
                             icon("circle-info",
                                  "fa-1x",
                                  title = inputs_hover[i])
                           ),values[[addData$i_names[i]]])
            }
            
          })),
        hr(),
        tags$div(
          
          column(12,
                 div(
                   id = "go-btn-container-naat",
                   actionButton(
                     "naat_go",
                     label = "Correr escenario",
                     class = "go-button",
                     icon("play"))),
                 align = "right")
          
        ),
        
        tags$script(HTML("
    $('#collapseInputs_naat').on('show.bs.collapse', function () {
      $('[data-target=\"#collapseInputs_naat\"] span i').removeClass('fa-caret-down').addClass('fa-caret-up');
    });
    $('#collapseInputs').on('hide.bs.collapse', function () {
      $('[data-target=\"#collapseInputs_naat\"] span i').removeClass('fa-caret-up').addClass('fa-caret-down');
    });
  "))
      ))
    
    
    
    
  })
  
  
  
  
}


ui_resultados_naat = function(input,output,resultados,naat_map_outputs) {
  output$naat_summaryTable = renderReactable({
    if (length(resultados)>1) {
      
      table = resultados$tablaMain %>% as.data.frame()
      
      cat_epi = 1:6
      cat_costos = 7:nrow(table)
      table$cat=""
      table$cat[cat_epi] = "Resultados epidemiológicos"
      table$cat[cat_costos] = "Resultados económicos"
      
      
      naat_map_outputs(table %>% dplyr::select(cat, Indicador, Valor))
      
      
      reactable(
        table[,c(1,2,4)],
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
          Valor = colDef(name = "Valor", align = "right")
        ),
        bordered = TRUE,
        highlight = TRUE
      )
    }
    
  })
  
  tagList(
    
    fluidRow(class="shadow-xl ring-1 ring-gray-900/5 my-6 py-8",
             column(12,
                    reactableOutput("naat_summaryTable")
             )
    )
    
    
  )
  
  
}


