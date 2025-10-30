ui_hearts = function (input,base_line, targets_default, costs, population) {
  
  country_sel = str_to_title(input$country)
  renderUI({
    
    input_labels = c(
      'Porcentaje de personas diagnosticadas que se encuentran en tratamiento (objetivo)',
      'Población total del país (n)',
      'Prevalencia de adultos con hipertensión, estandarizada por edad (%)',
      'Porcentaje de adultos con hipertensión diagnosticados',
      'Porcentaje de personas diagnosticadas que se encuentran en tratamiento (basal)',
      'Porcentaje de adultos con hipertensión controla entre los tratados',
      # 'Letalidad ponderada por edad y sexo para accidente cerebrovascular (grupo entre 30-79 años) (%)',
      # 'Letalidad ponderada por edad y sexo para enfermedad cardíaca isquémica (grupo entre 30-79 años) (%)',
      'Costo farmacológico anual promedio por paciente (USD)',
      'Costo de seguimiento anual promedio por paciente (USD)',
      'Costo de evento de enfermedad coronaria isquémica (USD)',
      'Costo de evento de accidente cerebro vascular (USD)',
      'Tasa de descuento (%)',
      'Costo programático anual (USD)'
    )
    
    inputs_hover = c(
      'Porcentaje de personas que están recibiendo tratamiento en relación con el total de individuos que han sido diagnosticados con hipertensión en un punto inicial o línea de base',
      'Refleja el objetivo fijado para el porcentaje de personas diagnosticadas con hipertensión que deben tener tratamiento',
      'Prevalencia de individuos con hipertensión, diagnosticada o no, en el grupo etario de 30 a 79 años en cada país, proporcionando un punto de referencia inicial',
      'Porcentaje de individuos entre 30 y 79 años previamente diagnosticados con hipertensión, proporcionando un punto de referencia inicial',
      'Porcentaje de personas de 30 a 79 años, ya diagnosticadas con hipertensión y en tratamiento, que han conseguido controlar su presión arterial, proporcionando un punto de referencia inicial',
      'Población total (proyectada para el año 2023) de cada país',
      # 'Índice de letalidad (tasa de letalidad ponderada por sexo y edad) para accidente cerebrovascular en el grupo de edad de 30 a 79 años',
      # 'Índice de letalidad (tasa de letalidad ponderada por sexo y edad) para enfermedad coronaria isquémica en el grupo de edad de 30 a 79 años',
      'Incluye la proporción de pacientes que están en cada paso por país para abril de 2023 (USD oficial a tasa de cambio nominal de cada país)',
      'Costos de tratar un evento de enfermedad coronaria isquémica por país para abril de 2023 (USD oficial a tasa de cambio nominal de cada país)',
      'Costos de tratar un evento de accidente cerebrovascular (USD oficial a tasa de cambio nominal de cada país)',
      'Costos de seguimiento (farmacológico, consultas médicas, etc) anual por paciente (USD oficial a tasa de cambio nominal de cada país)',
      'Se utiliza para traer al presente los costos y beneficios en salud futuros',
      'Costo de implementar y sostener la intervención en un año (USD oficial a tasa de cambio nominal de cada país)'
    )
    
    input_values = c(
      targets_default$treatment[targets_default$country==str_to_title(input$country)],
      population$population[population$country==str_to_title(input$country)],
      base_line$prevalence_of_hypertension[base_line$country==str_to_title(input$country)],
      base_line$prevalence_previous_diagnosis[base_line$country==str_to_title(input$country)],
      base_line$treatment[base_line$country==str_to_title(input$country)],
      base_line$control[base_line$country==str_to_title(input$country)],
      base_line$costo_farm[base_line$country==str_to_title(input$country)],
      base_line$costo_seguimiento[base_line$country==str_to_title(input$country)],
      base_line$costo_evento_eci[base_line$country==str_to_title(input$country)],
      costs$value[costs$parameter=="Evento de enfermedad cardiaca isquemica promedio  (***)" & costs$country==str_to_title(input$country)],
      0.05,
      0
    )
    
    if (is.null(input$country) == F) {
      i_names = c()
      for (i in 1:length(input_labels)) {
        i_names = c(i_names,paste0("hearts_input_",i))
      }
      
      i_labels = c()
      
      for (i in 1:length(input_labels)) {
        i_labels = c(i_labels,input_labels[i])
      }
      
      hearts_map_inputs = data.frame(
        intervencion = "HEARTS",
        i_names,
        i_labels
      )
      
      hearts_map_inputs$avanzado = NA
      hearts_map_inputs$avanzado[c(1)] = F
      hearts_map_inputs$avanzado[is.na(hearts_map_inputs$avanzado)] = T
      rownames(hearts_map_inputs) = 1:nrow(hearts_map_inputs)
      
      bsc = which(hearts_map_inputs$avanzado==F)
      avz = which(hearts_map_inputs$avanzado==T)
      prc = c(3,4,5,6,11)
      
      
      save(
        hearts_map_inputs,
        file = "hearts_map_inputs.Rdata"
      )
      
    }
    
    tagList(
      
      lapply(bsc, function (i) {
        sliderInput(paste0("hearts_input_",i),
                    tags$div(input_labels[i],icon("circle-info","fa-1x",title = inputs_hover[i])),
                    value = 100*targets_default$treatment[targets_default$country==country_sel],
                    min=0,
                    max=100,
                    step=.1)
      })
      ,
      
      tags$header(style="background-color: black; color: white; text-align: center", 
                  tags$h1(style="display: inline-block; margin: 0 auto;", class="flex-grow mt-8 mb-8",tags$b("Avanzado")),
                  actionLink(inputId = "toggle_avanzado_hearts", label=icon("stream", style = "color: white;"))
      ),
      br(),
      hidden(
        
        lapply(avz, function (i) {
          if (!i %in% prc) {
            numericInput(paste0("hearts_input_",i),
                         tags$div(input_labels[i],icon("circle-info","fa-1x",title = inputs_hover[i], verify_fa = FALSE)),
                         value = input_values[i],
                         step=.1)
          } else {
            sliderInput(paste0("hearts_input_",i),
                        tags$div(input_labels[i],icon("circle-info","fa-1x",title = inputs_hover[i], verify_fa = FALSE)),
                        value = input_values[i]*100,
                        min=0,
                        max=100,
                        step=.1)
          }
          
        })
        
        
      ),
      
      
      
      # ,
      # hidden(
      #   lapply(c(3), function (i) {
      #     sliderInput(paste0("hearts_input_",i),
      #                 tags$div(input_names[i],icon("circle-info","fa-1x",title = model_card_hearts$Descripción[model_card_hearts$inputID==paste0("hearts_input_",i)])),
      #                 value = 100*base_line[base_line$country==country_sel,names(input_names[i])],
      #                 min=0,
      #                 max=100,
      #                 step=.1)
      #   }),
      #   lapply(c(1,2,4), function (i) {
      #     sliderInput(paste0("hearts_input_",i),
      #                 tags$div(input_names[i],icon("circle-info","fa-1x",title = model_card_hearts$Descripción[model_card_hearts$inputID==paste0("hearts_input_",i)])),
      #                 value = 100*base_line[base_line$country==country_sel,names(input_names[i])],
      #                 min=0,
      #                 max=100,
      #                 step=.1)
      #   }),
      #   lapply(c(5,6,7), function (i) {
      #     numericInput(paste0("hearts_input_",i),
      #                  tags$div(input_names[i],icon("circle-info","fa-1x",title = model_card_hearts$Descripción[model_card_hearts$inputID==paste0("hearts_input_",i)])),
      #                 value = base_line[base_line$country==country_sel,names(input_names[i])],
      #                 step=.1)
      #   })
      # )
    )
    
    
    
    
  })
}

ui_resultados_hearts = function(input,output,resultados) {
  paste(input$hearts_input_1)
  country_sel = str_to_title(input$country)
  
  if (is.null(input$hearts_input_1)==F) {
    run_hearts = resultados()
    
    output$hearts_grafico_1 = renderPlotly({
      if (length(run_hearts)>0) {
        x = 0:100
        y = c()
        
        for (i in x) {
          y=c(y,exp((-0.0294 * i) +5.5305))
        }
        
        y1 = round(run_hearts$epi_model_outcomes$Modelo1_x1,1)
        y2 = round(run_hearts$epi_model_outcomes$Modelo1_x2,1)
        x1 = round(run_hearts$run[[country_sel]]$baseline$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`*100,1)
        x2 = round(run_hearts$run[[country_sel]]$target$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`*100,1)
        
        data <- data.frame(x,y,y1,y2,x1,x2)
        fig <-
          plot_ly(
            data)%>%
          add_trace(
            y = ~ y1,
            name = 'trace 1',
            mode = "line",
            type = 'scatter',
            line = list(
              color = '#FF691D',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'none'
          ) %>%
          add_trace(
            y = ~ y2,
            type = 'scatter',
            mode = "line",
            line = list(
              color = '#0a94d6',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'skip'
          ) %>%
          add_trace(
            x = ~ x1,
            y=~ y,
            type = 'scatter',
            mode = "line",
            line = list(
              color = '#FF691D',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'skip'
          ) %>%
          add_trace(
            x = ~ x2,
            y=~ y,
            type = 'scatter',
            mode = "line",
            line = list(
              color = '#0a94d6',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'skip'
          )  %>% 
          add_trace(
            
            x = ~ x,
            y = ~ y,
            name = 'trace 0',
            type = 'scatter',
            mode = "line",
            line = list(color = 'black', width = 2),
            hovertemplate = paste("Para una prevalencia estandarizada por edad del control de la HTA en la población de %{x:,}%<br>",
                                  " se espera una mortalidad por enfermedad isquémica del corazón de %{y:.1f} por 100 000 habitantes<extra></extra> ")
          ) %>%
          add_annotations(
            x = 3,
            y = data$y1[1] + 3,
            text = data$y1[1],
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            showarrow = F
          ) %>% add_annotations(
            x = 3,
            y = data$y2[1] + 3,
            text = data$y2[1],
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            showarrow = F
          ) %>% add_annotations(
            y = 15,
            x = data$x1[1] + 0.5,
            text = data$x1[1],
            textangle=90,
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            showarrow = F
          ) %>% add_annotations(
            y = 15,
            x = data$x2[1] + 0.5,
            text = data$x2[1],
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            textangle=90,
            showarrow = F
          ) %>% config(displayModeBar = FALSE) %>% layout(
            showlegend = FALSE,
            title = list(text='<b>Modelo predictivo para la mortalidad por enfermedad isquémica del corazón<b>',
                         font=
                           list(color = '#265787',
                                family = "Istok Web",
                                size=15)),
            xaxis = list(title =list(text="Prevalencia de control de HTA en la población (%)",
                                     font=list(
                                       family = "Istok Web",
                                       size = 14,
                                       color = '#265787')) ,
                         zeroline = FALSE,           
                         zerolinecolor = "gray",
                         showline= F),
            yaxis = list(title =list(text="Tasa por 100.000 habitantes", font=list(
              family = "Istok Web",
              size = 14,
              color = '#265787')) ,
              tickfont = list(color = "gray"),
              linecolor = "gray",
              showline= F),
            zeroline = FALSE,           
            zerolinecolor = "gray"
          )
        fig %>% layout(font = list(family ="Istok Web"))
        
        
      }
      
      
    })
    
    output$hearts_grafico = renderUI({
      
      if(length(run_hearts)>0) {
        table = run_hearts$resumen_resultados
        indicadores = c(
          'Años de vida ajustados por discapacidad evitados',
          'Costos totales de la intervención (USD)',
          'Diferencia de costos respecto al escenario basal (USD)',
          'Retorno de inversión (%)',
          'Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)'
        )
        table = table[table$indicador %in% indicadores,]
        
        graf_esc(table,output)
        
      }
    })
    
    output$hearts_grafico_2 = renderPlotly({
      if (length(run_hearts)>0) {
        x = 0:100
        y = c()
        
        for (i in x) {
          y=c(y,exp((-0.0240177 * i) + 4.57206))
        }
        
        y1 = round(run_hearts$epi_model_outcomes$Modelo2_x1,1)
        y2 = round(run_hearts$epi_model_outcomes$Modelo2_x2,1)
        
        x1 = round(run_hearts$run[[country_sel]]$baseline$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`*100,1)
        x2 = round(run_hearts$run[[country_sel]]$target$`Prevalence of controlled hypertension among adults aged 30-79 years with hypertension, age- standardized`*100,1)
        data <- data.frame(x,y,y1,y2,x1,x2)
        
        fig <-
          plot_ly(
            data)%>%
          add_trace(
            y = ~ y1,
            name = 'trace 1',
            mode = "line",
            type = 'scatter',
            line = list(
              color = '#FF691D',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'none'
          ) %>%
          add_trace(
            y = ~ y2,
            type = 'scatter',
            mode = "line",
            line = list(
              color = '#0a94d6',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'skip'
          ) %>%
          add_trace(
            x = ~ x1,
            y=~ y,
            type = 'scatter',
            mode = "line",
            line = list(
              color = '#FF691D',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'skip'
          ) %>%
          add_trace(
            x = ~ x2,
            y=~ y,
            type = 'scatter',
            mode = "line",
            line = list(
              color = '#0a94d6',
              width = 1,
              dash = 'dash'
            ),
            hoverinfo = 'skip'
          )  %>% 
          add_trace(
            
            x = ~ x,
            y = ~ y,
            name = 'trace 0',
            type = 'scatter',
            mode = "line",
            line = list(color = 'black', width = 2),
            hovertemplate = paste("Para una prevalencia estandarizada por edad del control de la HTA en la población de %{x:,}%<br>",
                                  " se espera una mortalidad por accidente cerebrovascular de %{y:.1f} por 100 000 habitantes<extra></extra>")
          ) %>%
          add_annotations(
            x = 3,
            y = data$y1[1]+ 1 ,
            text = data$y1[1],
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            showarrow = F
          ) %>% add_annotations(
            x = 3,
            y = data$y2[1]+ 1,
            text = data$y2[1],
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            showarrow = F
          ) %>% add_annotations(
            y = 15,
            x = data$x1[1] + 0.5,
            text = data$x1[1],
            textangle=90,
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            showarrow = F
          ) %>% add_annotations(
            y = 15,
            x = data$x2[1] + 0.5,
            text = data$x2[1],
            font =list(
              color = '#265787',
              family = "Istok Web",
              size = 10
            ),
            textangle=90,
            showarrow = F
          )%>% config(displayModeBar = FALSE) %>%
          layout(
            showlegend = FALSE,
            title = list(text='<b>Modelo predictivo para la mortalidad por accidente cerebrovascular',
                         font=
                           list(color = '#265787',
                                family = "Istok Web",
                                size = 15)
            ),
            xaxis = list(title = list(text="Prevalencia de control de HTA en la población (%)",font=list(
              family = "Istok Web",
              size = 14,
              color = '#265787')),
              zeroline = FALSE,           
              zerolinecolor = "gray",
              showline= F),
            yaxis = list(title = list( text="Tasa por 100.000 habitantes",font=list(
              family = "Istok Web",
              size = 14,
              color = '#265787')),
              tickfont = list(color = "gray"),
              linecolor = "gray",
              showline= F),
            zeroline = FALSE,           
            zerolinecolor = "gray"
          )
        fig
        
      }
      
      
    })
    
    output$hearts_resultados = renderReactable({
      if(length(run_hearts)>0) {
        table = run_hearts$resumen_resultados
        colnames(table) = c("Indicador","Valor")  
        
        cat_epi = 1:13
        cat_costos = 14:20
        table$cat=""
        table$cat[cat_epi] = "Resultados epidemiológicos"
        table$cat[cat_costos] = "Resultados económicos"
        rownames(table) = NULL
        table$Valor = format(round(table$Valor,1), big.mark=".", small.mark=",")
        
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
            Indicador = colDef(name = "Indicador", align = "left"),
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
                      uiOutput("hearts_grafico"))
      ),
      # fluidRow(
      #   column(6,
      #          plotlyOutput("hearts_grafico_1")),
      #   column(6,
      #          plotlyOutput("hearts_grafico_2"))
      # ),
      # br(),
      # br(),
      fluidRow(class="shadow-xl ring-1 ring-gray-900/5 my-6 py-8",
               column(12,
                      reactableOutput("hearts_resultados"))
               
      )
      
      
      
    )
    
    
  } else {NULL}
  
  
}
