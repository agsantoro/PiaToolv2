ui_inputs_multiComp = function (input, saved_scenarios, current_page, getCountryCode) {
  if (get_page() != "multiComp") { return() }
  
  if (length(saved_scenarios) > 1 & length(current_page) > 0) {
    
    # 1. Extraer modelos únicos
    models = unique(unlist(lapply(saved_scenarios, function(i) i[["model"]])))
    nModels = length(models)
    
    # 2. Mapear nombres a códigos de país
    choices_by_model = split(names(saved_scenarios), 
                             unlist(lapply(saved_scenarios, function(i) i$model)))
    
    # 3. Crear la lista de opciones con HTML procesado
    choices_with_flags <- lapply(names(choices_by_model), function(model_key) {
      scenario_ids <- choices_by_model[[model_key]]
      
      # Crear vector: HTML_LABEL = ID_VALUE
      opciones <- sapply(scenario_ids, function(id) {
        country_raw = saved_scenarios[[id]]$country
        countryCode <- tolower(getCountryCode(country_raw))
        
        if (!is.null(countryCode) && countryCode != "" && nchar(countryCode) > 0) {
          bandera_url <- sprintf("https://flagcdn.com/w20/%s.png", countryCode)
          
          # HTML como STRING (sin HTML())
          label_html <- sprintf(
            '<img src="%s" width="20" style="margin-right:8px;vertical-align:middle;"><span>%s</span>',
            bandera_url, 
            htmltools::htmlEscape(id)
          )
          return(label_html)
        } else {
          return(id)
        }
      }, USE.NAMES = FALSE)
      
      # setNames( VALORES , ETIQUETAS_HTML )
      res <- setNames(scenario_ids, opciones)
      return(res)
    })
    
    # 4. Traducir nombres de los grupos (Modelos)
    model_titles <- sapply(names(choices_by_model), function(x) {
      switch(x,
             "hearts" = "Iniciativa HEARTS",
             "hpv"    = "Vacunación contra el VPH",
             "tbc"    = "VDOT para Tuberculosis",
             "hepC"   = "Tratamiento para la hepatitis C crónica",
             "hpp"    = "Uso de oxitocina para la prevención de la hemorragia post parto",
             "prep"   = "Profilaxis pre exposición (PrEP) para VIH",
             "sifilis"= "Tests rápidos en punto de cuidado para sífilis gestacional",
             "naat"   = "Pruebas de amplificación de ácidos nucleicos (NAAT) para TBC",
             x)
    }, USE.NAMES = FALSE)
    
    names(choices_with_flags) <- model_titles
    
    # 5. Renderizar el input
    dragDropInput(
      inputId = "selectScenariosMulti",
      label = "Seleccione los escenarios a comparar",
      choices = choices_with_flags,
      n_cuadros = nModels
    )
    
  } else {
    return()
  }
}


ui_resultados_multiComp = function(input,output,session,current_page, saved_scenarios, selectedScenarios) {
  
  if (get_page()!="multiComp") {return()} else {
    #browser()
    if (is.null(input$selectScenariosMulti)) {return()}
    
    output$table_multi = renderReactable({
      # AVAD = list()
      # COSTO_TOTAL = list()
      # DIF_COSTO = list()
      # RCEI_AVAD = list()
      # ROI = list()
      # 
      SUMMARY = lapply(seq_along(saved_scenarios), function (i) {
        if (saved_scenarios[[i]]$model == "hearts") {
          data = saved_scenarios[[i]]$outputs
          list(
            "Name" = names(saved_scenarios[i]),
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
            "Model" = saved_scenarios[[i]]$model,
            "AVAD" = data$Undiscounted[data$Outcomes == "Años de Vida Ajustados por Discapacidad evitados (AVAD)"],
            "COSTO_TOTAL" = "",
            "DIF_COSTO" = "",
            "RCEI_AVAD" = "",
            "ROI" = "")
        } else if (saved_scenarios[[i]]$model == "tbc") {
          data = saved_scenarios[[i]]$outputs
          list(
            "Name" = names(saved_scenarios[i]),
            "Model" = saved_scenarios[[i]]$model,
            "AVAD" = data$vDOT[data$Parametro == "Años de vida ajustados por discapacidad evitados"],
            "COSTO_TOTAL" = "",
            "DIF_COSTO" = "",
            "RCEI_AVAD" = "",
            "ROI" = "")
        }
      })
      
      summaryTable = bind_rows(SUMMARY)
      
      summaryTable = summaryTable[summaryTable$Name %in% input$selectScenariosMulti,]
      return(reactable(summaryTable))
      
    })
    
    
  }
  
  
  
  #if (get_page=="comparisson" | input$compScenariosNames == "") {return()}
  
  
  # output$escenarios_guardados = renderUI({
  #   shinyjs::hide("header_comparacion_resultados")
  #   shinyjs::hide("header_tabla_inputs")
  #   shinyjs::hide("inputs_summary_table")
  #   if (length(input$comparacion_intervencion)>1) {
  #     intervenciones_seleccionadas = input$comparacion_intervencion
  #     escenarios_seleccionados = input$comparacion_escenario
  #     
  #     AVAD = list()
  #     COSTO_TOTAL = list()
  #     DIF_COSTO = list()
  #     RCEI_AVAD = list()
  #     ROI = list()
  #     
  #     for (j in escenarios_seleccionados[escenarios_seleccionados %in% summary_scenarios$table$scenarioName[summary_scenarios$table$intervencion=="Vacuna contra el HPV"]]) {
  #       AVAD[[j]] = scenarios$savedScenarios[[j]]$outcomes$undisc[scenarios$savedScenarios[[j]]$outcomes$outcomes=="Años de Vida Ajustados por Discapacidad evitados (AVAD)"]
  #       COSTO_TOTAL[[j]] = scenarios$savedScenarios[[j]]$outcomes$undisc[scenarios$savedScenarios[[j]]$outcomes$outcomes=="Costo total de la intervención (USD)"]
  #       DIF_COSTO[[j]] = scenarios$savedScenarios[[j]]$outcomes$undisc[scenarios$savedScenarios[[j]]$outcomes$outcomes=="Diferencia de Costos respecto al escenario basal (USD)"]
  #       ROI[[j]] = scenarios$savedScenarios[[j]]$outcomes$undisc[scenarios$savedScenarios[[j]]$outcomes$outcomes=="Retorno de Inversión (%)"]
  #       RCEI_AVAD[[j]] = scenarios$savedScenarios[[j]]$outcomes$undisc[scenarios$savedScenarios[[j]]$outcomes$outcomes=="Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitados (USD)"]
  #     }
  #     
  #     for (j in escenarios_seleccionados[escenarios_seleccionados %in% summary_scenarios$table$scenarioName[summary_scenarios$table$intervencion=="HEARTS"]]) {
  #       AVAD[[j]] = as.numeric(hearts_scenarios$savedScenarios[[j]]$Valor[hearts_scenarios$savedScenarios[[j]]$Indicador=="Años de vida ajustados por discapacidad evitados"])
  #       COSTO_TOTAL[[j]] = as.numeric(hearts_scenarios$savedScenarios[[j]]$Valor[hearts_scenarios$savedScenarios[[j]]$Indicador=="Costos totales de la intervención (USD)"])
  #       DIF_COSTO[[j]] = as.numeric(hearts_scenarios$savedScenarios[[j]]$Valor[hearts_scenarios$savedScenarios[[j]]$Indicador=="Diferencia de costos respecto al escenario basal (USD)"])
  #       ROI[[j]] = as.numeric(hearts_scenarios$savedScenarios[[j]]$Valor[hearts_scenarios$savedScenarios[[j]]$Indicador=="Retorno de inversión (%)"])
  #       RCEI_AVAD[[j]] = as.numeric(hearts_scenarios$savedScenarios[[j]]$Valor[hearts_scenarios$savedScenarios[[j]]$Indicador=="Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)"])
  #     }
  #     
  #     for (j in escenarios_seleccionados[escenarios_seleccionados %in% summary_scenarios$table$scenarioName[summary_scenarios$table$intervencion=="Hemorragia postparto"]]) {
  #       AVAD[[j]] = hpp_scenarios$savedScenarios[[j]]$valor[hpp_scenarios$savedScenarios[[j]]$indicador=="Años de vida ajustados por discapacidad evitados"]
  #       COSTO_TOTAL[[j]] = hpp_scenarios$savedScenarios[[j]]$valor[hpp_scenarios$savedScenarios[[j]]$indicador=="Costo total de la intervención (USD)"]
  #       DIF_COSTO[[j]] = hpp_scenarios$savedScenarios[[j]]$valor[hpp_scenarios$savedScenarios[[j]]$indicador=="Diferencia de costos respecto al escenario basal (USD)"]
  #       ROI[[j]] = hpp_scenarios$savedScenarios[[j]]$valor[hpp_scenarios$savedScenarios[[j]]$indicador=="Retorno de Inversión (%)"]
  #       RCEI_AVAD[[j]] = hpp_scenarios$savedScenarios[[j]]$valor[hpp_scenarios$savedScenarios[[j]]$indicador=="Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)"]
  #     }
  #     
  #     for (j in escenarios_seleccionados[escenarios_seleccionados %in% summary_scenarios$table$scenarioName[summary_scenarios$table$intervencion=="Hepatitis C"]]) {
  #       AVAD[[j]] = hepC_scenarios$savedScenarios[[j]]$Valor[hepC_scenarios$savedScenarios[[j]]$Indicador=="Años de Vida Ajustados por Discapacidad evitados"]
  #       COSTO_TOTAL[[j]] = hepC_scenarios$savedScenarios[[j]]$Valor[hepC_scenarios$savedScenarios[[j]]$Indicador=="Costo total de la intervención (USD)"]
  #       DIF_COSTO[[j]] = hepC_scenarios$savedScenarios[[j]]$Valor[hepC_scenarios$savedScenarios[[j]]$Indicador=="Diferencia de costos respecto al escenario basal (USD)"]
  #       ROI[[j]] = hepC_scenarios$savedScenarios[[j]]$Valor[hepC_scenarios$savedScenarios[[j]]$Indicador=="Retorno de Inversión (%)"]
  #       RCEI_AVAD[[j]] = hepC_scenarios$savedScenarios[[j]]$Valor[hepC_scenarios$savedScenarios[[j]]$Indicador=="Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitada (USD)"]
  #     }
  #     
  #     for (j in escenarios_seleccionados[escenarios_seleccionados %in% summary_scenarios$table$scenarioName[summary_scenarios$table$intervencion=="VDOT Tuberculosis"]]) {
  #       AVAD[[j]] = round(tbc_scenarios$savedScenarios[[j]]$vDOT[tbc_scenarios$savedScenarios[[j]]$Parametro=="Años de vida ajustados por discapacidad evitados"],1)
  #       COSTO_TOTAL[[j]] = round(tbc_scenarios$savedScenarios[[j]]$vDOT[tbc_scenarios$savedScenarios[[j]]$Parametro=="Costo total de la intervención (USD)"],1)
  #       DIF_COSTO[[j]] = round(tbc_scenarios$savedScenarios[[j]]$vDOT[tbc_scenarios$savedScenarios[[j]]$Parametro=="Diferencia de costos respecto al escenario basal (USD)"],1)
  #       ROI[[j]] = round(tbc_scenarios$savedScenarios[[j]]$vDOT[tbc_scenarios$savedScenarios[[j]]$Parametro=="Retorno de Inversión (%)"],1)
  #       RCEI_AVAD[[j]] = round(tbc_scenarios$savedScenarios[[j]]$vDOT[tbc_scenarios$savedScenarios[[j]]$Parametro=="Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido"],1)
  #     }
  #     
  #     for (j in escenarios_seleccionados[escenarios_seleccionados %in% summary_scenarios$table$scenarioName[summary_scenarios$table$intervencion=="Profilaxis Pre Exposición VIH"]]) {
  #       AVAD[[j]] = round(prep_scenarios$savedScenarios[[j]]$Valor[prep_scenarios$savedScenarios[[j]]$Parametro=="Años de vida ajustados por discapacidad evitados"],1)
  #       COSTO_TOTAL[[j]] = round(prep_scenarios$savedScenarios[[j]]$Valor[prep_scenarios$savedScenarios[[j]]$Parametro=="Costo total de la intervención (USD)"],1)
  #       DIF_COSTO[[j]] = round(prep_scenarios$savedScenarios[[j]]$Valor[prep_scenarios$savedScenarios[[j]]$Parametro=="Diferencia de costos respecto al escenario basal (USD)"],1)
  #       ROI[[j]] = round(prep_scenarios$savedScenarios[[j]]$Valor[prep_scenarios$savedScenarios[[j]]$Parametro=="Retorno de Inversión (ROI) (%)"],1)
  #       RCEI_AVAD[[j]] = round(prep_scenarios$savedScenarios[[j]]$Valor[prep_scenarios$savedScenarios[[j]]$Parametro=="Razón de costo-efectividad incremental (RCEI) por Años de Vida Ajustados por Discapacidad (AVAD) Evitados (descontado)"],1)
  #     }
  #     
  #     escenarios = names(unlist(ROI))
  #     
  #     indicadores = c(
  #       "AVAD",
  #       "COSTO_TOTAL",
  #       "DIF_COSTO",
  #       "ROI",
  #       "RCEI_AVAD")
  #     
  #     table = data.frame()
  #     
  #     for (i in indicadores) {
  #       append = data.frame(
  #         indicador = i,
  #         scenarioName = escenarios,
  #         value = unlist(eval(parse(text=i)))) %>% left_join(summary_scenarios$table)
  #       table = rbind(table,append)
  #     }
  #     
  #     table = table %>% dplyr::select(scenarioName,country,intervencion, indicador, value)
  #     
  #     
  #     
  #     ########## ACÁ METÉ LOS GRÁFICOS ##########
  #     
  #     
  #     compa <- table
  #     
  #     compa<- compa %>%
  #       mutate(Intervencion_escenario = paste0(intervencion,'<br>',"(",scenarioName,")" ),
  #              value = round(value, 1))
  #     
  #     compa$indicador[compa$indicador=="AVAD"] = "Años de vida ajustados por discapacidad evitados"
  #     compa$indicador[compa$indicador=="COSTO_TOTAL"] = "Costo total de la intervención (USD)"
  #     compa$indicador[compa$indicador=="DIF_COSTO"] = "Diferencia de costos respecto al escenario basal (USD)"
  #     compa$indicador[compa$indicador=="ROI"] = "Retorno de Inversión (%)"
  #     compa$indicador[compa$indicador=="RCEI_AVAD"] = "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido"
  #     
  #     
  #     
  #     # Colores de fondo para cada gráfico
  #     background_colors <- c("#FEDCB4", "#FCE3CB", "#c6efef", "#b2ceea", "#A8B7CC")
  #     
  #     unique_indicators <- unique(compa$indicador)
  #     
  #     #  gráficos
  #     list_of_plots <- lapply(seq_along( unique(compa$indicador)), function(idx) {
  #       indicador <- unique_indicators[idx]
  #       data_subset <-filter(compa, indicador == !!indicador)
  #       chart <- hchart(data_subset, "bar", hcaes(x = Intervencion_escenario, y = value, name = intervencion)) %>%
  #         hc_chart(backgroundColor = background_colors[idx %% length(background_colors) + 1]) %>% # Establecer color de fondo
  #         hc_title(text = paste("Indicador:", indicador),
  #                  style = list(fontSize = "14px")) %>%
  #         hc_plotOptions(series = list(
  #           color = '#596775' # Configurar el color de las barras a negro
  #           # dataLabels = list(
  #           #   enabled = TRUE, 
  #           #   format = '{point.y}',  # Usar el nombre de la opción de punto para la etiqueta
  #           #   color = 'black', # Cambiar el color del texto a negro
  #           #   align = 'right', # Alinear a la derecha (fuera de la barra)
  #           #   inside = FALSE, # Asegurar que la etiqueta esté fuera de la barra
  #           #   verticalAlign = 'middle', # Alinear verticalmente en el medio
  #           #   y = 0, # Ajustar posición vertical
  #           #   x = 5  # Ajustar posición horizontal (un poco a la derecha de la barra)
  #           # )
  #         )) %>%
  #         hc_xAxis(title = list(text = "Escenario selecionado"),
  #                  categories=data_subset$Intervencion_escenario) %>%
  #         hc_yAxis(title = list(text = ""),
  #                  opposite = TRUE,
  #                  plotLines = list(list(
  #                    value = 0, 
  #                    color = 'white',
  #                    width = 2 # Puedes ajustar el grosor de la línea aquí
  #                  )) )%>%
  #         hc_tooltip(pointFormat = paste('Valor de',indicador,': <b>{point.y:,.0f}</b><br/>'))
  #       chart
  #     })
  #     
  #     # cuadrícula
  #     
  #     
  #     # hw_grid(list_of_plots, rowheight = 240, ncol=5, add_htmlgrid_css = F) %>%
  #     #   htmltools::browsable()
  #     # 
  #     
  #     
  #     output$grafico_multiple1 = renderHighchart({list_of_plots[[1]]})
  #     output$grafico_multiple2 = renderHighchart({list_of_plots[[2]]})
  #     output$grafico_multiple3 = renderHighchart({list_of_plots[[3]]})
  #     output$grafico_multiple4 = renderHighchart({list_of_plots[[4]]})
  #     output$grafico_multiple5 = renderHighchart({list_of_plots[[5]]})
  #     
  #     output$tabla_escenarios_guardados = renderReactable({
  #       
  #       table_data = data.frame(
  #         scenarioName = paste0(table$scenarioName," (",table$country," / ",table$intervencion),
  #         table$indicador,
  #         table$value
  #       ) %>% dplyr::mutate(
  #         table.indicador = case_when(table$indicador == "AVAD" ~ "Años de vida ajustados por discapacidad evitados",
  #                                     table$indicador == "COSTO_TOTAL" ~ "Costos totales de la intervención (USD)",
  #                                     table$indicador == "DIF_COSTO"  ~ "Diferencia de costos respecto al escenario basal (USD)",
  #                                     table$indicador == "ROI"  ~ "Retorno de inversión (%)",
  #                                     table$indicador == "RCEI_AVAD" ~  "Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)"),
  #         table.value = format(round(table.value,1), big.mark=".", decimal.mark=",")
  #       )
  #       
  #       reactable(
  #         table_data,
  #         defaultExpanded = T,
  #         groupBy = "scenarioName",
  #         pagination = F,
  #         columns = list(
  #           scenarioName = colDef(name = "Escenario guardado", align = "left"),
  #           table.indicador = colDef(name = "Indicador", align = "left"),
  #           table.value = colDef(name = "Valor", align = "right")
  #         ),
  #         defaultColDef = colDef(
  #           headerStyle = list(background = "#236292", color = "white", borderWidth = "0")
  #         )
  #         
  #       )
  #       
  #       
  #       
  #     })
  #     
  #     output$infoBoxAVAD = renderUI({
  #       best = max(table$value[table$indicador=="AVAD"])
  #       nombre_scn = table$scenarioName[table$indicador == "AVAD" & table$value == best]
  #       hito = "Mayor cantidad de AVAD salvados"
  #       valor = format(round(best,1),big.mark=".",small.mark=",")
  #       intervencion = table$intervencion[table$indicador == "AVAD" & table$value == best]
  #       
  #       info_box(
  #         nombre_scn = nombre_scn,
  #         hito = hito,
  #         valor = valor,
  #         intervencion = intervencion)
  #       
  #     })
  #     
  #     output$infoBoxCostoTotal = renderUI({
  #       best = min(table$value[table$indicador=="COSTO_TOTAL"])
  #       nombre_scn = table$scenarioName[table$indicador == "COSTO_TOTAL" & table$value == best]
  #       hito = "Menor costo total de la intervención (%)"
  #       valor = format(round(best,1),big.mark=".",small.mark=",")
  #       intervencion = table$intervencion[table$indicador == "COSTO_TOTAL" & table$value == best]
  #       
  #       info_box(
  #         nombre_scn = nombre_scn,
  #         hito = hito,
  #         valor = valor,
  #         intervencion = intervencion)
  #       
  #     })
  #     
  #     output$infoBoxDiferenciaCosto = renderUI({
  #       best = min(table$value[table$indicador=="DIF_COSTO"])
  #       nombre_scn = table$scenarioName[table$indicador == "DIF_COSTO" & table$value == best]
  #       hito = "Menor diferencia de costo respecto del escenario basal (%)"
  #       valor = format(round(best,1),big.mark=".",small.mark=",")
  #       intervencion = table$intervencion[table$indicador == "DIF_COSTO" & table$value == best]
  #       
  #       info_box(
  #         nombre_scn = nombre_scn,
  #         hito = hito,
  #         valor = valor,
  #         intervencion = intervencion)
  #       
  #     })
  #     
  #     output$infoBoxROI = renderUI({
  #       best = max(table$value[table$indicador=="ROI"])
  #       nombre_scn = table$scenarioName[table$indicador == "ROI" & table$value == best]
  #       hito = "Mayor retorno de inversión (%)"
  #       valor = format(round(best,1),big.mark=".",small.mark=",")
  #       intervencion = table$intervencion[table$indicador == "ROI" & table$value == best]
  #       
  #       info_box(
  #         nombre_scn = nombre_scn,
  #         hito = hito,
  #         valor = valor,
  #         intervencion = intervencion)
  #       
  #     })
  #     
  #     output$infoBoxRCEIAVAD = renderUI({
  #       best = min(table$value[table$indicador=="RCEI_AVAD"])
  #       nombre_scn = table$scenarioName[table$indicador == "RCEI_AVAD" & table$value == best]
  #       hito = "Menor razón de costo incremental por AVAD evitado (%)"
  #       valor = format(round(best,1),big.mark=".",small.mark=",")
  #       intervencion = table$intervencion[table$indicador == "RCEI_AVAD" & table$value == best]
  #       
  #       info_box(
  #         nombre_scn = nombre_scn,
  #         hito = hito,
  #         valor = valor,
  #         intervencion = intervencion)
  #       
  #     })
  #     
  #   }
  #   
  #   
  #   output$prueba = renderReactable({
  #     if (is.null(sel_escenario)==F & is.null(input$comparacion_intervencion)==F & is.null(input$comparacion_escenario)==F) {
  #       tabla = inputs_table_generator_multiple(input,output, inputs_scenarios, summary_scenarios)
  #       #tabla = inputs_table_multiple
  #       intervenciones = names(tabla)
  #       tabla = lapply(intervenciones, function (i) {
  #         tabla[[i]] =
  #           lapply(tabla[[i]], function (j) {
  #             j = pivot_longer(
  #               j,
  #               names_to = "escenario",
  #               values_to = "valor",
  #               cols = 2
  #             )
  #           })
  #       })
  #       
  #       names(tabla) = intervenciones
  #       
  #       tabla = lapply(intervenciones, function (i) {
  #         tabla[[i]] = cbind(int=i,bind_rows(tabla[[i]]))  
  #       })
  #       tabla = bind_rows(tabla)
  #       
  #       tabla$Categoría = NULL
  #       
  #       tabla$escenario_full = paste0(tabla$int,": ",tabla$escenario)
  #       
  #       tabla = tabla[,c("Input","valor","escenario_full")]
  #       
  #       reactable(
  #         tabla,
  #         groupBy = "escenario_full",
  #         pagination = F,
  #         columns = list(
  #           escenario_full = colDef(name = "Escenario guardado", align = "left"),
  #           Input = colDef(name = "Input", align = "left"),
  #           valor = colDef(name = "Valor", align = "right")
  #         ),
  #         defaultColDef = colDef(
  #           headerStyle = list(background = "#236292", color = "white", borderWidth = "0")
  #         )
  #         
  #       ) 
  #     }
  #     
  #     
  #     
  #   })
  #   
  #   shiny::tagList(
  #     fluidRow(
  #       column(9,
  #              tags$header(class="text-1xl flex justify-between items-center p-5 mt-4",style="background-color: #FF671B; color: white; text-align: center", 
  #                          tags$h1(style="display: inline-block; margin: 0 auto;", class="flex-grow mt-8 mb-8",tags$b("Generales")),
  #              ),
  #              br(),
  #              reactableOutput("tabla_escenarios_guardados"), align="center"),
  #       column(3,
  #              tags$header(class="text-1xl flex justify-between items-center p-5 mt-4", style="background-color: #FF671B; color: white; text-align: center", 
  #                          tags$h1(style="display: inline-block; margin: 0 auto;", class="flex-grow mt-8 mb-8",tags$b("Destacados")),
  #              ),
  #              br(),
  #              htmlOutput("infoBoxAVAD"),
  #              htmlOutput("infoBoxCostoTotal"),
  #              htmlOutput("infoBoxDiferenciaCosto"),
  #              htmlOutput("infoBoxROI"),
  #              htmlOutput("infoBoxRCEIAVAD"), align = "center")
  #       
  #     ),
  #     
  #     fluidRow(
  #       
  #       column(
  #         12,
  #         br(),
  #         tags$header(class="text-1xl flex justify-between items-center p-5 mt-4", style="background-color: #FF671B; color: white; text-align: center", 
  #                     tags$h1(style="display: inline-block; margin: 0 auto;", class="flex-grow mt-8 mb-8",tags$b("Gráficos")),
  #                     
  #         ),
  #         br()
  #       ), align="center"),
  #     fluidRow(
  #       column(4,highchartOutput("grafico_multiple1")),
  #       column(4,highchartOutput("grafico_multiple2")),
  #       column(4,highchartOutput("grafico_multiple3"))
  #     ),
  #     fluidRow(
  #       column(4,highchartOutput("grafico_multiple4")),
  #       column(4,highchartOutput("grafico_multiple5"))
  #     ),
  #     fluidRow(
  #       br(),
  #       column(12,
  #              br(),
  #              tags$header(id = "header_tabla_inputs_multiple", class="text-1xl flex justify-between items-center p-5 mt-4", style="background-color: #FF671B; color: white; text-align: center", 
  #                          tags$h1(style="display: inline-block; margin: 0 auto;", class="flex-grow mt-8 mb-8",tags$b("Descripción de escenarios guardados")),
  #                          actionLink(inputId = "toggle_tabla_inputs_multiple", label=icon("stream", style = "color: white;"))
  #              ),
  #              br(),
  #              hidden(reactableOutput("prueba")),
  #              br(),
  #              br()
  #       )
  #     )
  #     
  #   )
  #   
  #   # tagList(
  #   #   br(),
  #   #   br(),
  #   #   hr(),
  #   #   highchartOutput("grafico_multiple"),
  #   #   br(),
  #   #   tags$header(id = "header_tabla_inputs_multiple", class="text-1xl flex justify-between items-center p-5 mt-4", style="background-color: #FF671B; color: white; text-align: center", 
  #   #               tags$h1(style="display: inline-block; margin: 0 auto;", class="flex-grow mt-8 mb-8",tags$b("Descripción de escenarios guardados")),
  #   #               actionLink(inputId = "toggle_tabla_inputs_multiple", label=icon("stream", style = "color: white;"))
  #   #   ),
  #   #   br(),
  #   #   reactableOutput("inputs_summary_table")
  #   #   
  #   # )
  #   
  #   
  # })
}
  


