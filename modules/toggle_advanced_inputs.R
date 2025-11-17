toggle_advanced_inputs = function (input, output, session) {
  # función para mostrar parámetros avanzados
  observeEvent(input$toggle_avanzado_hpv, {
    load("hpv_map_inputs.Rdata")
    for (i in hpv_map_inputs$i_names[hpv_map_inputs$avanzado==T]) {
      isVisible <- shinyjs::toggleState(id = i)
      
      toggle(id = i, anim = TRUE, animType = "slide", condition = isVisible)
      enable(i)
    }
    shinyjs::disable("totalVaccineCostPerFIG")
  })
  
  
  # observeEvent(input$toggle_avanzado_hearts, {
  #   
  #   load("hearts_map_inputs.Rdata")
  #   inputs_hide = hearts_map_inputs$i_names[hearts_map_inputs$avanzado==T]
  #   
  #   for (i in c(inputs_hide)) {
  #     isVisible <- shinyjs::toggleState(id = i)
  #     toggle(id = i, anim = TRUE, animType = "slide", condition = isVisible)
  #     enable(i)
  #   }
  # })
  # 
  
  observeEvent(input$toggle_avanzado_hpp, {
    load("hpp_map_inputs.Rdata")
    inputs_hide = hpp_map_inputs$i_names[hpp_map_inputs$avanzado==T]
    
    
    for (i in inputs_hide) {
      isVisible <- shinyjs::toggleState(id = i)
      shinyjs::toggle(id = i, anim = TRUE, animType = "slide", condition = isVisible)
      shinyjs::enable(i)
    }
  })
  
  
  observeEvent(input$toggle_avanzado_tbc, {
    load("tbc_map_inputs.Rdata")
    inputs_hide = tbc_map_inputs$i_names[tbc_map_inputs$avanzado]
    
    
    for (i in inputs_hide) {
      isVisible <- shinyjs::toggleState(id = i)
      toggle(id = i, anim = TRUE, animType = "slide", condition = isVisible)
      enable(i)
    }
  })
  
  observeEvent(input$toggle_avanzado_hepC, {
    input_names = c(
      "Costos de fibrosis descompensada" = "aCostoDC", 
      "Costos de estadíos de fibrosis F0 a F2" = "aCostoF0F2", 
      "Costos de estadío de fibrosis F3" = "aCostoF3", 
      "Costos de estadío de fibrosis F4" = "aCostoF4", 
      "Costos de carcinoma hepatocelular" = "aCostoHCC", 
      "Tasa de descuento" = "AtasaDescuento", 
      "Tamaño de la cohorte" = "cohorte", 
      "Costo de la evaluación de la respuesta al tratamiento" = "Costo_Evaluacion", 
      "Costo de tratamiento de 4 semanas de Epclusa" = "Costo_Tratamiento", 
      "Probabilidad de encontrarse en estadio de fibrosis F0 al diagnóstico" = "F0", 
      "Probabilidad de encontrarse en estadio de fibrosis F1 al diagnóstico" = "F1", 
      "Probabilidad de encontrarse en estadio de fibrosis F2 al diagnóstico" = "F2", 
      "Probabilidad de encontrarse en estadio de fibrosis F3 al diagnóstico" = "F3", 
      "Probabilidad de encontrarse en estadio de fibrosis F4 al diagnóstico" = "F4", 
      "Proporción de pacientes que abandonan el tratamiento." = "pAbandono", 
      "Eficacia de Sofosbuvir / velpatasvir" = "pSVR", 
      "Duración del tratamiento" = "tDuracion_Meses"
    )
    inputs_hide = input_names[1:14]
    
    
    for (i in inputs_hide) {
      isVisible <- shinyjs::toggleState(id = i)
      toggle(id = i, anim = TRUE, animType = "slide", condition = isVisible)
      enable(i)
    }
  })
  
  observeEvent(input$toggle_avanzado_prep, {
    inputs_hide = names(get_prep_params(input$country))[5:length(names(get_prep_params(input$country)))]
    
    
    for (i in inputs_hide) {
      isVisible <- shinyjs::toggleState(id = i)
      toggle(id = i, anim = TRUE, animType = "slide", condition = isVisible)
      enable(i)
    }
  })
  
  
  
}