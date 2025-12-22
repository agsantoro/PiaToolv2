btnSequence = function (page, input, output, session, map_inputs, map_outputs, saved_scenarios, model_comp, getCountryCode, back_btn_clicked_comp) {
  
  # aplica lógica de activar y desactivar botones para todas las páginas
  observeEvent(input[[glue("save_scenario_btn_{page}")]], {
    paste(map_outputs())
  })
  
  # desactiva botones por default
  shinyjs::disable(glue("show_comparisson_btn_{page}"))
  shinyjs::disable(glue("save_scenario_btn_{page}"))
  shinyjs::disable(glue("new_scenario_btn_{page}"))
  shinyjs::disable(glue("download_scenario_btn_{page}"))
  
  observeEvent(input[[glue("{page}_go")]], {
    enable(glue("save_scenario_btn_{page}"))
    enable(glue("new_scenario_btn_{page}"))
    enable(glue("download_scenario_btn_{page}"))
  })
  
  observeEvent(input[[glue("new_scenario_btn_{page}")]], {
    shinyjs::disable(glue("save_scenario_btn_{page}"))
    shinyjs::disable(glue("new_scenario_btn_{page}"))
    shinyjs::disable(glue("download_scenario_btn_{page}"))
    shinyjs::disable("country")
    hide(glue("resultados_{page}"))
    
    lapply(c("inputContainer","country",do::exec(glue("map_inputs()$i_names"))), 
           function (i) {
             enable(i)
           })
    
  })
  
  observeEvent(input[[glue("save_scenario_btn_{page}")]], {
    
    map_inputs = do::exec(glue("data.frame(isolate(map_inputs()))"))
    map_outputs = do::exec(glue("data.frame(isolate(map_outputs()))"))
    
    if (isolate(back_btn_clicked_comp()) == F) {
      shinyjs::show(id = "saveScenarioModal")
      shinyjs::show(id = "modalOverlay")
    }
    
    back_btn_clicked_comp(F)
    
    onclick("closeSaveBtn", {
      # Ocultar el modal de guardado y el overlay
      shinyjs::hide(id = "saveScenarioModal")
      shinyjs::hide(id = "modalOverlay")
    })
    
    onclick("modalOverlay", {
      # Ocultar AMBOS modales (si están abiertos) y el overlay
      shinyjs::hide(id = "elegantModal")
      shinyjs::hide(id = "saveScenarioModal")
      shinyjs::hide(id = "modalOverlay")
    })
    
    onclick("confirm_save", {
      # Ocultar AMBOS modales (si están abiertos) y el overlay
      dataScenario = list(
        "model" = isolate(get_page()),
        "country" = isolate(input$country),
        "outputs" = map_outputs,
        "inputs" = map_inputs
      )
      
      if (input$scenario_name %in% names(saved_scenarios())) {
        showNotification(
          duration = 3,
          type = "error",
          "Ya existe un escenario guardado con ese nombre")
        return()
      }
      
      if (substring(input$scenario_name,1,1) == " ") {
        showNotification(
          duration = 3,
          type = "error",
          "El nombre no debe empezar con espacio")
        return()
      }
      
      scenario_name_valid = input$scenario_name
      
      showNotification(
        duration = 3,
        type = "message",
        glue("Escenario guardado: {scenario_name_valid}"))
      
      appendData = saved_scenarios()
      appendData[[input$scenario_name]] = dataScenario
      saved_scenarios(appendData)
      
      shinyjs::hide(id = "saveScenarioModal")
      shinyjs::hide(id = "modalOverlay")
      updateTextInput(session,"scenario_name", value = "")
    })
    
    observeEvent(saved_scenarios(),{
      if (length(saved_scenarios())>1) {
        enable(glue("show_comparisson_btn_{page}"))
      } else {
        disable("show_comparisson_btn_{page}")
      }
    })
    
    onclick(glue("show_comparisson_btn_{page}"), {
      model_comp(get_page())
    })
  })
  
  
  output$inputs_comparisson = renderUI({
    dataComp = saved_scenarios()
    ui_comparisson(input, dataComp, model_comp(), getCountryCode)
  })

  output$resultados_comparisson = renderUI({
    tagList(
      ui_resultados_comparisson(input,output,session,saved_scenarios(), isolate(model_comp()))
    )
  })
  
}

