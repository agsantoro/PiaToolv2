ui_inputs_multiComp = function (input, saved_scenarios, current_page, getCountryCode) {
  browser()
  if (length(saved_scenarios)>1 & length(current_page)>0) {
    browser()
  } else {
    return()
  }
}


ui_resultados_multiComp = function(input,output,session,current_page) {
  if (get_page()!="multiComp") {return()}
  #if (get_page=="comparisson" | input$compScenariosNames == "") {return()}
  reactable(iris)
}


