tempHideInputs = function (page, input, map_inputs) {
  lapply(c("inputContainer","country",do::exec(glue("map_inputs$i_names")) ), function (i) {
    #browser()
    if (i == "country") {
      shinyjs::disable(selector = ".country-input-class")
    } else {
      disable(i)
    }
  })
}

