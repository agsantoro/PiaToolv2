tempHideInputs = function (page, input, map_inputs) {
  lapply(c(
    "country",
    glue("go-btn-container-{page}"),
    "inputContainer",
    do::exec(glue("map_inputs$i_names"))), 
    function (i) {
    if (i == "country") {
      shinyjs::disable(selector = ".country-input-class")
    } else {
      disable(i)
    }
  })
}

