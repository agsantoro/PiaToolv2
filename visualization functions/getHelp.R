getHelp = function(currentPage, session) {
  pagina_actual = currentPage
  if (pagina_actual == "/") {
    # Steps 1 a 3 para la página principal
    introjs(session, options = list(
      "nextLabel" = "Siguiente",
      "prevLabel" = "Anterior",
      "doneLabel" = "Cerrar",
      "exitOnEsc" = TRUE,
      steps = list(
        list(element = "[data-step='1']", intro = "Bienvenido/a al PIATools ! Para un tutorial de la herramienta, presione siguiente."),
        list(element = "[data-step='2']", intro = "En cada una de estas tarjetas se pueden ver los títulos de las intervenciones y un breve resúmen del  modelo empleado. Para comenzar a utilizar los modelos, haga click en la tarjeta correspondiente."),
        list(element = "[data-step='3']", intro = "En todas las páginas encontrará este botón para obtener ayuda sobre el contenido.")
      )
    ))
    
  } else if (pagina_actual == "hearts") {
    # Solo step 4 para la página hearts
    introjs(session, options = list(
      "nextLabel" = "Siguiente",
      "prevLabel" = "Anterior",
      "doneLabel" = "Cerrar",
      "exitOnEsc" = TRUE,
      steps = list(
        list(element = "[data-step='4']", intro = "En este panel puede modificar los parámetros básicos y avanzados del modelo."),
        list(element = "[data-step='5']", intro = "Presione este botón para visualizar los resultados."),
        list(element = "[data-step='6']", intro = "En este panel puede guardar el escenario en pantalla, generar uno nuevo, desargar la información o solicitar ayuda sobre el uso de la página.")
      )
    ))
  }
}

