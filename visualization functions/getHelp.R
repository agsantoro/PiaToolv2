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
        list(element = "[data-step='3']", intro = "<div style='display: grid;
            grid-template-columns: auto 1fr;
            gap: 15px;
            align-items: center;
            padding: 15px;
            background: #f8f9fa;
            border-radius: 12px;
            max-width: 400px;'>
  
  <!-- Columna izquierda: Icono centrado -->
  <div style='display: flex;
              align-items: center;
              justify-content: center;'>
    <button id='help' 
            type='button' 
            class='btn action-button floating-btn' 
            title='Ayuda de navegación'
            style='background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
                   color: white;
                   border: none;
                   border-radius: 50%;
                   width: 50px;
                   height: 50px;
                   font-size: 1.2em;
                   display: flex;
                   align-items: center;
                   justify-content: center;
                   cursor: pointer;
                   box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
                   transition: all 0.3s ease;'>
      <i class='fa fa-question' role='presentation' aria-label='question icon'></i>
    </button>
  </div>
  
  <!-- Columna derecha: Texto alineado al margen izquierdo -->
  <div style='display: flex;
              align-items: center;'>
    <p style='margin: 0;
              color: #103362;
              font-size: 0.95em;
              line-height: 1.4;
              text-align: left;'>
      En todas las páginas encontrará este botón para obtener ayuda sobre el contenido.
    </p>
  </div>
  
</div>")
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

