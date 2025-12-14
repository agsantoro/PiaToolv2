getHeader = function(homeButton) {
    div(
      class = "fixed-header",
      
      # Imagen izquierda
      img(
        src = "ops.png",  # Reemplaza con la ruta de tu imagen
        alt = "Logo izquierdo",
        class = "header-logo"
      ),
      
      # Contenedor del título con icono home
      div(
        class = "header-title-container",
        
        # Título centrado
        h1("Programme Impact Assessment Tool", class = "header-title"),
        
        if (homeButton) {
          # Icono home con link a landing page
          tags$a(
            href = "?page=landing",  # Ajusta según tu sistema de navegación
            onclick = "Shiny.setInputValue('goto_landing', Math.random(), {priority: 'event'});",
            icon("home", class = "home-icon"),
            title = "Ir al inicio",
            style = "text-decoration: none;"
          )
        }
        
      ),
      
      # Imagen derecha
      tags$div(
        class = "p-2", 
        # NOTA: El HTML original tenía id="class"="p-2", lo cual es inválido. 
        # Asumo que la intención era class="p-2" o id="p-2". He usado class="p-2".
        
        tags$div(
          class = "text-right text-lg",
          tags$a(href = "", "Español"),
          " | ", # El separador de texto simple
          tags$a(href = "", "Inglés"),
          " | ", # El separador de texto simple
          tags$a(href = "", "Portugués")
        )
      )
      
    )
  
}