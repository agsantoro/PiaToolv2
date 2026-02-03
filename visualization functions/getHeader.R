getHeader = function(homeButton) {
  div(
    class = "fixed-header",
    
    # Imagen izquierda
    img(
      src = "logo-OPS.png",
      alt = "Logo izquierdo",
      class = "header-logo"
    ),
    
    # Línea vertical separadora
    div(class = "header-divider"),
    
    # Contenedor del título con icono home
    div(
      class = "header-title-container",
      
      # Título alineado a la izquierda
      h1("Programme Impact Assessment Tool", class = "header-title"),
      
      if (homeButton) {
        # Icono home con link a landing page
        tags$a(
          href = "?page=landing",
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
      
      tags$div(
        class = "text-right text-lg",
        tags$a(href = "", "Español"),
        " | ",
        tags$a(href = "", "Inglés"),
        " | ",
        tags$a(href = "", "Portugués")
      )
    )
  )
}