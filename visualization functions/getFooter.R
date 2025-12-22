getFooter = function (landing) {
  # if (landing == T) {
  #   div(
  #     style = "
  #     background: linear-gradient(135deg, #2C5F8B 0%, #1a3a5c 100%);
  #     color: white;
  #     padding: 40px 0;
  #     margin-top: 0;",
  #     
  #     div(
  #       style = "max-width: 1200px; margin: 0 auto; padding: 0 30px;",
  #       
  #       # Contenido del footer en grid
  #       div(
  #         style = "
  #         display: grid;
  #         grid-template-columns: 1fr 1fr 1fr;
  #         gap: 40px;
  #         margin-bottom: 30px;",
  #         
  #         # Columna 1: Logo y descripción
  #         div(
  #           style = "text-align: left;",
  #           
  #           h4("Instituto de Efectividad Clínica y Sanitaria", 
  #              style = "color: white; font-size: 1.1em; font-weight: 500; margin-bottom: 10px; line-height: 1.3;"),
  #           p("Desarrollamos proyectos de investigación y cooperación técnica para mejorar la equidad, accesibilidad y calidad del sistema de salud.",
  #             style = "color: rgba(255,255,255,0.8); font-size: 0.9em; line-height: 1.5; margin: 0;")
  #         ),
  #         
  #         # Columna 2: Contacto
  #         div(
  #           style = "text-align: left;",
  #           h4("Contacto", 
  #              style = "color: white; font-size: 1.1em; font-weight: 500; margin-bottom: 15px;"),
  #           div(
  #             style = "color: rgba(255,255,255,0.8); font-size: 0.9em; line-height: 1.8;",
  #             p(style = "margin: 5px 0;", icon("envelope", style = "margin-right: 8px;"), "info@iecs.org.ar"),
  #             p(style = "margin: 5px 0;", icon("phone", style = "margin-right: 8px;"), "+54 11 4777-8767"),
  #             p(style = "margin: 5px 0;", icon("map-marker-alt", style = "margin-right: 8px;"), "Dr. Emilio Ravignani 2024, CABA")
  #           )
  #         ),
  #         
  #         # Columna 3: Enlaces institucionales
  #         div(
  #           style = "text-align: left;",
  #           h4("Enlaces", 
  #              style = "color: white; font-size: 1.1em; font-weight: 500; margin-bottom: 15px;"),
  #           div(
  #             style = "color: rgba(255,255,255,0.8); font-size: 0.9em; line-height: 1.8;",
  #             tags$a(
  #               p(style = "margin: 5px 0; cursor: pointer;", icon("external-link-alt", style = "margin-right: 8px;"), "IECS Argentina"),
  #               href = "https://iecs.org.ar",
  #               target = "_blank",
  #               class = "footer-link",
  #               style = "color: rgba(255,255,255,0.8) !important; transition: color 0.2s ease;"
  #             ),
  #             tags$a(
  #               p(style = "margin: 5px 0; cursor: pointer;", icon("external-link-alt", style = "margin-right: 8px;"), "CIIPS"),
  #               href = "https://iecs.org.ar/home-organizacion/centros/ciips/",
  #               target = "_blank", 
  #               class = "footer-link",
  #               style = "color: rgba(255,255,255,0.8) !important; transition: color 0.2s ease;"
  #             ),
  #             p(style = "margin: 5px 0;", icon("database", style = "margin-right: 8px;"), "Metodología")
  #           )
  #         )
  #       ),
  #       
  #       # Separador
  #       div(style = "height: 1px; background: rgba(255,255,255,0.2); margin: 30px 0;"),
  #       
  #       # Copyright y créditos
  #       div(
  #         style = "
  #         display: flex;
  #         justify-content: space-between;
  #         align-items: center;
  #         flex-wrap: wrap;
  #         gap: 20px;",
  #         
  #         div(
  #           style = "color: rgba(255,255,255,0.7); font-size: 0.85em;",
  #           "© 2025 IECS. Programme Impact Assessment Tool."
  #         ),
  #         
  #         div(
  #           style = "display: flex; gap: 15px;",
  #           tags$a(
  #             icon("twitter", style = "font-size: 1.2em; color: rgba(255,255,255,0.7);"),
  #             href = "#",
  #             class = "footer-social",
  #             style = "transition: all 0.2s ease;"
  #           ),
  #           tags$a(
  #             icon("linkedin", style = "font-size: 1.2em; color: rgba(255,255,255,0.7);"),
  #             href = "#",
  #             class = "footer-social", 
  #             style = "transition: all 0.2s ease;"
  #           ),
  #           tags$a(
  #             icon("envelope", style = "font-size: 1.2em; color: rgba(255,255,255,0.7);"),
  #             href = "mailto:info@iecs.org.ar",
  #             class = "footer-social",
  #             style = "transition: all 0.2s ease;"
  #           )
  #         )
  #       )
  #     )
  #   )
  # } else {
    div(
      style = "
    background: linear-gradient(135deg, #2C5F8B 0%, #1a3a5c 100%);
    color: white;
    padding: 10px 0;
    text-align: center;
    margin-top: 0;
    font-size: 0.65em;
    width: 100%;", # Asegura que el footer se extienda a lo ancho
      tags$p(
        
        as.character(format(Sys.Date(), "%Y")), # Obtiene el año actual dinámicamente
        " OPS - Departamento de Evidencia e Inteligencia para la Acción en Salud (EIH)",
        style = "margin: 0;"
      )
    )
  # }
  
}
