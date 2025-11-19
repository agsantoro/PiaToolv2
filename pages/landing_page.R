landing_page <- div(
  
  tags$style(
    "
    body {
    padding: 0;
    }
    
    html {
  overflow:   scroll
  margin: 0;
  padding;0;
    } 
  
  .container-fluid {
  font-size: 1.3em;
        padding: 0 !important;
        margin: 0px !important;
      
    }  
    
    ::-webkit-scrollbar {
      width: 0px;
      background: transparent; /* make scrollbar transparent */
    }
    
    /* Estilos para el header fijo */
    .fixed-header {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      height: 80px;
      background: rgba(255, 255, 255, 0.95);
      backdrop-filter: blur(10px);
      border-bottom: 1px solid rgba(16, 51, 98, 0.1);
      z-index: 1000;
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 0 30px;
      box-sizing: border-box;
    }
    
    .header-logo {
      height: 50px;
      width: auto;
      object-fit: contain;
    }
    
    .header-title-container {
      display: flex;
      align-items: center;
      flex-grow: 1;
      justify-content: center;
      margin: 0 20px;
    }
    
    .header-title {
      font-size: 1.8em;
      font-weight: bold;
      color: #103362;
      text-align: center;
      margin: 0;
    }
    
    .home-icon {
      font-size: 1.5em;
      color: #103362;
      margin-left: 15px;
      cursor: pointer;
      transition: all 0.2s ease;
      padding: 8px;
      border-radius: 50%;
      background: rgba(16, 51, 98, 0.05);
    }
    
    .home-icon:hover {
      color: #2C5F8B;
      background: rgba(16, 51, 98, 0.1);
      transform: translateY(-1px);
    }
    
    .main-content {
      margin-top: 80px; /* Espacio para el header fijo */
    }
    "
  ),
  
  div(
    class = "fixed-header",
    
    # Imagen izquierda
    img(
      src = "iecslogo.png",  # Reemplaza con la ruta de tu imagen
      alt = "Logo izquierdo",
      class = "header-logo"
    ),
    
    # Contenedor del título con icono home
    div(
      class = "header-title-container",
      
      # Título centrado
      h1("Programme Impact Assessment Tool", class = "header-title"),
      
      # Icono home con link a landing page
      # tags$a(
      #   href = "?page=landing",  # Ajusta según tu sistema de navegación
      #   onclick = "Shiny.setInputValue('goto_landing', Math.random(), {priority: 'event'});",
      #   icon("home", class = "home-icon"),
      #   title = "Ir al inicio",
      #   style = "text-decoration: none;"
      # )
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
        tags$a(href = "", "Inglés")
      )
    )
      
  ),
  
  # Contenido principal con margen superior
  div(
    class = "main-content",
    style = "
        align-items: center;
    flex: 1; 
    
            padding: 0; 
            background: #F2F3FA;
            color: #103362;
            display: flex; 
            flex-direction: column; 
            justify-content: center;
            text-align: center; 
            min-height: 100vh; 
            box-sizing: border-box;",
    
    
    h3("Este portal es una herramienta clave para analizar y visualizar las tendencias, diferencias y patrones geográficos de la mortalidad en Argentina, desagregados por sexo. Ofrece información actualizada y detallada para identificar disparidades, monitorear la evolución de las brechas de género y diseñar políticas de salud equitativas.",
       class = "animate-left",
       style = "margin-bottom: 40px; opacity: 0.9;width: 60%; margin-top: 0"),
    
    # Contenedor grid para las características - MODIFICADO PARA IGUAL ALTURA
    div(
      style = "display: grid; 
               grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
               grid-auto-rows: 1fr;
               gap: 20px; 
               max-width: 1000px; 
               margin: 0 auto;
               padding: 20px 20px 20px 20px;",

      # Primera característica - CON COLOR DE FONDO SUAVE
      menuBox(
        title = "Iniciativa HEARTS",
        text = "Compare el desempeño de las diferentes jurisdicciones y regiones de Argentina.
        Analice la evolución de las brechas de género. Descargue figuras y reportes.",
        iconType = "heart",
        iconColor = "#2C5F8B",
        linkTo = "chart"
      ),
      menuBox(
        title = "Vacunación contra el VPH",
        text = "Compare el desempeño de las diferentes jurisdicciones y regiones de Argentina.
        Analice la evolución de las brechas de género. Descargue figuras y reportes.",
        iconType = "heart",
        iconColor = "#2C5F8B",
        linkTo = "chart"
      ),
      menuBox(
        title = "Tratamiento de observación directa por vídeo para tuberculosis",
        text = "Compare el desempeño de las diferentes jurisdicciones y regiones de Argentina.
        Analice la evolución de las brechas de género. Descargue figuras y reportes.",
        iconType = "heart",
        iconColor = "#2C5F8B",
        linkTo = "chart"
      )
      # menuBox(
      #   title = "Brechas de género",
      #   text = "Compare la evolución histórica de las brechas de género para los principales indicadores
      #   de mortalidad.",
      #   iconType = "chart-line",
      #   iconColor = "#737B4F",
      #   linkTo = "gaps"
      # ),
      
      
    )
  ),
  
  # FOOTER INSTITUCIONAL CIIPS
  div(
    style = "
      background: linear-gradient(135deg, #2C5F8B 0%, #1a3a5c 100%);
      color: white;
      padding: 40px 0;
      margin-top: 0;",
    
    div(
      style = "max-width: 1200px; margin: 0 auto; padding: 0 30px;",
      
      # Contenido del footer en grid
      div(
        style = "
          display: grid;
          grid-template-columns: 1fr 1fr 1fr;
          gap: 40px;
          margin-bottom: 30px;",
        
        # Columna 1: Logo y descripción
        div(
          style = "text-align: left;",
          img(
            src = "CIPS_fondo-transparente (2).png",
            style = "height: 60px; width: auto; margin-bottom: 15px; filter: brightness(0) invert(1);"
          ),
          h4("Centro de Implementación e Innovación en Políticas de Salud", 
             style = "color: white; font-size: 1.1em; font-weight: 500; margin-bottom: 10px; line-height: 1.3;"),
          p("Desarrollamos proyectos de investigación y cooperación técnica para mejorar la equidad, accesibilidad y calidad del sistema de salud.",
            style = "color: rgba(255,255,255,0.8); font-size: 0.9em; line-height: 1.5; margin: 0;")
        ),
        
        # Columna 2: Contacto
        div(
          style = "text-align: left;",
          h4("Contacto", 
             style = "color: white; font-size: 1.1em; font-weight: 500; margin-bottom: 15px;"),
          div(
            style = "color: rgba(255,255,255,0.8); font-size: 0.9em; line-height: 1.8;",
            p(style = "margin: 5px 0;", icon("envelope", style = "margin-right: 8px;"), "info@iecs.org.ar"),
            p(style = "margin: 5px 0;", icon("phone", style = "margin-right: 8px;"), "+54 11 4777-8767"),
            p(style = "margin: 5px 0;", icon("map-marker-alt", style = "margin-right: 8px;"), "Dr. Emilio Ravignani 2024, CABA")
          )
        ),
        
        # Columna 3: Enlaces institucionales
        div(
          style = "text-align: left;",
          h4("Enlaces", 
             style = "color: white; font-size: 1.1em; font-weight: 500; margin-bottom: 15px;"),
          div(
            style = "color: rgba(255,255,255,0.8); font-size: 0.9em; line-height: 1.8;",
            tags$a(
              p(style = "margin: 5px 0; cursor: pointer;", icon("external-link-alt", style = "margin-right: 8px;"), "IECS Argentina"),
              href = "https://iecs.org.ar",
              target = "_blank",
              class = "footer-link",
              style = "color: rgba(255,255,255,0.8) !important; transition: color 0.2s ease;"
            ),
            tags$a(
              p(style = "margin: 5px 0; cursor: pointer;", icon("external-link-alt", style = "margin-right: 8px;"), "CIIPS"),
              href = "https://iecs.org.ar/home-organizacion/centros/ciips/",
              target = "_blank", 
              class = "footer-link",
              style = "color: rgba(255,255,255,0.8) !important; transition: color 0.2s ease;"
            ),
            p(style = "margin: 5px 0;", icon("database", style = "margin-right: 8px;"), "Metodología")
          )
        )
      ),
      
      # Separador
      div(style = "height: 1px; background: rgba(255,255,255,0.2); margin: 30px 0;"),
      
      # Copyright y créditos
      div(
        style = "
          display: flex;
          justify-content: space-between;
          align-items: center;
          flex-wrap: wrap;
          gap: 20px;",
        
        div(
          style = "color: rgba(255,255,255,0.7); font-size: 0.85em;",
          "© 2025 CIIPS - IECS. Observatorio de inequidades de género en mortalidad."
        ),
        
        div(
          style = "display: flex; gap: 15px;",
          tags$a(
            icon("twitter", style = "font-size: 1.2em; color: rgba(255,255,255,0.7);"),
            href = "#",
            class = "footer-social",
            style = "transition: all 0.2s ease;"
          ),
          tags$a(
            icon("linkedin", style = "font-size: 1.2em; color: rgba(255,255,255,0.7);"),
            href = "#",
            class = "footer-social", 
            style = "transition: all 0.2s ease;"
          ),
          tags$a(
            icon("envelope", style = "font-size: 1.2em; color: rgba(255,255,255,0.7);"),
            href = "mailto:info@iecs.org.ar",
            class = "footer-social",
            style = "transition: all 0.2s ease;"
          )
        )
      )
    )
  ),
  
  # CSS adicional para efectos hover específicos de landing
  tags$style(HTML("
  /* Eliminar subrayado y cambios de color en enlaces */
  a, a:hover, a:focus, a:active, a:visited {
    text-decoration: none !important;
    color: inherit !important;
  }
  
  /* Específicamente para los enlaces de los cuadros */
  a .grid-item, 
  a:hover .grid-item, 
  a:focus .grid-item, 
  a:active .grid-item, 
  a:visited .grid-item {
    color: #103362 !important;
  }
  
  /* Para asegurar que el texto dentro de los cuadros mantenga el color */
  .grid-item h3,
  .grid-item p,
  .grid-item div,
  .grid-item span {
    color: #103362 !important;
  }
  
  /* Mantener solo la elevación en hover, sin cambios de color */
  .grid-item:hover {
    transform: translateY(-5px);
    box-shadow: 0 10px 25px rgba(0,0,0,0.2);
  }
  
  /* Estilos para el footer */
  .footer-link:hover {
    color: #ffffff !important;
  }
  
  .footer-social:hover {
    color: #ffffff !important;
    transform: translateY(-2px);
  }
  
  /* Responsivo para el header */
  @media (max-width: 768px) {
    .fixed-header {
      padding: 0 15px;
      height: 70px;
    }
    
    .header-title {
      font-size: 1.4em;
      margin: 0 10px;
    }
    
    .header-logo {
      height: 40px;
    }
    
    .main-content {
      margin-top: 70px;
    }
    
    div[style*='grid-template-columns'] {
      grid-template-columns: 1fr !important;
      gap: 15px !important;
      padding: 10px !important;
    }
    
    /* Footer mobile */
    div[style*='grid-template-columns: 1fr 1fr 1fr'] {
      grid-template-columns: 1fr !important;
      text-align: center !important;
    }
    
    div[style*='display: flex; justify-content: space-between'] {
      flex-direction: column !important;
      text-align: center !important;
      gap: 15px !important;
    }
  }
  
  @media (min-width: 769px) and (max-width: 1024px) {
    .header-title {
      font-size: 1.6em;
    }
    
    div[style*='grid-template-columns'] {
      grid-template-columns: repeat(2, 1fr) !important;
    }
    
    /* Footer responsive */
    div[style*='grid-template-columns: 1fr 1fr 1fr'] {
      grid-template-columns: 1fr 1fr !important;
      gap: 30px !important;
    }
    
    div[style*='grid-template-columns: 1fr 1fr 1fr'] > div:last-child {
      grid-column: 1 / -1;
      text-align: center;
    }
  }
  
  /* Efecto suave de aparición del header al hacer scroll */
  .fixed-header {
    transition: all 0.3s ease-in-out;
  }
"
  ))
)