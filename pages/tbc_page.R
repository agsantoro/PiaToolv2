library(shiny)
library(htmltools)

paises_info <- list(
  # El valor (value) es lo que se retorna; El nombre (name) es lo que se muestra
  "ARGENTINA" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ar.svg",
      alt = "Bandera Argentina", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Argentina")
  ),
  "BRAZIL" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/br.svg",
      alt = "Bandera Brazil", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Brazil")
  ),
  "CHILE" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cl.svg",
      alt = "Bandera Chile", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Chile")
  ),
  "COLOMBIA" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/co.svg",
      alt = "Bandera Colombia", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Colombia")
  ),
  "COSTA RICA" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cr.svg",
      alt = "Bandera Costa Rica", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Costa Rica")
  ),
  "JAMAICA" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/jm.svg",
      alt = "Bandera Jamaica", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Jamaica")
  ),
  "MEXICO" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mx.svg",
      alt = "Bandera Mexico", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Mexico")
  ),
  "PERU" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pe.svg",
      alt = "Bandera Peru", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Peru")
  ),
  "REPÚBLICA DOMINICANA" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/do.svg",
      alt = "Bandera República Dominicana", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("República Dominicana")
  ),
  "URUGUAY" = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/uy.svg",
      alt = "Bandera Uruguay", width = 20, height = 15, style = "margin-right: 8px;"
    ),
    tags$span("Uruguay")
  )
  
)

options_con_html <- lapply(names(paises_info), function(cod) {
  list(value = cod, content = as.character(paises_info[[cod]]))
})

# Le damos formato de lista con nombres para usar en el argumento 'choices'
paises_con_banderas <- setNames(
  lapply(options_con_html, function(x) x$content),
  lapply(options_con_html, function(x) x$value)
)


tbc_page <- div(
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap",
      rel = "stylesheet"
    )
  ),
  
  tags$style(
    "
    .apexcharts-canvas {
    min-width: 1px !important;
    }
    
    
    @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap');
    
    body, html {
      font-family: 'Roboto', sans-serif;
      padding: 0;
      margin: 0;
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    }
    
    h1, h2, h3, h4, h5, h6, p, span, div:not([class*='icon']):not([class*='fa']) {
      font-family: 'Roboto', sans-serif;
    }
    
    .dropdown-menu {
    position: absolute; 
    top: 100%;          
    left: 0;
    z-index: 10;        
    
}
    i, .fa, .fas, .far, .fab, .fal, .fad,
    [class*='icon-'], [class^='icon-'] {
      font-family: 'Font Awesome 5 Free', 'Font Awesome 5 Pro', 'FontAwesome', inherit !important;
    }
    
    html {
      overflow: scroll;
      margin: 0;
      padding: 0;
    } 
  
    .container-fluid {
      font-size: 1em;
      padding: 0 !important;
      margin: 0px !important;
    }  
    
    ::-webkit-scrollbar {
      width: 6px;
      background: #f1f3f4;
    }
    
    ::-webkit-scrollbar-thumb {
      background: linear-gradient(45deg, #2C5F8B, #4A90A4);
      border-radius: 10px;
    }
    
    /* Header profesional con identidad CIIPS */
    .fixed-header {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      height: 85px;
      background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
      border-bottom: 3px solid #2C5F8B;
      z-index: 1000;
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 0 30px;
      box-sizing: border-box;
      box-shadow: 0 4px 20px rgba(44, 95, 139, 0.1);
    }
    
    .header-logo {
      height: 55px;
      width: auto;
      object-fit: contain;
      
    }
    
    .header-title {
      font-size: 1.8em;
      font-weight: 500;
      color: #2C5F8B;
      text-align: center;
      flex-grow: 1;
      margin: 0 20px;
    }
    
    .main-content {
      margin-top: 85px;
    }
    
    /* Desactivar efectos de enlace */
    a, a:hover, a:focus, a:active, a:visited {
      text-decoration: none !important;
      color: inherit !important;
    }
    
    /* Estilos para los nuevos botones fijos */
    .floating-buttons-container {
      position: fixed;
      bottom: 20px;
      right: 20px;
      display: flex;
      flex-direction: column;
      gap: 10px; /* Separación uniforme entre botones */
      z-index: 1000; /* Asegura que estén sobre el contenido */
    }
    
    .floating-btn {
      /* Estilo general para todos los botones flotantes */
      background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
      color: white;
      border: none;
      border-radius: 50%; /* Botón circular */
      width: 50px;
      height: 50px;
      font-size: 1.2em;
      display: flex;
      align-items: center;
      justify-content: center;
      cursor: pointer;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
      transition: all 0.3s ease;
    }
    
    .floating-btn:hover {
      background: linear-gradient(135deg, #1e4368 0%, #3a7a8a 100%);
      transform: scale(1.05);
      box-shadow: 0 6px 15px rgba(0, 0, 0, 0.4);
    }
    
    /* Para el botón principal que no tiene el texto del icono */
    .floating-btn .fa {
      margin: 0 !important;
    }
    "
  ),
  
  getHeader(homeButton = T),
  
  
  # Contenido principal con layout 40%-60%
  div(
    class = "main-content",
    style = "
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      min-height: calc(100vh - 85px);
      display: flex;
      flex-direction: row;",
    
    # PANEL IZQUIERDO (30%) - Azul institucional sobrio
    div(
      style = "
        width: 30%;
        background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
        padding: 40px;
        display: inline;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        text-align: center;",
      
      
      # MenuBox con estilo institucional
      div(
        style = "
          background: rgba(255, 255, 255, 0.95);
          padding: 40px;
          border-radius: 12px;
          box-shadow: 0 8px 30px rgba(44, 95, 139, 0.15);
          
          width: 100%;
          border: 1px solid rgba(44, 95, 139, 0.1);",
        
        div(
          style = "
    display: flex;
    align-items: center;
    justify-content: center;
    margin-bottom: 25px;
    gap: 15px;",
          
          icon("lungs", style = "
    font-size: 3em; 
    color: #2C5F8B;
    filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1));"),
          
          h2("Tratamiento de observación directa por vídeo para tuberculosis (VDOT)", 
             style = "margin: 0; font-size: 1.5em; font-weight: 500; color: #2C5F8B;")
        ),
        
        # Texto descriptivo
        p(strong("Seleccione los parámetros del modelo"),
          style = "
            font-size: 1.05em;
            line-height: 1.6;
            color: #B985A9;
            margin-bottom: 35px;",
          class = "animate-left"),
        
        tags$div(
          style = "overflow-y: auto; height: 80%; width: 100% !important;",
          div(
            id = "inputContainer",
            div(
              id = "wrapper_country", class = "country-input-class",
              pickerInput(
                inputId = "country",
                label = "Selecciona un País:",
                choices = names(paises_con_banderas),
                selected = "AR", # Argentina por defecto
                choicesOpt = list(
                  content = unname(paises_con_banderas) # Le pasamos el vector de HTML
                ),
                options = list(
                  style = "btn-info", # Estilo elegante (bootstrap)
                  liveSearch = TRUE,  # Permite buscar
                  size = 5           # Muestra 5 elementos antes de scroll
                )
              )
            )
            
          ,
          uiOutput("inputs_tbc")
          )
        )
        
        
      )
    ),
    
    # PANEL DERECHO (70%) - Fondo limpio y profesional
    div(
      style = "
        width: 70%;
        padding: 20px;
        display: inline;
        flex-direction: column;
        justify-content: center;
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);",
      
      div(
        style = "
        
        
        padding: 30px;
        border-radius: 10px;
        backdrop-filter: blur(10px);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        cursor: pointer;
        display: flex;
        flex-direction: column;
        height: 100%;
          box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1); 
        ",
      
      div(
        style = "
        /* Fondo degradado discreto basado en #EE8223 */
        background: linear-gradient(180deg, rgba(238, 130, 35, 0.05) 0%, rgba(255, 255, 255, 1) 100%);
        border-radius: 8px; /* Bordes suaves */
        padding: 20px 0; /* Padding vertical para no chocar con el contenido interno */
        margin-bottom: 20px; /* Separación del gráfico de resultados */
        border: 1px solid rgba(238, 130, 35, 0.1); /* Borde muy sutil */
      ",
        p("El modelo de VDOT (Tratamiento Directamente Observado por Video) permite evaluar el impacto de este tipo de tratamiento en la carga de enfermedad por Tuberculosis pulmonar modificando parámetros como el porcentaje de adherencia a vDOT y los costos del tratamiento.",
          style = "
          font-size: 0.9em; 
          margin-bottom: 5px; 
          color: #495057; 
          text-align: justify; 
          line-height: 1.7;
          padding: 0 20px;"),
        
        p("Con este modelo podrá obtener indicadores como la cantidad de muertes evitadas, los años de vida ajustados por discapacidad evitados, el costo total de la intervención y el retorno de inversión (ROI).
",
          style = "
          font-size: 0.9em; 
          margin-bottom: 5px; 
          color: #495057; 
          text-align: justify; 
          line-height: 1.7;
          padding: 0 20px;"),
        
        p("Este modelo está basado en el modelo descrito por Fekadu y cols.",
          style = "
        font-size: 0.9em; 
        margin-bottom: 5px; 
        color: #495057; 
          text-align: justify; 
        line-height: 1.7;
        padding: 0 20px;"),
        
      ),
        
        
      # Grid de características con estilo sobrio
      div(
        style = "display: grid; 
                 grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
                 grid-auto-rows: 1fr;
                 gap: 20px;
                 padding: 0 20px;",
        
        # Caja de gráfico con estilo institucional
        hidden(uiOutput("resultados_tbc"))
      )
      )
        
        
      
      
    )
  ),
  
  
  getFooter(landing=F),
  
  # Contenedor de botones flotantes (AÑADIDO)
  div(
    class = "floating-buttons-container",
    
    tags$a(
      actionButton(
        inputId = "show_comparisson_btn_tbc",
        label = NULL,
        icon = icon("table"),
        class = "floating-btn",
        title = "Mostrar comparación de escenarios",
        style = "margin-bottom: 6px;"
      ),
      href = route_link("comparisson")
    ),
    # Botón 1: Guardar Escenario (Encima del de Crear)
    actionButton(
      inputId = "save_scenario_btn_tbc",
      label = NULL,
      icon = icon("save"), # Icono de disquete (save)
      class = "floating-btn",
      title = "Guardar Escenario en Pantalla"
    ),
    
    # Botón 2: Nuevo Escenario
    actionButton(
      inputId = "new_scenario_btn_tbc",
      label = NULL,
      icon = icon("rocket"), # Icono de cohete (nuevo escenario)
      class = "floating-btn",
      title = "Crear Nuevo Escenario"
    ),
    
    # Botón Fijo Existente (Asumo que era un botón para algo como "Descargar")
    # Usaré un icono de descarga y un ID genérico para este.
    actionButton(
      inputId = "download_scenario_btn_tbc",
      label = NULL,
      icon = icon("download"),
      class = "floating-btn",
      title = "Descargar Resultados"
    )
  ),
  
  # CSS adicional para estilo institucional sobrio
  tags$style(HTML("
    /* Eliminar subrayado y cambios de color en enlaces */
    a, a:hover, a:focus, a:active, a:visited {
      text-decoration: none !important;
      color: inherit !important;
    }
    
    /* Estilos para inputs con diseño institucional */
    .form-control, .selectize-control .selectize-input {
      background: #ffffff !important;
      border: 2px solid #dee2e6 !important;
      border-radius: 6px !important;
      color: #495057 !important;
      transition: all 0.2s ease;
      font-size: 0.95em;
    }
    
    .form-control:focus, .selectize-control.focus .selectize-input {
      background: #ffffff !important;
      border-color: #2C5F8B !important;
      box-shadow: 0 0 0 3px rgba(44, 95, 139, 0.1) !important;
    }
    
    .control-label {
      color: #2C5F8B !important;
      font-weight: 500 !important;
      font-size: 0.9em;
      margin-bottom: 8px;
    }
    
    /* Estilo sobrio para selectores */
    .selectize-dropdown {
      background: #ffffff;
      border: 1px solid #2C5F8B;
      border-radius: 6px;
      box-shadow: 0 4px 15px rgba(44, 95, 139, 0.1);
    }
    
    .selectize-dropdown-content .option {
      color: #495057;
      padding: 8px 12px;
    }
    
    .selectize-dropdown-content .option:hover {
      background: #f1f6f9;
      color: #2C5F8B;
    }
    
    /* Slider con colores institucionales */
    .irs-bar {
      background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%) !important;
    }
    
    .irs-handle {
      background: #2C5F8B !important;
      border: 2px solid #ffffff !important;
    }
    
    /* Mantener color del texto en los cuadros */
    a .grid-item,
    a .grid-item h3,
    a .grid-item p {
      color: #2C5F8B !important;
    }
    
    /* Efecto hover suave para menuBox */
    .menu-box:hover {
      transform: translateY(-3px);
      box-shadow: 0 12px 30px rgba(44, 95, 139, 0.15);
      border-color: #2C5F8B;
    }
    
    /* Hover para el botón principal con estilo institucional */
    a div[style*='linear-gradient(135deg, #2C5F8B']:hover {
      background: linear-gradient(135deg, #1e4368 0%, #3a7a8a 100%) !important;
      transform: translateY(-2px);
      box-shadow: 0 6px 20px rgba(44, 95, 139, 0.3);
    }
    
    /* Transiciones suaves y profesionales */
    * {
      transition: all 0.2s ease;
    }
    
    /* Efectos sutiles para inputs */
    .form-control:hover {
      border-color: #4A90A4 !important;
      box-shadow: 0 2px 8px rgba(44, 95, 139, 0.08);
    }
    
    /* Estilo para plotBox institucional */
    .plot-box {
      background: #ffffff;
      border: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 25px;
      box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
      transition: all 0.2s ease;
    }
    
    .plot-box:hover {
      box-shadow: 0 8px 25px rgba(44, 95, 139, 0.1);
      border-color: #2C5F8B;
    }
    
    .plot-box h3 {
      color: #2C5F8B;
      font-weight: 500;
      margin-bottom: 15px;
    }
    
    .plot-box p {
      color: #6c757d;
      line-height: 1.5;
    }
    
    .plot-box i {
      color: #2C5F8B;
      font-size: 2.5em;
      margin-bottom: 15px;
    }
    
    /* Responsive design profesional */
    @media (max-width: 1024px) {
      .main-content {
        flex-direction: column !important;
      }
      
      .main-content > div[style*='width: 40%'], 
      .main-content > div[style*='width: 60%'] {
        width: 100% !important;
      }
      
      .fixed-header {
        height: 75px;
        padding: 0 20px;
      }
      
      .header-title {
        font-size: 1.5em;
        margin: 0 15px;
      }
      
      .header-logo {
        height: 45px;
      }
      
      .main-content {
        margin-top: 75px;
      }
      
      div[style*='grid-template-columns'] {
        grid-template-columns: repeat(2, 1fr) !important;
        padding: 0 10px;
      }
    }
    
    @media (max-width: 768px) {
      div[style*='grid-template-columns'] {
        grid-template-columns: 1fr !important;
        gap: 15px !important;
        padding: 0 10px !important;
      }
      
      .header-title {
        font-size: 1.2em;
      }
      
      .fixed-header {
        height: 70px;
        padding: 0 15px;
      }
      
      .main-content {
        margin-top: 70px;
      }
      
      .floating-buttons-container {
        bottom: 10px;
        right: 10px;
      }
      
      .floating-btn {
        width: 45px;
        height: 45px;
      }
    }
    
    /* Animación sutil de entrada */
    @keyframes fadeIn {
      from {
        opacity: 0;
        transform: translateY(20px);
      }
      to {
        opacity: 1;
        transform: translateY(0);
      }
    }
    
    .menu-box {
      animation: fadeIn 0.5s ease-out;
    }
    
    /* Estilo para elementos de formulario con marca CIIPS */
    .form-group {
      margin-bottom: 20px;
    }
    
    .btn-primary {
      background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
      border: none;
      border-radius: 6px;
      padding: 10px 20px;
      font-weight: 500;
      transition: all 0.2s ease;
    }
    
    .btn-primary:hover {
      background: linear-gradient(135deg, #1e4368 0%, #3a7a8a 100%);
      transform: translateY(-1px);
      box-shadow: 0 4px 12px rgba(44, 95, 139, 0.2);
    }
    
    /* Tipografía institucional */
    h1, h2, h3 {
      font-weight: 500;
      letter-spacing: -0.02em;
    }
    
    p {
      font-weight: 400;
      letter-spacing: 0.01em;
    }
    
    /* Separadores sutiles */
    .section-separator {
      height: 1px;
      background: linear-gradient(90deg, transparent 0%, #dee2e6 50%, transparent 100%);
      margin: 30px 0;
    }
  "))
)