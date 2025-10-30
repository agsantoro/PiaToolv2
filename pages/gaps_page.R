load("data/dataMortReg.rda")
load("data/dataMortProv.rda")
load("data/mortalityCauses.rda")

gaps_page <- div(
  
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
    "
  ),
  
  # header
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
      h1("Observatorio de las inequidades de género en la mortalidad", class = "header-title"),
      
      # Icono home con link a landing page
      tags$a(
        href = "?page=landing",  # Ajusta según tu sistema de navegación
        onclick = "Shiny.setInputValue('goto_landing', Math.random(), {priority: 'event'});",
        icon("home", class = "home-icon"),
        title = "Ir al inicio",
        style = "text-decoration: none;"
      )
    ),
    
    # Imagen derecha
    img(
      src = "CIPS_fondo-transparente (2).png",  # Reemplaza con la ruta de tu imagen
      alt = "Logo derecho", 
      class = "header-logo"
    )
  ),
  
  
  # Contenido principal con layout 40%-60%
  div(
    class = "main-content",
    style = "
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      min-height: calc(100vh - 85px);
      display: flex;
      flex-direction: row;",
    
    # PANEL IZQUIERDO (40%) - Azul institucional sobrio
    div(
      style = "
        width: 40%;
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
          
          icon("chart-line", style = "
    font-size: 3em; 
    color: #737B4F;
    filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1));"),
          
          h2("Brechas de género", 
             style = "margin: 0; font-size: 2em; font-weight: 500; color: #2C5F8B;")
        ),
        
        # Texto descriptivo
        p("Compare la evolución histórica de las brechas de género para los principales indicadores
        de mortalidad.",
          style = "
            font-size: 1.05em; 
            line-height: 1.6; 
            color: #495057; 
            margin-bottom: 35px;",
          class = "animate-left"),
        
        tags$div(
          style = "overflow-y: auto; height: 80%; width: 100% !important;",
          selectInput(
            inputId = "indicador",
            label = "Seleccionar indicador",
            choices = indicatorsList(),
            selected = "tmape",
            width = "100%"
            
          ),
          selectInput(
            inputId = "geo",
            label = "Segmentación geográfica",
            choices = c("Jurisdicciones","Regiones"),
            selected = "Jurisdicciones",
            multiple = F,
            width = "100%"
          ),        
          selectInput(
            inputId = "area",
            label = "Seleccionar área",
            choices= "",
            selected = "01",
            multiple = T,
            width = "100%"
          ),
          treeviewInput(
            inputId = "causas",
            label = "Seleccionar causa:",
            choices = make_tree(mortalityCauses,colnames(mortalityCauses)),
            selected = "0000 TODAS LAS CAUSAS",
            returnValue = "text",
            closeDepth = 0,
            width = "100%"
          )
        )
        
        
      )
    ),
    
    # PANEL DERECHO (60%) - Fondo limpio y profesional
    div(
      style = "
        width: 60%;
        padding: 40px;
        display: inline;
        flex-direction: column;
        justify-content: center;
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);",
      
      # Título profesional
      h2(paste0("Análisis de Datos de Mortalidad"),
         style = "
         
           font-size: 1.5em; 
           margin-bottom: 20px; 
           font-weight: 400; 
           text-align: center; 
           color: #2C5F8B;
           border-bottom: 2px solid #4A90A4;
           padding-bottom: 15px;"),
      
      p("El Centro de Implementación e Innovación en Políticas de Salud (CIIPS) presenta esta plataforma 
        de análisis interactivo para la exploración de datos de mortalidad y brechas de género en Argentina. 
        Desarrollada con metodologías basadas en evidencia para investigadores, académicos y tomadores de decisión.",
        style = "
          font-size: 1.1em; 
          margin-bottom: 40px; 
          color: #495057; 
          text-align: justify; 
          line-height: 1.7;
          padding: 0 20px;"),
      
      # Grid de características con estilo sobrio
      div(
        style = "display: grid; 
                 grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
                 grid-auto-rows: 1fr;
                 gap: 20px;
                 padding: 0 20px;",
        
        # Caja de gráfico con estilo institucional
        plotBox(
          title = "Visualización de Datos",
          text = "Gráficos interactivos para análisis epidemiológico",
          iconType = "chart-line",
          iconColor = "#737B4F",
          outputIdName = "gaps"
          
        )
      )
    )
  ),
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
      " CIIPS - IECS. Todos los derechos reservados.",
      style = "margin: 0;"
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