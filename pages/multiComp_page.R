library(shiny)
library(htmltools)

multiComp_page <- div(
  # 1. HEAD Y FUENTES
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap",
      rel = "stylesheet"
    )
  ),
  
  # 2. ESTILOS CSS GENERALES
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap');
    
    .apexcharts-canvas { min-width: 1px !important; }
    
    body, html {
      font-family: 'Roboto', sans-serif;
      padding: 0;
      margin: 0;
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      overflow-x: hidden;
    }
    
    .main-content {
      margin-top: 85px; /* Altura del header */
      display: flex;
      flex-direction: column; /* Alineación vertical */
      min-height: calc(100vh - 85px);
    }
    
    /* Header fijo */
    .fixed-header {
      position: fixed;
      top: 0; left: 0; right: 0;
      height: 85px;
      background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
      border-bottom: 3px solid #2C5F8B;
      z-index: 1000;
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 0 30px;
      box-shadow: 0 4px 20px rgba(44, 95, 139, 0.1);
    }
    
    /* Botón flotante único de retorno */
    .floating-back-container {
      position: fixed;
      bottom: 30px;
      right: 30px;
      z-index: 1000;
    }
    
    .back-btn {
      background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
      color: white !important;
      border: none;
      border-radius: 50%;
      width: 60px;
      height: 60px;
      font-size: 1.5em;
      display: flex;
      align-items: center;
      justify-content: center;
      cursor: pointer;
      box-shadow: 0 6px 15px rgba(0, 0, 0, 0.3);
      transition: all 0.3s cubic-bezier(0.175, 0.885, 0.32, 1.275);
    }
    
    .back-btn:hover {
      transform: scale(1.1) translateX(-5px);
      background: linear-gradient(135deg, #1e4368 0%, #3a7a8a 100%);
      box-shadow: 0 8px 20px rgba(0, 0, 0, 0.4);
    }

    /* Estilos de Formulario e Inputs */
    .form-control, .selectize-input {
      border: 2px solid #dee2e6 !important;
      border-radius: 6px !important;
    }
    
    .control-label {
      color: #2C5F8B !important;
      font-weight: 500 !important;
    }
  ")),
  
  # 3. HEADER
  getHeader(homeButton = T),
  
  # 4. CONTENIDO PRINCIPAL (Layout Vertical)
  div(
    class = "main-content",
    
    # --- PANEL SUPERIOR: SELECCIÓN ---
    div(
      style = "
        width: 100%;
        background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
        padding: 40px 20px;
        display: flex;
        justify-content: center;
        align-items: center;",
      
      div(
        style = "
          background: rgba(255, 255, 255, 0.95);
          border-radius: 12px;
          box-shadow: 0 8px 30px rgba(0,0,0,0.2);
          padding: 30px;
          width: 95%;
          max-width: 1200px;",
        
        div(
          style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
          icon("table", style = "font-size: 2.5em; color: #2C5F8B;"),
          div(
            h2("Comparación de escenarios guardados", 
               style = "margin: 0; font-size: 1.6em; color: #2C5F8B;"),
            p("Seleccione los escenarios para comparar los resultados proyectados", 
              style = "margin: 5px 0 0 0; color: #666;")
          )
        ),
        
        div(
          style = "border-top: 1px solid #eee; padding-top: 20px;",
          uiOutput("inputs_multicomp")
        )
      )
    ),
    
    # --- PANEL INFERIOR: RESULTADOS ---
    div(
      style = "
        width: 100%;
        padding: 40px 20px;
        background: transparent;",
      
      div(
        style = "
          max-width: 1400px;
          margin: 0 auto;
          background: white;
          padding: 30px;
          border-radius: 15px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.05);",
        
        div(
          style = "
            display: grid; 
            grid-template-columns: repeat(auto-fit, minmax(450px, 1fr));
            gap: 30px;",
          uiOutput("resultados_multiComp")
        )
      )
    )
  ),
  
  # 5. FOOTER
  getFooter(landing = FALSE),
  
  
)