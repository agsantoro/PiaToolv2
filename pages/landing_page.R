landing_page <- div(
  tags$script(HTML("
  // ========================
  // Cerrar IntroJS al click fuera
  // ========================
  document.addEventListener('click', function(e) {
    // Si el click ocurre dentro del tooltip, no cerrar
    if (e.target.closest && e.target.closest('.introjs-tooltip')) return;

    // Si el click ocurre en el overlay, cerrar el tour
    if (e.target.classList.contains('introjs-overlay') ||
        (e.target.closest && e.target.closest('.introjs-overlay'))) {

      if (window.introJs) {
        window.introJs().exit();
      }
    }
  });

  // ========================
  // Cerrar con tecla ESC
  // ========================
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape' && window.introJs) {
      window.introJs().exit();
    }
  });
")),
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
  
  #help {
  background-color: #dce7f4 !important;
          padding: 12px;
          color: #2C5F8B;
          border-radius: 12px;
          box-shadow: 0 8px 30px rgba(44, 95, 139, 0.15);
          
          width: 100%;
          border: 1px solid rgba(44, 95, 139, 0.1);
}
 
     /* Efecto hover suave para help */
    #help:hover {
      transform: translateY(-3px);
      box-shadow: 0 12px 30px rgba(44, 95, 139, 0.15);
      border: 1px solid rgba(44, 95, 139, 0.1);
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
  z-index: 2000; /* Suficientemente alto, pero no rompe IntroJS */
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0 30px;
  box-sizing: border-box;
   }
.introjs-hint {
visibility: hidden;
}

.introjs-helperLayer,
.introjs-tooltipReferenceLayer,
.introjs-showElement {
  z-index: 3000 !important;  /* mayor que el header */
  
}

.introjs-tooltip {

  color: #2c3e50 !important;
  padding: 25px 30px !important;
  border-radius: 14px !important;
  box-shadow: 0 10px 40px rgba(0,0,0,0.15) !important;
  max-width: 420px !important;
  font-family: 'Roboto', sans-serif !important;
  border: 1px solid rgba(16,51,98,0.1) !important;
}

.introjs-overlay {
    pointer-events: auto !important;
    z-index: 4000 !important;
    cursor: pointer !important;
  }
  
  /* Bloquear todos los enlaces durante el tour */
  body:has(.introjs-overlay) a:not(.introjs-tooltip a) {
    pointer-events: none !important;
    cursor: default !important;
    opacity: 0.7;
  }
  
  /* Permitir clicks solo en el elemento destacado */
  .introjs-showElement a {
    pointer-events: auto !important;
    cursor: pointer !important;
    opacity: 1 !important;
  }

/* Capa que resalta el elemento (highlight) */
.introjs-helperLayer {
  z-index: 5000 !important;
  box-shadow: rgba(33, 33, 33, 0.8) 0px 0px 0px 0px,
              rgba(33, 33, 33, 0.5) 0px 0px 0px 5000px !important;
}

/* Elemento destacado */
.introjs-showElement {
  z-index: 6000 !important;
}

/* Capa base del tooltip */
.introjs-tooltipReferenceLayer {
  z-index: 7000 !important;
}

/* Tooltip de IntroJS – SIEMPRE arriba de todo */
.introjs-tooltip {
  z-index: 8000 !important;
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
    
    #help.floating-btn  {
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
  
  getHeader(homeButton = F),
  
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
            box-sizing: border-box;
    text-align; center !important;",
    
    introBox(
      h3("La Herramienta PIA Tool es una solución interactiva diseñada para guiar a los usuarios en la evaluación del impacto epidemiológico y económico de intervenciones prioritarias identificadas por la Organización Mundial de la Salud.",
         class = "animate-left",
         style = "margin-bottom: 40px; 
                  opacity: 0.9;width: 60%; 
                  margin-top: 20px; 
                  margin-left: auto;
            margin-right: auto;"),
      data.step = 1,
      data.intro = "Bienvenido/a al PIATools ! Para un tutorial de la herramienta, presione siguiente."
    ),    
    
    div(
      class = "floating-buttons-container",
      introBox(
        
        actionButton(
          inputId = "help",
          label = NULL,
          icon = icon("question"),
          class = "floating-btn",
          title = "Ayuda de navegación"
        ),
        data.step = 3,
        data.intro = "En todas las páginas encontrará este botón para obtener ayuda sobre el contenido.",
        data.hint = "You can press me"
      )
      ),
    
    # Contenedor grid para las características - MODIFICADO PARA IGUAL ALTURA
    introBox(
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
          text = "El modelo de la iniciativa HEARTS permite evaluar el impacto de aumentar la cobertura del tratamiento farmacológico de personas con hipertensión ya diagnosticadas en la carga de enfermedad cardio y cerebrovascular modificando diversos parámetros como el porcentaje de cobertura de tratamiento objetivo y el costo farmacológico anual promedio por paciente.",
          iconType = "heart",
          iconColor = "#2C5F8B",
          linkTo = "hearts"
        ),
        menuBox(
          title = "Vacunación contra el VPH",
          text = "El modelo de la vacunación contra el virus del papiloma humano (VPH) permite evaluar el impacto del aumento de cobertura de vacunación contra el VPH para las niñas en la carga de enfermedad por cáncer de cuello uterino modificando diversos parámetros como el porcentaje de cobertura de vacunación objetivo, la edad de vacunación y el costo de vacunación.",
          iconType = "syringe",
          iconColor = "#2C5F8B",
          linkTo = "hpv"
        ),
        menuBox(
          title = "Tratamiento de observación directa por vídeo para tuberculosis (VDOT)",
          text = "El modelo de VDOT permite evaluar el impacto de este tipo de tratamiento en la carga de enfermedad por Tuberculosis pulmonar modificando parámetros como el porcentaje de adherencia a vDOT y los costos del tratamiento.",
          iconType = "lungs",
          iconColor = "#2C5F8B",
          linkTo = "tbc"
        ),
        menuBox(
          title = "Tratamiento para la hepatitis C crónica",
          text = "El modelo del uso de tratamiento específico para Hepatitis C Crónica le permite evaluar el impacto del uso del mismo en personas ya diagnosticadas, con distintos estadíos de fibrosis hepática y que nunca han recibido tratamiento anteriormente, en la carga de enfermedad por Hepatitis C Crónica.",
          iconType = "virus",
          iconColor = "#2C5F8B",
          linkTo = "hepC"
        ),
        menuBox(
          title = "Uso de oxitocina para la prevención de la hemorragia post parto",
          text = "El modelo del uso de Oxitocina para la prevención de Hemorragia Post Parto permite evaluar el impacto del aumento de cobertura del uso de oxitocina durante el parto en la carga de enfermedad por hemorragia postparto modificando parámetros como el porcentaje de uso de oxitocina y el costo de la misma.",
          iconType = "baby",
          iconColor = "#2C5F8B",
          linkTo = "hpp"
        ),
        menuBox(
          title = "Profilaxis pre exposición (PrEP) para VIH",
          text = "El modelo de PrEP permite evaluar el impacto del uso de profilaxis pre exposición oral en personas con alto riesgo de infección por el Virus de la Inmunodeficiencia Humana (VIH) en la carga de enfermedad por esta infección modificando parámetros como el porcentaje de adherencia a la medicación y el costo anual del uso de PrEP.",
          iconType = "pills",
          iconColor = "#2C5F8B",
          linkTo = "prep"
        ),
        menuBox(
          title = "Tests rápidos en punto de cuidado para sífilis gestacional",
          text = "Este modelo permite evaluar el impacto de la incorporación de test rápidos en el proceso de diagnóstico y tratamiento oportuno de la sífilis en gestantes. El modelo contrasta un escenario basal (pruebas convencionales) con un escenario alternativo (test rápidos) para estimar las variaciones en los resultados de salud adversos en el producto de la gestación, así como en los años de vida ajustados por discapacidad y los costos en salud asociados.",
          iconType = "vial",
          iconColor = "#2C5F8B",
          linkTo = "sifilis"
        )
        
        # menuBox(
        #   title = "Brechas de género",
        #   text = "Compare la evolución histórica de las brechas de género para los principales indicadores
        #   de mortalidad.",
        #   iconType = "chart-line",
        #   iconColor = "#737B4F",
        #   linkTo = "gaps"
        # ),
        
        
        
      ),
      data.step = 2,
      data.intro = "En cada una de estas tarjetas se pueden ver los títulos de las intervenciones y un breve resúmen del  modelo empleado. Para comenzar a utilizar los modelos, haga click en la tarjeta correspondiente."
      
    )
      
    
  ),
  
  # FOOTER INSTITUCIONAL CIIPS
  getFooter(landing=T),
  
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