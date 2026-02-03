# Cargar las librerías necesarias
library(shiny)
library(shiny.fluent)
library(shiny.router)
library(leaflet)
library(dplyr)
library(apexcharter)
library(shinyjs)
library(shinytreeview)
library(glue)
library(shinyalert)
library(apexcharter)
library(highcharter)
library(paletteer)
library(openxlsx)
library(plotly)
library(reactable)
library(shinyWidgets)
library(shinycssloaders)
library(rintrojs)


source("modules/toggle_advanced_inputs.R")

source("models/estimaTool/UI/UI_hearts.R")
source("models/estimaTool/estimaTool.R")
source("models/hpv/UI/UI_hpv.R", encoding = "UTF-8")
source("models/hpv/getPrime.R", encoding = "UTF-8")
source("models/tbc/UI/UI_tbc.R", encoding = "UTF-8")
source("models/tbc/funcion.R", encoding = "UTF-8")
source("models/hepC/UI/UI_hepC.R", encoding = "UTF-8")
source("models/hepC/funcion_hepC.R", encoding = "UTF-8")

source("models/naat/UI/UI_naat.R", encoding = "UTF-8")
source("models/naat/TbcNaatModel.R", encoding = "UTF-8")

source("models/hpp/UI/UI_hpp.R")
source("models/hpp/funciones/funciones.R")

source("models/prep/UI/UI_prep.R")
source("models/prep/fn_prep4.R")

source("models/sifilis/UI/UI_sifilis.R")
source("models/sifilis/SifilisModel.R")

source("multiComp/UI/UI_multiComp.R")

source("comparisson/UI/UI_comparisson.R")

source("functions/graf_esc.R")

source("visualization functions/getHeader.R")
source("visualization functions/getFooter.R")
source("visualization functions/getHelp.R")
source("visualization functions/menuBox.R")
source("visualization functions/btnSequence.R")
source("visualization functions/getCountryCode.R")
source("visualization functions/tempHideInputs.R")
source("visualization functions/getStartModal.R")
source("visualization functions/getHelpModalText.R")
source("visualization functions/dragDropInput.R")
source("visualization functions/infoBox.R")
source("visualization functions/infoBoxCarousel.R")
source("visualization functions/getModelName.R")
source("visualization functions/functionJSToggle.R")

source("functions/getStyle.R")

source("pages/landing_page.R")
source("pages/hearts_page.R")
source("pages/hpv_page.R")
source("pages/tbc_page.R")
source("pages/hepC_page.R")
source("pages/hpp_page.R")
source("pages/prep_page.R")
source("pages/sifilis_page.R")
source("pages/naat_page.R")
source("pages/comparisson_page.R")
source("pages/multiComp_page.R")

firstTime = T

saved_scenarios <- reactiveVal(list())
model_comp = reactiveVal()
multiCompFirstTime = reactiveVal(T)

# Definir las páginas

ui <- fluidPage(
  introjsUI(),
  
  shinyjs::useShinyjs(),
  
  functionJSToggle("hearts"),
  functionJSToggle("hpv"),
  functionJSToggle("tbc"),
  functionJSToggle("hepC"),
  functionJSToggle("hpp"),
  functionJSToggle("prep"),
  functionJSToggle("sifilis"),
  functionJSToggle("naat"),
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap",
      rel = "stylesheet"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  ),
  tags$style(
    
    "
    
     @font-face {
        font-family: 'Frutiger';
        src: url('fonts/Frutiger 55 Roman.otf') format('opentype');
        font-weight: normal;
        font-style: normal;
      }
      
      body {
        font-family: 'Frutiger', Arial, sans-serif !important;
      }
    
    
    @keyframes pulse-left {
        0% { transform: scale(1); box-shadow: 0 0 0 0 rgba(229, 115, 115, 0.7); }
        50% { transform: scale(1.1); box-shadow: 0 0 0 15px rgba(229, 115, 115, 0); }
        100% { transform: scale(1); box-shadow: 0 0 0 0 rgba(229, 115, 115, 0); }
      }
      
      .pulse-highlight-left { animation: pulse-left 1s ease-out; }

    /* Botón Especial Izquierda (Independiente) */
    #multiCompBtn.left-red-btn {
  position: fixed;
  bottom: 30px;
  left: 30px;
  z-index: 2500;
  
  /* Gradiente basado en #00205C */
  background: linear-gradient(135deg, #003d8f 0%, #00205C 100%);
  color: white !important;
  
  /* Identidad visual igual a los de la derecha */
  border: none;
  border-radius: 50%; /* Circular como los de la derecha */
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

#multiCompBtn.left-red-btn {
  position: fixed;
  bottom: 30px;
  left: 30px;
  z-index: 2500;
  
  /* Gradiente basado en #00205C */
  background: linear-gradient(135deg, #003d8f 0%, #00205C 100%);
  color: white !important;
  
  /* Identidad visual igual a los de la derecha */
  border: none;
  border-radius: 50%; /* Circular como los de la derecha */
  width: 50px;
  height: 50px;
  font-size: 1.2em;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
  transition: all 0.3s ease;
  outline: none !important; /* Elimina el outline al hacer click */
}

#multiCompBtn.left-red-btn:hover {
  /* Hover ligeramente más claro */
  background: linear-gradient(135deg, #0051c4 0%, #003d8f 100%);
  transform: scale(1.05);
  box-shadow: 0 6px 15px rgba(0, 0, 0, 0.4);
}

#multiCompBtn.left-red-btn:focus {
  outline: none !important; /* Elimina el outline al enfocar */
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3) !important;
}

#multiCompBtn.left-red-btn:active {
  outline: none !important; /* Elimina el outline al hacer click */
  transform: scale(0.98); /* Efecto de presión */
}

#multiCompBtn.left-red-btn .fa {
  margin: 0 !important;
}

/* Estilos cuando el botón está deshabilitado */
#multiCompBtn.left-red-btn:disabled,
.left-side-button:disabled {
  opacity: 0.4 !important;
  cursor: not-allowed !important;
  pointer-events: auto !important; /* Permite mostrar el cursor */
  background: linear-gradient(135deg, #003d8f 0%, #00205C 100%) !important;
  outline: none !important;
}

#multiCompBtn.left-red-btn:disabled:hover,
.left-side-button:disabled:hover {
  transform: none !important;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3) !important;
  background: linear-gradient(135deg, #003d8f 0%, #00205C 100%) !important;
}      
    .header-destacados {
    font-size: 1.25rem;
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1.25rem;
    margin-top: 1rem;
    background-color: #FF671B;
    color: white;
    text-align: center;
  }
  
  .header-destacados-title {
    display: inline-block;
    margin: 2rem auto;
    flex-grow: 1;
    font-weight: bold;
  }
    .comp-box {
      padding: 30px;
      border-radius: 10px;
      backdrop-filter: blur(10px);
      transition: transform 0.3s ease, box-shadow 0.3s ease;
      cursor: pointer;
      display: flex;
      flex-direction: column;
      height: 100%;
      box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
      }
    
          
          
    .modal-dialog { 
        width: 80% !important; 
        max-width: 80% !important; 
        margin-top: 100px !important; 
      }
      
      
      .modal { 
        z-index: 99999 !important; 
      }
      .modal-backdrop { 
        z-index: 99998 !important; 
      }
      
      .modal-footer{ display:none}
    
    
    .shiny-input-number {
    text-align: center;
    }
    .tlist-group {
    font-size: 0.7em !important;}
    
    /* Animación para el h3 */
    @keyframes slideInLeft {
      from {
        transform: translateX(-100px);
        opacity: 0;
      }
    to {
      transform: translateX(0);
      opacity: 1;
    }
  }
  
  
  .animate-left {
    animation: slideInLeft 1s ease-out;
  }
  
  
  
  /* --- HEADER --- */
.fixed-header {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  height: 80px;
  background: rgba(255, 255, 255, 0.95);
  backdrop-filter: blur(10px);
  border-bottom: 1px solid rgba(16, 51, 98, 0.1);
  z-index: 999999999 !important;  /* siempre arriba */
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 10px 20px;
  box-sizing: border-box;
  pointer-events: auto !important;
  gap: 15px;
}

/* Línea vertical separadora */
.header-divider {
  width: 1px;
  height: 50px; /* Ajusta según la altura del logo */
  background-color: #9d9d9d;
  margin: 0 5 px;
}

.header-title-container {
  display: flex;
  align-items: center;
  margin-left: 5px !important;
  gap: 15px;
  flex: 1;
}

.header-title {
  color: #236192 !important;
  margin: 0 !important;
  text-align: left !important;
  font-size: 28px  !important; /* Ajusta según necesites */
}

.home-icon {
  font-size: 20px;
  color: #236192;
  transition: color 0.3s;
}

.home-icon:hover {
  color: #1a4a6f;
}

.p-2 {
  margin-left: auto;
}
  
  
  


/* --- INTROJS FIX PARA NO TAPAR EL HEADER --- */

/* El overlay oscuro debe permitir clicks */
.introjs-overlay {
  z-index: 999999990 !important;
  pointer-events: auto !important;  /* Asegura que pueda recibir clicks */
  cursor: pointer !important;
}

/* Asegura que el header no interfiera con los clicks del overlay */
.fixed-header {
  pointer-events: auto !important;
}

/* La capa que resalta el elemento tampoco debe tapar al header */
.introjs-helperLayer {
  z-index: 999999991 !important;
}

/* La referencia del tooltip tampoco */
.introjs-tooltipReferenceLayer {
  z-index: 999999992 !important;
}

/* El elemento enfocado */
.introjs-showElement {
  z-index: 999999993 !important;
}

/* El tooltip SÍ debe aparecer arriba del header */
.introjs-tooltip {
  z-index: 999999999 !important;
  
}



      /* Modal elegante */
.elegant-modal {
  display: none;
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  background: white;
  padding: 40px;
  border-radius: 15px;
  box-shadow: 0 10px 40px rgba(0, 0, 0, 0.3);
  max-width: 600px;
  width: 90%;
  z-index: 1001;
  /* --- Propiedades agregadas para el scroll --- */
  max-height: 90vh; /* Establece una altura máxima, por ejemplo, el 90% del alto de la ventana (viewport) */
  overflow-y: auto; /* Agrega la barra de desplazamiento vertical si el contenido supera el max-height */
  /* ------------------------------------------- */
}

  .modal-overlay {
    display: none;
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0, 0, 0, 0.5);
    z-index: 1000;
  }
  
  .modal-header {
    font-size: 28px;
    font-weight: 600;
    color: #2c3e50;
    margin-bottom: 20px;
    border-bottom: 2px solid #4682b4;
    padding-bottom: 15px;
  }
  
  .modal-content-text {
    font-size: 16px;
    line-height: 1.8;
    color: #555;
    text-align: justify;
  }
  
  .close-btn {
    position: absolute;
    top: 15px;
    right: 20px;
    font-size: 30px;
    color: #999;
    cursor: pointer;
  }
  
  .close-btn:hover {
    color: #333;
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
      gap: 10px;
      z-index: 1000;
      transition: all 0.4s cubic-bezier(0.68, -0.55, 0.265, 1.55);
    }
    
    .floating-buttons-container.collapsed {
      gap: 0;
    }
    
    .floating-btn {
      background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
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
      transition: all 0.4s cubic-bezier(0.68, -0.55, 0.265, 1.55);
      opacity: 1;
      transform: scale(1) translateY(0);
      pointer-events: auto;
    }
    
    .floating-btn:hover {
      background: linear-gradient(135deg, #1e4368 0%, #3a7a8a 100%);
      transform: scale(1.05) translateY(0);
      box-shadow: 0 6px 15px rgba(0, 0, 0, 0.4);
    }
    
    /* Botones colapsados */
    .floating-buttons-container.collapsed .floating-btn:not(.toggle-btn) {
      opacity: 0;
      transform: scale(0.3) translateY(20px);
      pointer-events: none;
      margin: -25px 0;
    }
    
    /* Botón toggle siempre visible */
    .toggle-btn {
      background: #EC7016 !important;
      position: relative;
      z-index: 1001;
    }
    
    .toggle-btn:hover {
      background: #D46212 !important;
    }
    
    /* Animación de rotación del icono */
    .toggle-btn i {
      transition: transform 0.4s cubic-bezier(0.68, -0.55, 0.265, 1.55);
    }
    
    .floating-buttons-container.collapsed .toggle-btn i {
      transform: rotate(180deg);
    }
    
    .floating-btn .fa {
      margin: 0 !important;
    }
    
    
    .go-button {
  background-color: #EC7016 !important;
  color: white !important;
  border: none !important;
  border-radius: 8px !important;
  padding: 10px 20px !important;
  font-weight: 500 !important;
  transition: all 0.3s ease !important;
}

.go-button:hover {
  background-color: #d4631a !important;
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(236, 112, 22, 0.3) !important;
}

.go-button:active {
  transform: translateY(0);
  box-shadow: 0 2px 4px rgba(236, 112, 22, 0.3) !important;
}

.go-button .fa-play {
  margin-right: 8px;
}
    
    "
  
  ),
  
  title = "Pia Tool 2.0",
  
  # Overlay del modal
  div(class = "modal-overlay", id = "modalOverlay"),
  
  # Modal elegante
  div(class = "elegant-modal", id = "elegantModal",
      span(class = "close-btn", id = "closeBtn", "×"),
      div(class = "modal-header", uiOutput("modalTitle")),
      div(class = "modal-content-text",
          uiOutput("modalContent")
      )
  ),
  
  
  # JavaScript para manejar el modal
  tags$script(HTML("
    $(document).ready(function() {
      $('#fixedIcon').on('click', function() {
        $('#elegantModal').show();
        $('#modalOverlay').show();
      });
      
      $('#closeBtn, #modalOverlay').on('click', function() {
        $('#elegantModal').hide();
        $('#modalOverlay').hide();
      });
    });
  ")),
  
  div(class = "elegant-modal", id = "saveScenarioModal",
      span(class = "close-btn", id = "closeSaveBtn", "×"), # Botón de cerrar
      div(class = "modal-header", "💾 Guardar Escenario"),
      div(class = "modal-content-text",
          # Input de texto para el nombre del escenario
          shiny::textInput("scenario_name", label = "Nombre del Escenario:", value = ""),
          # Botón para confirmar el guardado
          actionButton("confirm_save", "Guardar", class = "btn-primary")
      )
  ),
  
  # Contenido principal sin margen adicional para páginas completas
  router_ui(
    route("/", landing_page),
    route("hearts", hearts_page),
    route("hpv", hpv_page),
    route("tbc", tbc_page),
    route("hepC", hepC_page),
    route("hpp", hpp_page),
    route("prep", prep_page),
    route("sifilis", sifilis_page),
    route("naat", naat_page),
    route("comparisson", comparisson_page),
    route("multiComp", multiComp_page)
    
  )
)

server <- function(input, output, session) {
  
  
  
  router_server()
  
  observeEvent(TRUE, {
    if (firstTime) {
      showModal(modalDialog(
        title = NULL,
        # Usamos HTML() para que reconozca la etiqueta <strong>
        HTML(getStartModal()), 
        footer = modalButton("Ingresar"),
        easyClose = TRUE
      ))
    }
    firstTime <<- F
  }) # Se ejecuta solo una vez
  
  onclick("boton_ingresar", {
    removeModal()
  })
  
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  observeEvent(input$help, {
    pagina_actual <- isolate(get_page())
    getHelp(pagina_actual, session)
  })
  

  
  
  
  hearts_map_inputs = reactiveVal()
  hpv_map_inputs = reactiveVal()
  tbc_map_inputs = reactiveVal()
  hepC_map_inputs = reactiveVal()
  hpp_map_inputs = reactiveVal()
  prep_map_inputs = reactiveVal()
  sifilis_map_inputs = reactiveVal()
  naat_map_inputs = reactiveVal()
  
  hearts_map_outputs = reactiveVal()
  hpv_map_outputs = reactiveVal()
  tbc_map_outputs = reactiveVal()
  hepC_map_outputs = reactiveVal()
  hpp_map_outputs = reactiveVal()
  prep_map_outputs = reactiveVal()
  sifilis_map_outputs = reactiveVal()
  naat_map_outputs = reactiveVal()
  
  
  back_btn_clicked_comp = reactiveVal(F)
  # mostrar parámetros avanzados
  #toggle_advanced_inputs(input, output, session)
  
  
  ##### HEARTS #####
  
  run_hearts <- reactive({
    if (is.null(input$hearts_input_1)==F) {
      estimaToolCosts(
        country = input$country,
        Population = input$hearts_input_2,
        `BASELINE_Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` = input$hearts_input_3/100,
        `TARGET_Prevalencia de hipertensión entre adultos de 30-79 años, estandarizada por edad` = input$hearts_input_3/100,
        `BASELINE_Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` = input$hearts_input_4/100,
        `TARGET_Prevalencia de diagnóstico previo de hipertensión entre adultos de 30-79 años con hipertensión, estandarizada por edad` = input$hearts_input_4/100,
        `BASELINE_Tratamiento entre los diagnosticados (%)` = input$hearts_input_5/100,
        `TARGET_Tratamiento entre los diagnosticados (%)` = input$hearts_input_1,
        `BASELINE_Control de la hipertensión entre los tratados (%)` = input$hearts_input_6/100,
        `TARGET_Control de la hipertensión entre los tratados (%)` = input$hearts_input_6/100,
        `Costo farmacológico anual por paciente promedio (**)` = input$hearts_input_7,
        `Evento de enfermedad cardiaca isquemica promedio  (***)` = input$hearts_input_9,
        `Costo anual de consulta médica en paciente promedio (*)` = input$hearts_input_8
      )
    }
  })
    
  ##### outputs hearts #####
  
  output$inputs_hearts = renderUI({
    ui_hearts(input, base_line, targets_default, costs, population, hearts_map_inputs)
  })
  
  observeEvent(input$hearts_go, {
    
    toggle("recuadro_resultados_hearts")
    toggle("resultados_hearts")
    output$resultados_hearts = renderUI({
       ui_resultados_hearts(input,output,run_hearts, hearts_map_outputs)
    })
    
    
    tempHideInputs("hearts", input, hearts_map_inputs())
      
  })
  
  ##### HPV #####
  
  run_hpv  <-  reactive({
    
    getPrime(
      input = input,
      country= input$country,
      coverageBase = input$coverageBase,
      #input$birthCohortSizeFemale,
      cohortSizeAtVaccinationAgeFemale = input$cohortSizeAtVaccinationAgeFemale,
      coverageAllDosis = input$coverageAllDosis,
      vaccineEfficacyVsHPV16_18 = input$vaccineEfficacyVsHPV16_18,
      targetAgeGroup = input$targetAgeGroup,
      vaccinePricePerFIG = input$vaccinePricePerFIG,
      vaccineDeliveryCostPerFIG = input$vaccineDeliveryCostPerFIG,
      totalVaccineCostPerFIG = input$totalVaccineCostPerFIG,
      cancerTreatmentCostPerEpisodeOverLifetime = input$cancerTreatmentCostPerEpisodeOverLifetime,
      DALYsForCancerDiagnosis = input$DALYsForCancerDiagnosis,
      DALYsForNonTerminalCancerSequelaePperYear = input$DALYsForNonTerminalCancerSequelaePperYear,
      DALYsForTerminalCancer = input$DALYsForTerminalCancer,
      discountRate = input$discountRate,
      proportionOfCervicalCancerCasesThatAreDueToHPV16_18 = input$proportionOfCervicalCancerCasesThatAreDueToHPV16_18,
      #input$GDPPerCapita,
      costoProg = input$costoProg,
      mortall = mortall,
      mortcecx = mortcecx,
      incidence = incidence,
      #dalys,
      parameters = parameters
    )
  })
  
  ##### outputs hpv #####
  
  output$inputs_hpv = renderUI({
    output$uiOutput_basica <- ui_hpv_basica(input,inputs_hpv(), run_hearts(), hpv_map_inputs)
  })
  
  outputOptions(output, "inputs_hpv", suspendWhenHidden = T)
  
  observeEvent(input$targetAgeGroup, {
    updateNumericInput(session, "cohortSizeAtVaccinationAgeFemale", value = 
                         cohortSizeAcVac[cohortSizeAcVac$age == input$targetAgeGroup & cohortSizeAcVac$country == input$country,]$value)
    
  })
  
  observeEvent(input$hpv_go, {
    toggle("recuadro_resultados_hpv")
    toggle("resultados_hpv")
    
    output$resultados_hpv = renderUI({
      tagList(
        #ui_grafico_nuevo_hpv(run_hpv(), input, output),
        ui_grafico_hpv(run_hpv(), input),
        ui_tabla_hpv(run_hpv(), input, hpv_map_outputs)
      )
      
    })
    
    tempHideInputs("hpv", input, hpv_map_inputs())
  })
  
  ##### TBC #####
  
  tbc_run <- reactive({
    
    if (length(input$VOTrrExito)!=0) {
      table = modelo_tbc(input$country,
                         input$VOTrrExito,
                         input$VOTadherencia/100,
                         input$costo_evento_VOT,
                         input$cantidad_vot_semana,
                         input$ttoExitoso_Duracion,
                         input$pExitoso/100,
                         input$pFalla/100,
                         input$pMuerte/100,
                         input$VOTrrFalla,
                         input$VOTrrMuerte,
                         input$DOTrrExito,
                         input$DOTrrFalla,
                         input$DOTrrMuerte,
                         input$DOTadherencia/100,
                         input$cantidad_dot_semana,
                         input$mediana_edad_paciente,
                         input$cohorte,
                         input$utilidad_pob_gral,
                         input$disutilidad_tbc_activa,
                         input$prob_internacion_con_falla/100,
                         input$cantidadDiasInternacion,
                         input$costo_trat_induccion,
                         input$costo_trat_consolidacion,
                         input$costo_seguimiento,
                         input$costo_examenes_complemen,
                         input$costo_evento_DOT,
                         input$costo_internacion,
                         input$costoConsulta,
                         input$costo_trat_multires_induccion,
                         input$costo_trat_multires_consolidacion,
                         input$tasa_descuento_anual/100,
                         input$costo_intervencion_vDOT)
      table
    }
    
  })
  
  ##### outputs tbc #####
  
  output$inputs_tbc = renderUI({
    ui_tbc(input, tbc_map_inputs)
  })
  
  observeEvent(input$tbc_go, {
    toggle("recuadro_resultados_tbc")
    toggle("resultados_tbc")
    output$resultados_tbc = renderUI({
      ui_resultados_tbc(input,output,tbc_run, tbc_map_outputs)
    })
    
    
    tempHideInputs("tbc", input, tbc_map_inputs())
  })
  
  ##### HEPATITIS C #####
  
  hepC_run = reactive({
    if (length(input$cohorte) > 0) {
      hepC = hepC_full(
        input,
        output,
        input_pais = str_to_title(input$country),
        input_cohorte = input$cohorte,
        input_AtasaDescuento = 0.03,
        input_F0 = input$F0/100,
        input_F1 = input$F1/100,
        input_F2 = input$F2/100,
        input_F3 = input$F3/100,
        input_F4 = input$F4/100,
        input_aCostoF0F2 = input$aCostoF0F2,
        input_aCostoF3 = input$aCostoF3,
        input_aCostoF4 = input$aCostoF4,
        input_aCostoDC = input$aCostoDC,
        input_aCostoHCC = input$aCostoHCC,
        input_pSVR = input$pSVR/100,
        input_tDuracion_Meses = input$tDuracion_Meses,
        input_pAbandono = input$pAbandono/100,
        input_Costo_Tratamiento = input$Costo_Tratamiento,
        input_Costo_Evaluacion = input$Costo_Evaluacion
      )
      hepC_indicators = names(hepC$Comparacion)
      hepC_values = unlist(hepC$Comparacion)
      
      hepCTable = data.frame(
        hepC_indicators,
        hepC_values
      )
      
      rownames(hepCTable) = NULL
      colnames(hepCTable) = c("Indicador", "Valor")
      hepCTable
    }
    
    
  })
  
  ##### outputs hep c #####
  
  output$inputs_hepC = renderUI({
    ui_hepC(input, datosPais, hepC_map_inputs)
  })
  
  observeEvent(input$hepC_go, {
    toggle("recuadro_resultados_hepC")
    toggle("resultados_hepC")
    output$resultados_hepC = renderUI({
      tagList(
        ui_resultados_hepC(input,output,hepC_run, hepC_map_outputs)
      )
    })
    
    
    tempHideInputs("hepC", input, hepC_map_inputs())
  })
  
  ##### HPP #####
  
  hpp_run = reactive({
    
    if (length(input$hpp_uso_oxitocina_base)>0) {
      
      resultados = resultados_comparados(
        pais = str_to_title(input$country),
        usoOxitocina_base = input$hpp_uso_oxitocina_base/100,
        usoOxitocina_target = input$hpp_uso_oxitocina_taget/100,
        partos_anuales = input$hpp_partos_anuales,
        edad_al_parto = input$hpp_edad_parto,
        partos_institucionales = input$hpp_partos_institucionales/100,
        eficacia_Intervencion = 0.30230,
        mortalidad_materna = input$hpp_mortalidad_materna,
        mortalidad_hpp = input$hpp_mortalidad_hpp/100,
        pHPP = input$hpp_pHPP/100,
        pHPP_Severa = input$hpp_pHPP_Severa/100,
        pHisterectomia = input$hpp_pHisterectomia/100,
        eficaciaOxitocina = input$hpp_eficaciaOxitocina/100,  
        uHisterectomia = input$hpp_uHisterectomia,
        costo_oxitocina = input$hpp_costo_oxitocina,
        descuento = input$hpp_tasa_descuento/100, #Tasa de descuento (INPUT)
        costoIntervencion = input$hpp_costo_programatico #Costo de la intervención  (INPUT)
      )
    }
    
  })
  
  ##### outputs hpp #####
  
  output$inputs_hpp = renderUI({
    ui_hpp(input, hpp_map_inputs)
  })
  
  observeEvent(input$hpp_go, {
    toggle("recuadro_resultados_hpp")
    toggle("resultados_hpp")
    output$resultados_hpp = renderUI({
      tagList(
        ui_resultados_hpp(input, output, hpp_run, hpp_map_outputs)
      )
    })
    
    
    tempHideInputs("hpp", input, hpp_map_inputs())
    
  })
  
  ##### PREP #####
  
  prep_run = reactive({
    if (length(input$duracionPrEP)>0) {
      corrida = NA
      for (i in names(get_prep_params(input$country))) {
        corrida = c(corrida, input[[i]])
      }
      corrida = corrida[2:length(corrida)]
      names(corrida) = names(get_prep_params(input$country))
      
      resultados = funcionCalculos(corrida,toupper(input$country))
      resultados
    }
  })
  
  ##### outputs prep #####
  
  output$inputs_prep = renderUI({
    ui_prep(input, prep_map_inputs)
  })
  
  observeEvent(input$prep_go, {
    toggle("recuadro_resultados_prep")
    toggle("resultados_prep")
    output$resultados_prep = renderUI({
      tagList(
        ui_resultados_prep(input, output, prep_run, prep_map_outputs)
      )
    })


    tempHideInputs("prep", input, prep_map_inputs())
  })
  
  ##### SIFILIS #####
  
  sifilis_run = reactive({
    if (is.null(input$country) == F) {
      
      
      
      params = cargar(input$country)
      inputsCountry = sifilisInputList()
      
      paramsRunSifilis <- lapply(inputsCountry$var, function(i) {
        if (inputsCountry$tipo[inputsCountry$var == i] %in% c("Avanzado", "Basico")) {
          input[[i]]
        } else {
          params[[i]]
        }
      })
      
      names(paramsRunSifilis) = inputsCountry$var
      correrModelo(paramsRunSifilis)
      
    }
  })

  
  ##### outputs sifilis #####
  
  output$inputs_sifilis = renderUI({
    UI_sifilis(input, sifilis_map_inputs)
  })
  
  observeEvent(input$sifilis_go, {
    toggle("recuadro_resultados_sifilis") 
  toggle("resultados_sifilis")
   output$resultados_sifilis = renderUI({
     tagList(
       ui_resultados_sifilis(input, output, sifilis_run(), sifilis_map_outputs)
     )
   })
   tempHideInputs("sifilis", input, sifilis_map_inputs())
  })
  
  
  ##### NAAT #####
  
  naat_run = reactive({
    
    if (is.null(input$country) == F) {
      params = cargar_naat()[[input$country]]
      inputsCountry = naatInputList()
      inputsCountry$var = paste0(inputsCountry$var,"_naat")
      
      paramsRunNaat <- lapply(inputsCountry$var, function(i) {
        if (inputsCountry$tipo[inputsCountry$var == i] %in% c("Avanzado", "Basico")) {
          input[[i]]
        } else {
          params[[substring(i,1,nchar(i) - 5)]]
        }
      })
      inputsCountry$var =substring(inputsCountry$var, 1, nchar(inputsCountry$var) - 5)
      names(paramsRunNaat) = inputsCountry$var
      
      correrModelo_naat(paramsRunNaat)
      
    }
  })
  
  
  output$inputs_naat = renderUI({
    UI_naat(input, naat_map_inputs)
  })
  
  observeEvent(input$naat_go, {
    toggle("recuadro_resultados_naat")
    toggle("resultados_naat")
    
    output$resultados_naat = renderUI({
      tagList(
        ui_resultados_naat(input, output, naat_run(), naat_map_outputs)
      )
    })
    tempHideInputs("naat", input, naat_map_inputs())
  })
  
  
  
  ##### MULTICOMP #####
  
  ##### outputs multicomp #####
  
  output$inputs_multicomp = renderUI({
    escenarios = isolate(saved_scenarios())
    ui_inputs_multiComp(input, escenarios, get_page(), getCountryCode)
  })
  
  output$resultados_multiComp = renderUI({
    tagList(
      ui_resultados_multiComp(input,output,session,current_page(), saved_scenarios(), input$selectScenariosMulti)
    )
    
    
  })


  


  ##### ONCLICK #####
  interventions = c("hearts","hpv","hepC","sifilis","hpp", "prep","tbc", "naat")
  
  lapply(c(interventions, "multiComp"), function(i) {
    observeEvent(input[[glue("help_{i}")]], {
      showModal(modalDialog(
        title = NULL,
        HTML(getHelpModalText(i)), 
        easyClose = TRUE
      ))
    })
  })
  
  
  
  
  observeEvent(get_page(), {
    currentPage = isolate(get_page())
    
    btnSequence(
      currentPage, 
      input, 
      output, 
      session, 
      eval(parse(text=glue("{currentPage}_map_inputs"))), 
      eval(parse(text=glue("{currentPage}_map_outputs"))), 
      saved_scenarios,
      model_comp,
      getCountryCode,
      back_btn_clicked_comp)
    
  })
  
  # onclick("backComp", {
  #   backTo = model_comp()
  #   click(glue("menuBox_{backTo}"))
  #   
  # })
  
  output$back_btn_comp = renderUI({
    
    if (is.null(model_comp())) {return()}
    
    tagList(
      tags$a(
        id = "backComp",
        div(
          class = "floating-back-container",
          tags$button(
            id = "btn_back_hearts",
            class = "back-btn",
            title = "Volver",
            # JavaScript para cambiar de pestaña/página en Shiny
            icon("arrow-left")
          )
        ),
        href = route_link(model_comp())
      )
    )
  })
    
  onclick("backComp", {
    back_btn_clicked_comp(T)
  })
  
  
  
  observeEvent(saved_scenarios(), {
    print("pasa")
    if (length(saved_scenarios())>1 & length(unique(rownames(do.call(rbind,saved_scenarios()))))>1) {
      #enable("acceso-multiComp")
      
      enable(selector = ".left-side-button")
      
      if (length(saved_scenarios())==2 & multiCompFirstTime() == T) {
        showNotification(
          HTML("<p>Panel de comparación de intervenciones múltplies <strong>activado</strong><p>")
        )
      }
      
      addClass(id = "multiCompBtn", class = "pulse-highlight-left")
      delay(3000, removeClass(id = "multiCompBtn", class = "pulse-highlight-left"))
      
      multiCompFirstTime(F)
      
    } else {
      disable(selector = ".left-side-button")
      #disable("acceso-multiComp")
    }
    
    
      
      
  })
  
  ##### toggle buttons #####
  
  
  
}

shinyApp(ui, server)
