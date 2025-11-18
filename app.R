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

source("modules/toggle_advanced_inputs.R")
source("models/estimaTool/UI/UI_hearts.R")
source("models/estimaTool/estimaTool.R")
source("functions/graf_esc.R")

source("visualization functions/indicatorsList.R")
source("visualization functions/menuBox.R")
source("visualization functions/plotBox.R")
source("visualization functions/comparisonChart.R")
source("visualization functions/getPalette.R")
source("visualization functions/gapsChart.R")
source("visualization functions/ratesRatio.R")
source("visualization functions/ratesDifferences.R")
source("visualization functions/rmsCalculate.R")
source("visualization functions/modalText.R")

source("functions/getStyle.R")

source("pages/landing_page.R")
source("pages/chart_page.R")

load("labels/labels_provincia.rda")
load("labels/labels_sexo.rda")
load("labels/labels_region.rda")

# Definir las páginas

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
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
    
    "body {font-family: 'Roboto', sans-serif !important;}
    
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
  
  /* Ícono fijo abajo a la derecha */
      .fixed-icon {
        position: fixed;
        bottom: 30px;
        right: 30px;
        width: 60px;
        height: 60px;
        background: rgba(70, 130, 180, 0.7);
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.3);
        z-index: 1000;
      }
      
      .fixed-icon:hover {
        background: rgba(70, 130, 180, 0.9);
      }
      
      .fixed-icon i {
        color: white;
        font-size: 28px;
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
  
  # Contenido principal sin margen adicional para páginas completas
  router_ui(
    route("/", landing_page),
    route("chart", chart_page)
  )
)

server <- function(input, output, session) {
  router_server()
  
  hearts_map_inputs = reactiveVal()
  
  # mostrar parámetros avanzados
  toggle_advanced_inputs(input, output, session)
  
  output$modalContent = renderUI({
    current_page = get_page()
    modalText(current_page)$content
    
  })
  
  output$modalTitle = renderUI({
    current_page = get_page()
    modalText(current_page)$title
    
  })
  
  outputOptions(output, "modalContent", suspendWhenHidden = FALSE)
  outputOptions(output, "modalTitle", suspendWhenHidden = FALSE)
  
  
  
  observeEvent(input$geo, {
    if (input$geo == "Jurisdicciones") {
      updateSelectInput(session,"area",choices = labels_provincia, selected = "01")
    } else {
      updateSelectInput(session,"area",choices = labels_region, selected = labels_region[1])
    }
  })
  
  
  # datosFiltrados = reactive({
  #   
  #   causaSeleccionada = input$causas
  #   
  #   if (input$geo == "Jurisdicciones") {
  #     dataset = dataMortProv
  #     area = "provincia"
  #   } else {
  #     area = "region"
  #     dataset = dataMortReg
  #   }
  #   
  #   if (length(causaSeleccionada)>0){
  #     data = dataset %>% dplyr::filter(causa == causaSeleccionada)
  #   } else {data=data.frame()}
  #   
  #   data
  #   
  # })
  
  output$comparisonChart = renderApexchart({
    comparisonChart()
  })
  
  output$map = renderLeaflet({
    leaflet() %>% addTiles() %>% 
      setView(lng = 144, lat = -37, zoom = 09)
  })
  
  
  output$grafico = renderUI({
    
    if (get_page(session = shiny::getDefaultReactiveDomain()) == "chart") {
      comparisonChart(
        input,
        output,
        datosFiltrados,
        indicatorsList,
        labels_provincia,
        labels_region,
        labels_sexo, 
        diferenciaTasas)
      
    }
  })
  
  output$gaps = renderUI({
    if (length(input$causas)>0) {
      gapsChart(
        input,
        output,
        session,
        datosFiltrados,
        labels_provincia,
        labels_region,
        labels_sexo, 
        ratesRatio, 
        ratesDifferences,
        firstTimeGaps
      )
    } else {
      HTML("<strong>Advertencia: </strong><br>Debe seleccionar al menos un grupo de causas de muerte")
    }
    
  })
  
  output$mapa = renderLeaflet({
    
    if (length(input$causas)>0) {
      trienio = input$trienio
      causa = input$causas
      map = rmsCalculate(input,trienio, causa, "cuartiles")
      map 
    } else {
      return()
    }
    })
  
  
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
        `TARGET_Tratamiento entre los diagnosticados (%)` = input$hearts_input_1/100,
        `BASELINE_Control de la hipertensión entre los tratados (%)` = input$hearts_input_6/100,
        `TARGET_Control de la hipertensión entre los tratados (%)` = input$hearts_input_6/100,
        `Costo farmacológico anual por paciente promedio (**)` = input$hearts_input_7,
        `Evento de enfermedad cardiaca isquemica promedio  (***)` = input$hearts_input_9,
        `Costo anual de consulta médica en paciente promedio (*)` = input$hearts_input_8
      )
    }
  })
    
  output$inputs_hearts = renderUI({
    load("models/estimaTool/base_line.RData")
    load("models/estimaTool/targets_default.RData")
    load("models/estimaTool/costs.RData")
    load("models/estimaTool/population.RData")
    ui_hearts(input, base_line, targets_default, costs, population, hearts_map_inputs)
  })
  
  observeEvent(input$hearts_go, {
    
    show("resultados_hearts")
    
    output$resultados_hearts = renderUI({
       ui_resultados_hearts(input,output,run_hearts)
    })
    lapply(c("inputContainer",hearts_map_inputs()$i_names), function (i) {
      disable(i)
      
    })
  })
  
  onclick("new_scenario_btn", {
    
    hide("resultados_hearts")
    lapply(c("inputContainer",hearts_map_inputs()$i_names), function (i) {
      enable(i)
      
    })
  })
}

shinyApp(ui, server)
