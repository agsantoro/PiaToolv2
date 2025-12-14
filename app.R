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

source("models/hpp/UI/UI_hpp.R")
source("models/hpp/funciones/funciones.R")

source("models/prep/UI/UI_prep.R")
source("models/prep/fn_prep4.R")

source("models/sifilis/UI/UI_sifilis.R")
source("models/sifilis/SifilisModel.R")

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
source("pages/hearts_page.R")
source("pages/hpv_page.R")
source("pages/tbc_page.R")
source("pages/hepC_page.R")
source("pages/hpp_page.R")
source("pages/prep_page.R")
source("pages/sifilis_page.R")

# Definir las páginas

ui <- fluidPage(
  introjsUI(),
  
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
    
    "
    
    
    body {font-family: 'Roboto', sans-serif !important;}
    
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
  padding: 0 30px;
  box-sizing: border-box;
  pointer-events: auto !important;
}

/* --- INTROJS FIX PARA NO TAPAR EL HEADER --- */

/* El overlay oscuro NO debe tapar al header */
.introjs-overlay {
  z-index: 999999990 !important;  
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
    route("hearts", hearts_page),
    route("hpv", hpv_page),
    route("tbc", tbc_page),
    route("hepC", hepC_page),
    route("hpp", hpp_page),
    route("prep", prep_page),
    route("sifilis", sifilis_page)
    
  )
)

server <- function(input, output, session) {
  router_server()
  
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Siguiente",
                                               "prevLabel"="Anterior"))
  )

  
  
  
  hearts_map_inputs = reactiveVal()
  hpv_map_inputs = reactiveVal()
  tbc_map_inputs = reactiveVal()
  hepC_map_inputs = reactiveVal()
  hpp_map_inputs = reactiveVal()
  prep_map_inputs = reactiveVal()
  sifilis_map_inputs = reactiveVal()
  
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
    
  ##### outputs hearts #####
  
  output$inputs_hearts = renderUI({
    ui_hearts(input, base_line, targets_default, costs, population, hearts_map_inputs)
  })
  
  observeEvent(input$hearts_go, {
    
    toggle("resultados_hearts")
    output$resultados_hearts = renderUI({
       ui_resultados_hearts(input,output,run_hearts)
    })
    
    
    lapply(c("inputContainer",hearts_map_inputs()$i_names), function (i) {
      disable(i)
      
    })
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
  
  onclick("hpv_go", {
    
    toggle("resultados_hpv")
    
    output$resultados_hpv = renderUI({
      tagList(
        #ui_grafico_nuevo_hpv(run_hpv(), input, output),
        ui_grafico_hpv(run_hpv(), input),
        ui_tabla_hpv(run_hpv(), input)
      )
      
    })
    
    lapply(c("inputContainer",hpv_map_inputs()$i_names), function (i) {
      disable(i)
      
    })
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
    toggle("resultados_tbc")
    output$resultados_tbc = renderUI({
      ui_resultados_tbc(input,output,tbc_run)
    })
    
    
    lapply(c("inputContainer",tbc_map_inputs()$i_names), function (i) {
      disable(i)
      
    })
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
    toggle("resultados_hepC")
    output$resultados_hepC = renderUI({
      tagList(
        ui_resultados_hepC(input,output,hepC_run)
      )
    })
    
    
    lapply(c("inputContainer",hepC_map_inputs()$i_names), function (i) {
      disable(i)
    })
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
    toggle("resultados_hpp")
    output$resultados_hpp = renderUI({
      tagList(
        ui_resultados_hpp(input, output, hpp_run)
      )
    })
    
    
    lapply(c("inputContainer",hpp_map_inputs()$i_names), function (i) {
      disable(i)
    })
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
    toggle("resultados_prep")
    output$resultados_prep = renderUI({
      tagList(
        ui_resultados_prep(input, output, prep_run)
      )
    })


    lapply(c("inputContainer",prep_map_inputs()$i_names), function (i) {
      disable(i)
    })
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

  
  ##### outputs prep #####
  
  output$inputs_sifilis = renderUI({
    UI_sifilis(input, sifilis_map_inputs)
  })
  
  observeEvent(input$sifilis_go, {
   
  toggle("resultados_sifilis")
   output$resultados_sifilis = renderUI({
     tagList(
       ui_resultados_sifilis(input, output, sifilis_run())
     )
   })
  
   lapply(c("inputContainer",sifilis_map_inputs()$i_names), function (i) {
     disable(i)
   })
  })


  
  
  
  ##### ONCLICK #####
  
  # hearts
  onclick("new_scenario_btn_hearts", {
    hide("resultados_hearts")
    lapply(c("inputContainer",hearts_map_inputs()$i_names), function (i) {
      enable(i)
    })
  })
  
  # hpv
  onclick("new_scenario_btn_hpv", {
    hide("resultados_hpv")
    lapply(c("inputContainer",hpv_map_inputs()$i_names), function (i) {
      enable(i)
    })
  })
  
  # tbc
  onclick("new_scenario_btn_tbc", {
    hide("resultados_tbc")
    lapply(c("inputContainer",tbc_map_inputs()$i_names), function (i) {
      enable(i)
    })
  })
  
  # hepC
  onclick("new_scenario_btn_hepC", {
    hide("resultados_hepC")
    lapply(c("inputContainer",hepC_map_inputs()$i_names), function (i) {
      enable(i)
    })
  })
  
  # hpp
  onclick("new_scenario_btn_hpp", {
    hide("resultados_hpp")
    lapply(c("inputContainer",hpp_map_inputs()$i_names), function (i) {
      enable(i)
    })
  })
  
  # prep
  onclick("new_scenario_btn_prep", {
    hide("resultados_prep")
    lapply(c("inputContainer",prep_map_inputs()$i_names), function (i) {
      enable(i)
    })
  })
  
  # sifilis
  onclick("new_scenario_btn_sifilis", {
    hide("resultados_sifilis")
    lapply(c("inputContainer",sifilis_map_inputs()$i_names), function (i) {
      enable(i)
    })
  })
}

shinyApp(ui, server)
