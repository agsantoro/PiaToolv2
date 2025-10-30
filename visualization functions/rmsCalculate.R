library(ggplot2)
library(stringr)
library(htmltools)
library(epitools)
library(sf)
library(spdep)
library(apexcharter)
library(dplyr)
library(tidyr)
library(leaflet)
library(highcharter)
library(scales)

rmsCalculate = function (input,trienio, causa, tipo_mapa) {
  ##### PREPARACION DE DATOS #####
  # cargar poblaciones por departamentos
  if (get_page() != "map" | is.null(input$causas)) {return()}
  withProgress(message = "Cargando mapa...", {
    incProgress(0.1)
    load("data/population/pobDeptosCenso.rda")
    
    # carga mapa de polígonos de departamentos
    
    load("data/maps/mapaSimple.rda")
    
    # carga dataset de tasas de mortalidad de población estandar
    incProgress(0.1)
    load("data/population/tasasEstandarizadas.rda")
    incProgress(0.1)
    load("data/indicators/NBIDEPTOS.rda")
    
    # crea lista para guardar resultados
    resultados = list()
    
    # carga lista de causas de mortalidad
    load("data/mortalityCauses.rda")
    causasMortalidad = mortalityCauses %>% pivot_longer(cols= 1:ncol(mortalityCauses),names_to = "LISTA") %>% dplyr::filter(is.na(value)==F) %>% distinct(LISTA, value)
    
    # identifica lista de la causa seleccionada
    lista = max(causasMortalidad$LISTA[causasMortalidad$value==causa] )
    
    # identifica el año del censo y los años de las defucniones según el trienio seleccionado
    if (trienio == 1) {
      anoCenso = "2001"
      anosDef = 2000:2002
      anosDef = substring(anosDef,3,4)
    } else if (trienio == 2) {
      anoCenso = "2010"
      anosDef = 2009:2011
      anosDef = substring(anosDef,3,4)
    } else if (trienio == 3) {
      anoCenso = "2022"
      anosDef = 2021:2023
      anosDef = substring(anosDef,3,4)
    }
    
    
    # formatea df de población:
    # - filtra mujeres
    # - agrupa edades
    
    pobDeptosCenso = pobDeptosCenso %>% dplyr::filter(SEXO == 2) %>% pivot_longer(cols = 5:(ncol(pobDeptosCenso)-1), names_to = "GRUPEDAD", values_to = "POBLACION")
    pobDeptosCenso$GRUPEDAD[pobDeptosCenso$GRUPEDAD %in% c("POB7579","POB8084","POB8589","POB9094","POB95MAS")] = "POB75MAS"
    
    pobDeptosCenso = pobDeptosCenso %>% mutate(GRUPEDAD_DEPTOS = case_when(
      GRUPEDAD == "POB0004" ~ "POB0004",
      GRUPEDAD == "POB0509" ~ "POB0514",
      GRUPEDAD == "POB1014" ~ "POB0514",
      GRUPEDAD == "POB1519" ~ "POB1524",
      GRUPEDAD == "POB2024" ~ "POB1524",
      GRUPEDAD == "POB2529" ~ "POB2534",
      GRUPEDAD == "POB3034" ~ "POB2534",
      GRUPEDAD == "POB3539" ~ "POB3544",
      GRUPEDAD == "POB4044" ~ "POB3544",
      GRUPEDAD == "POB4549" ~ "POB4554",
      GRUPEDAD == "POB5054" ~ "POB4554",
      GRUPEDAD == "POB5559" ~ "POB5564",
      GRUPEDAD == "POB6064" ~ "POB5564",
      GRUPEDAD == "POB6569" ~ "POB6574",
      GRUPEDAD == "POB7074" ~ "POB6574",
      GRUPEDAD == "POB75MAS" ~ "POB75MAS"
    )) %>% 
      dplyr::filter(is.na(GRUPEDAD_DEPTOS) == F) %>%
      group_by(ANO, CODIGODEPTO, SEXO, GRUPEDAD_DEPTOS) %>%
      summarise(POBLACION = sum(POBLACION))
    
    incProgress(0.1)
    # carga defunciones del trienio seleccionado
    if (trienio == 1) {
      load("data/population/defTri1.rda")
    } else if (trienio == 2) {
      load("data/population/defTri2.rda")
    } else if (trienio == 3) {
      load("data/population/defTri3.rda")
    }
    
    incProgress(0.1)
    # filtra según la causa seleccionada
    if (causa != "0000 TODAS LAS CAUSAS") {
      muertes = defTri %>% dplyr::filter(get(lista) == causa)
    } else {
      muertes = defTri
    }
    
    # formatea df de muertes
    muertes = muertes %>% 
      group_by(CODIGODEPTO, SEXO, GRUPEDAD_DEPTOS) %>%
      summarise(DEFUNCIONES= n())
    
    incProgress(0.1)
    # crea tabla con información de población y muertes por departamento
    pobDeptosCenso$CODIGODEPTO = as.character(pobDeptosCenso$CODIGODEPTO)
    tablaDeptos = 
      pobDeptosCenso[pobDeptosCenso$ANO==anoCenso & pobDeptosCenso$SEXO %in% c(1,2),] %>% 
      left_join(muertes, by = c("CODIGODEPTO","GRUPEDAD_DEPTOS","SEXO"))
    tablaDeptos$DEFUNCIONES[is.na(tablaDeptos$DEFUNCIONES)] = 0
    
    # crea tabla de tasas de mortalidad estandar
    tasasEstandar = tasasEstandar[tasasEstandar$CAUSA == causa,]
    incProgress(0.1)
    # crea tabla con RMEs y NBI por depto
    RMEtable = data.frame()
    incProgress(0.2)
    for (i in unique(tablaDeptos$CODIGODEPTO)) {
      #for (i = "70126") {
      RME = ageadjust.indirect(
        count = tablaDeptos$DEFUNCIONES[tablaDeptos$CODIGODEPTO == i],
        pop = tablaDeptos$POBLACION[tablaDeptos$CODIGODEPTO == i]*3,
        stdcount = tasasEstandar$DEFUNCIONES,
        stdpop = tasasEstandar$POBLACION,
        stdrate = NULL, 
        conf.level = 0.95
      )
      
      NBI = list(
        NBI_MUJERES = NBIDEPTOS$`MUJER NBI SI`[NBIDEPTOS$REDCODE==i & NBIDEPTOS$AÑO == anoCenso],
        DENOM_NBI = NBIDEPTOS$`TOTAL MUJERES`[NBIDEPTOS$REDCODE==i & NBIDEPTOS$AÑO == anoCenso]
      )
      
      NBI$PROPNBI_MUJERES = NBI$NBI_MUJERES/NBI$DENOM_NBI
      
      append = data.frame(
        CODIGODEPTO = i,
        RME = RME$sir[["sir"]],
        STDRATE = RME$rate[["adj.rate"]],
        DEFUNCIONES = sum(tablaDeptos$DEFUNCIONES[tablaDeptos$CODIGODEPTO == i]),
        POBLACION = sum(tablaDeptos$POBLACION[tablaDeptos$CODIGODEPTO == i])*3,
        ESPERADAS = RME$sir[["exp"]],
        NBI_MUJERES = NBI$NBI_MUJERES,
        DENOM_NBI_MUJERES = NBI$DENOM_NBI,
        PROP_NBI_MUJERES = NBI$PROPNBI_MUJERES
      )
      
      RMEtable = rbind(
        RMEtable,
        append
      )
      
    }
    incProgress(0.1)
    datosMapa = RMEtable %>% dplyr::select(CODIGODEPTO, POBLACION, RME, DEFUNCIONES, ESPERADAS, STDRATE, NBI_MUJERES, DENOM_NBI_MUJERES, PROP_NBI_MUJERES)
    colnames(datosMapa)[1]="in1"
    
    # suma indicadores al mapa
    mapa = mapa %>% left_join(datosMapa)
    
    
    ##### MAPA RME CON GGPLOT #####
    
    # Paleta para cuartiles
    paleta <- data.frame(
      cuartiles = c("Q1", "Q2", "Q3", "Q4"),
      color = c("#ffffd4", "#fed98e", "#fe9929", "#cc4c02")
    )
    
    # Calcular cuartiles y asignar colores
    mapa$cuartiles <- cut(
      mapa$RME,
      breaks = quantile(mapa$RME, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("Q1", "Q2", "Q3", "Q4"),
      include.lowest = TRUE
    )
    
    mapa <- mapa %>% 
      left_join(paleta, by = c("cuartiles"))
    
    labels = defTri %>% distinct(CODIGODEPTO, NOMPROVRES) 
    
    # Leyenda con valores reales
    minMax <- mapa %>%
      st_drop_geometry() %>%
      group_by(cuartiles) %>%
      summarise(
        minimo = min(RME, na.rm = TRUE),
        maximo = max(RME, na.rm = TRUE)
      ) %>%
      left_join(paleta, by = "cuartiles") %>%
      mutate(rango = paste0(
        format(minimo, digits = 3, big.mark = ".", decimal.mark = ","),
        " - ",
        format(maximo, digits = 3, big.mark = ".", decimal.mark = ",")
      ))
    
    # Cuartiles como factor ordenado
    mapa$cuartiles <- factor(mapa$cuartiles, levels = c("Q1", "Q2", "Q3", "Q4"))
    minMax$cuartiles <- factor(minMax$cuartiles, levels = c("Q1", "Q2", "Q3", "Q4"))
    mapa = mapa %>% left_join(minMax, by = "cuartiles")
    mapa$rango = factor(mapa$rango, levels = minMax$rango)
    mapa = mapa %>% left_join(labels, by = c("in1" = "CODIGODEPTO"))
    mapa$NOMPROVRES[mapa$in1 == "02000"] = "Ciudad Autónoma de Buenos Aires"
    
    if (tipo_mapa == "cuartiles") {
      # Mapa 1: por cuartiles
      pal <- colorFactor(
        palette = paleta$color,
        domain = mapa$rango
      )
      
      mapa$rango = as.factor(mapa$rango)
      # Crear el mapa con Leaflet
      mapa_depto <- leaflet(mapa) %>%
        addPolygons(
          fillColor = ~color.y,
          fillOpacity = 0.7,
          color = "black",
          weight = 0.1,
          popup = ~paste0(
            "<strong>Área:</strong> ", NOMPROVRES, "<br>",
            "<strong>Valor:</strong> ", format(RME, digits = 3), "<br>",
            "<strong>Cuartil:</strong> ", cuartiles, "<br>",
            "<strong>Rango:</strong> ", rango
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = ~rango,
          title = "Quantile cut points",
          labels = minMax$rango,
          opacity = 0.7) %>%
        setView(-60,-45, zoom = 4) 
      # %>%
      #   addTiles('https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png', 
      #            attribution = "Argenmap v2 - Instituto Geográfico Nacional") 
      # 
      # Visualizar el mapa
      mapa_depto %>% addTiles()
      
    } else {
      # Mapa 2: continuo 
      mapa_depto = ggplot(mapa) +
        geom_sf(aes(fill = RME), color = "black", size = 0.1) +
        scale_fill_viridis_c(
          name = "Razones de mortalidad estandarizadas",
          option = "magma",
          direction = -1
        ) +
        labs(title = "RME continua") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
    }
    
  })
  
  
}



