##funciones
calcularUtilidadRestante <- function(ciclosPorAno, esperanzaVida, utilidad) {
  # Esta función devuelve un vector de utilidad restante por ciclo.
  # Para un ciclo dado, el vector contendrá cuánta utilidad (según la población general) le quedaba por vivir
  # para obtener los qalys perdidos ante una muerte prematura.
  
  # Vector que contendrá la utilidad restante de cada ciclo.
  uti <- numeric(101 * ciclosPorAno)
  
  # Recorremos desde 0 todos los ciclos comprendidos entre 0 y 100 años.
  for (i in 0:((101 * ciclosPorAno) - 1)) {
    # Para cada ciclo recorremos desde el ciclo siguiente hasta la cantidad de ciclos correspondiente a la esperanza de vida del país
    for (z in (i + 1):(esperanzaVida * ciclosPorAno)) {
      # A uti le asignamos el valor de la utilidad trimestral del año que corresponde al ciclo.
      # Ajuste para el índice (R empieza en 1, no en 0 como VBA)
      idx <- ((z - 1) %/% ciclosPorAno) + 1
      if (idx <= length(utilidad)) {
        uti[i + 1] <- uti[i + 1] + utilidad[idx]
      }
    }
  }
  
  return(uti)
}



escribirResultadosComparacion <- function(escenario, anosVividos, qalysVividos,
                                          LyPerdidos, qalysPerdidosMP, QalysPerdidosDIS,
                                          qalysPerdidos, muertesHIV, muertesTotales,
                                          casosHIV, casosHIVDx, TiempoSinDx, 
                                          CostoSano, CostoPrep, CostoHIV, 
                                          casosTotales, anosVividosD, qalysVividosD,
                                          anosPerdidosMPD, qalysPerdidosMPD, QalysPerdidosDD,
                                          qalysPerdidosD, cSanoD, cPrEPD, 
                                          cHIVd, cohorteSizeFinal) {
  resultados <- data.frame(
    Escenario = escenario,
    AnosVividos = c(anosVividos, anosVividosD),
    QALYsVividos = c(qalysVividos, qalysVividosD),
    LyPerdidos = c(LyPerdidos, anosPerdidosMPD),
    QALYsPerdidosMP = c(qalysPerdidosMP, qalysPerdidosMPD),
    QALYsPerdidosDIS = c(QalysPerdidosDIS, QalysPerdidosDD),
    QALYsPerdidos = c(qalysPerdidos, qalysPerdidosD),
    MuertesHIV = muertesHIV,
    ProporcionMuertesHIV = muertesHIV / muertesTotales,
    CasosHIV = casosHIV,
    CasosHIVDx = casosHIVDx,
    CasosTotales = casosTotales,
    TiempoSinDx = TiempoSinDx,
    CostoSano = c(CostoSano, cSanoD),
    CostoPrep = c(CostoPrep, cPrEPD),
    CostoHIV = c(CostoHIV, cHIVd),
    CostoTotal = c(CostoSano + CostoPrep + CostoHIV, cSanoD + cPrEPD + cHIVd),
    CohorteSizeFinal = cohorteSizeFinal
  )
  
  # Escribir el data frame en un archivo Excel
  #nombreArchivo <- paste("prep/Resultados_Escenario_", escenario, ".xlsx", sep = "")
  #write.xlsx(resultados, file = nombreArchivo)
  
  return(resultados)
}


escribirOutput <- function(ciclo, ciclosPorAno, sanosSinPrep, sanosOffPrep, sanosOnPrep, infectadosSinDx,
                           infectadosPreDx, infectadosDx, muertosNoHiv, MuertosHIV, personasRiesgo, HIVTotales,
                           CasosNuevos, CasosAcumulados, nuevosDx, qalyNOHIV, qalyHIV, LyPerdidosMP, 
                           QalyPerdidosMP, Disutilidad, CostoSano, CostoPrep, CostoHIV) {
  # Crear una fila de datos
  fila_datos <- data.frame(
    Ciclo = ciclo,
    Ano = ciclo %/% ciclosPorAno,
    SanosSinPrep = sanosSinPrep,
    SanosOffPrep = sanosOffPrep,
    SanosOnPrep = sanosOnPrep,
    InfectadosSinDx = infectadosSinDx,
    InfectadosPreDx = infectadosPreDx,
    InfectadosDx = infectadosDx,
    MuertosNoHiv = muertosNoHiv,
    MuertosHIV = MuertosHIV,
    Total = MuertosHIV + muertosNoHiv + infectadosDx + infectadosPreDx + infectadosSinDx + sanosOnPrep + sanosOffPrep + sanosSinPrep,
    PersonasRiesgo = personasRiesgo,
    HIVTotales = HIVTotales,
    CasosNuevos = CasosNuevos,
    NuevosDx = nuevosDx,
    CasosAcumulados = CasosAcumulados,
    QalyNOHIV = qalyNOHIV,
    QalyHIV = qalyHIV,
    TotalQALY = qalyHIV + qalyNOHIV,
    LyPerdidosMP = LyPerdidosMP,
    QalyPerdidosMP = QalyPerdidosMP,
    Disutilidad = Disutilidad,
    CostoSano = CostoSano,
    CostoPrep = CostoPrep,
    CostoHIV = CostoHIV
  )
  
  # Agregar la fila a un data frame global o escribir en un archivo
  # Opción 1: Agregar a un data frame global (debes crear 'resultados_df' antes)
  # resultados_df <- rbind(resultados_df, fila_datos)
  
  # Opción 2: Escribir/adjuntar a un archivo CSV
  #write.table(fila_datos, file = "prep/resultados.csv", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  
  return(fila_datos)
}

cargarDistribucion <- function(edadMin, edadMax, distribucion_cohortes) {
  
  # Edad mínima y máxima con datos disponibles
  minDistEdad <- 18
  maxDistEdad <- 50
  
  # Ajustar rangos basados en los límites de edad proporcionados
  minR <- ifelse(edadMin <= minDistEdad, edadMin, minDistEdad)
  maxR <- ifelse(edadMax >= maxDistEdad, edadMax, maxDistEdad)
  
  # Crear un vector para almacenar la distribución basal
  distBasal <- numeric(edadMax - edadMin + 1)
  #names(distBasal) <- as.character(minR:maxR)
  
  # Cargar datos para el rango de edades (reemplazar con tu método de carga de datos)
  for (i in 0:32) {
    # Aquí debes reemplazar el acceso a la hoja de Excel por el método apropiado en R
    # Por ejemplo, si los datos están en un dataframe llamado 'datosExcel', podrías hacer algo así:
    # distBasal[as.character(18 + i)] <- datosExcel[6 + i, 8]
    distBasal[18 + i] <- distribucion_cohortes[5 + i, 8]
  }
  
  # Calcular la diferencia en aos entre lo que se tiene y lo que se pide
  difAnos <- 33 - (edadMax - edadMin + 1)
  dsAcumulada <- 0
  
  # Crear un vector para almacenar los resultados
  res <- numeric(edadMax - edadMin + 1)
  #names(res) <- as.character(edadMin:edadMax)
  
  # Calcular la probabilidad acumulada en el rango solicitado
  for (i in edadMin:edadMax ) {
    if (i < 18) {
      # Si la edad es menor a 18, asignar la misma probabilidad que a la edad 18
      distBasal[i] <- distBasal[18]
      dsAcumulada <- dsAcumulada + as.numeric(distBasal[18])
    } else if (i > 50) {
      # Si la edad es mayor a 50, asignar la misma probabilidad que a la edad 50
      distBasal[i] <- distBasal[50]
      dsAcumulada <- dsAcumulada + as.numeric(distBasal[50])
    } else {
      # Si la edad está en el rango, solo sumar esa probabilidad
      dsAcumulada <- dsAcumulada + as.numeric(distBasal[i])
    }
  }
  
  # Calcular la diferencia entre la probabilidad total deseada y la acumulada
  resto <- 1 - dsAcumulada
  
  # Calcular la modificación necesaria si hay una diferencia
  modificacion <- ifelse(resto != 0, resto / (edadMax - edadMin + 1), 0)
  
  # Inicializar la probabilidad acumulada después de realizar los ajustes
  drAcumulada <- 0
  
  # Aplicar el ajuste a cada elemento en el rango deseado
  for (i in edadMin:edadMax) {
    res[i] <- as.numeric(distBasal[i]) + modificacion
    # Acumular la probabilidad para verificar si suma 1 al final
    drAcumulada <- drAcumulada + res[i]
  }
  
  # Ajustar en caso de que la probabilidad total no sume exactamente 1
  if (drAcumulada != 1) {
    res[edadMin] <- res[edadMin] + (1 - drAcumulada)
  }
  
  
  return(res) 
}


####crear parametros

library(readxl)
library(dplyr)
library(rlang)

crearParametros <- function(linea) {
  # Leer los datos del archivo Excel
  parametros <- readxl::read_excel("prep/data/parametros.xlsx")
  
  # Lista de nombres de parámetros a extraer
  nombres_parametros <- c("Tamaño Cohorte:", "Tasa descuento:", "Edad Minima:", 
                          "Edad Final:", "Tipo Duracion:", "Duracion PrEP:", 
                          "Edad Maxima Inicial:", "PrEP upTake:", "Edad No Indicacion de PrEP",
                          "Limite edad Riesgo:", "Eficacia PrEP:", "Adherencia PrEP:",
                          "Limite edad Contagio:", "Cohorte Dinamica")
  
  nombres_parametros_nuevos <- c("cohorteSize", "tasaDescuento", "edadMinima", 
                                 "edadFinal", "tipoDuracion", "duracionPrEP", 
                                 "edadMaximaInicial", "PrEPuptake", "edadFinPrEP",
                                 "limiteEdadRiesgo", "eficaciaPrEP", "adherenciaPrEP",
                                 "limiteEdadContagiosos", "tipoCohorte")
  
  # Crear una lista vacía para almacenar los parámetros
  parametro <- list()
  
  # Bucle para extraer cada parámetro
  for (i in seq_along(nombres_parametros)) {
    nombre_original <- nombres_parametros[i]
    nombre_nuevo <- nombres_parametros_nuevos[i]
    
    parametro[[nombre_nuevo]] <- parametros %>%
      filter(Parametro == nombre_original) %>%
      select(!!sym(linea)) %>%
      as.numeric()
  }
  
  return(parametro)
}

###lista resultados

funcionCalculos <- function(parametros,pais) {
  
  
  withProgress(message = "Ejecutando modelo", value=0, {#[LEAN2 tuve que sacar esto.]
  parametro1 <- list()
  parametro2 <- list()
  
  # Clasificar los elementos en las dos listas
  for (nombre in names(parametros)) {
    if (grepl("_nuevo$", nombre)) {
      parametro2[[gsub("_nuevo$", "", nombre)]] <- parametros[[nombre]]
    } else {
      parametro1[[nombre]] <- parametros[[nombre]]
    }
  }
  
  tipoCohorte=1
  tipoDuracion=1
  
  # Aplica funcionPrincipal para ambos escenarios
  incProgress(0.2)
  
  funcionPrincipal("Baseline", pais, parametro1)
  resultados_baseline <<- resultados
  casosTotalesHIV <- resultados$CasosTotales[1]
  nuevosCasosHivDx <- resultados$CasosHIVDx[1]
  
  incProgress(0.2)
  
  funcionPrincipal("Nuevo", pais, parametro2)
  resultados_nuevo <<- resultados
  casosTotalesHIV2 <- resultados$CasosTotales[1]
  nuevosCasosHivDx2 <- resultados$CasosHIVDx[1]
  
  # Cálculos de promedios, diferencias, etc.
  #Promedios
  if (tipoCohorte == 0) {
    
    AnosVividos_prom_escenario1 = resultados_baseline$AnosVividos[1] / parametro1$cohorteSize 
    AnosVividos_prom_escenario2 = resultados_nuevo$AnosVividos[1] / parametro2$cohorteSize
    qalysVividos_prom_escenario1 = resultados_baseline$QALYsVividos[1] / parametro1$cohorteSize 
    qalysVividos_prom_escenario2 = resultados_nuevo$QALYsVividos[1] / parametro2$cohorteSize
  }
  #LEAN2 TODO ESTO NO VA
  #32
  #LY_perdidos_MP_prom_escenario1 = resultados_baseline$LyPerdidos[1] / resultados_baseline$CasosTotales
  #LY_perdidos_MP_prom_escenario2 = resultados_nuevo$LyPerdidos[1] / resultados_nuevo$CasosTotales
  
  #33
  #qalys_perdidos_disc_prom_escenario1 = resultados_baseline$QALYsPerdidosDIS[1] / resultados_baseline$CasosTotales
  #qalys_perdidos_disc_prom_escenario2 = resultados_nuevo$QALYsPerdidosDIS[1] / resultados_nuevo$CasosTotales
  
  #34
  #qalys_perdidos_mp_prom_escenario1 = resultados_baseline$QALYsPerdidosMP[1] / resultados_baseline$CasosTotales
  #qalys_perdidos_mp_prom_escenario2 = resultados_nuevo$QALYsPerdidosMP[1] / resultados_nuevo$CasosTotales
  
  #35
  #qalys_perdidos_prom_escenario1 = resultados_baseline$QALYsPerdidos[1] / resultados_baseline$CasosTotales
  #qalys_perdidos_prom_escenario2 = resultados_nuevo$QALYsPerdidos[1] / resultados_nuevo$CasosTotales
  
  
  #41
  #qalys_perdidos_prom_escenario1 = resultados_baseline$TiempoSinDx[1] / resultados_baseline$CasosHIVDx
  #qalys_perdidos_prom_escenario2 = resultados_nuevo$TiempoSinDx[1] / resultados_nuevo$CasosHIVDx
  
  
  # ahora diferencias
  
  ##Columna total del excel
  #30
  AnosVividos_total <- resultados_nuevo$AnosVividos[1]-resultados_baseline$AnosVividos[1]
  
  #31
  qalysVividos_total <- resultados_nuevo$QALYsVividos[1]-resultados_baseline$QALYsVividos[1]
  
  #30d
  AnosVividos_total_d <- resultados_nuevo$AnosVividos[2]-resultados_baseline$AnosVividos[2]
  
  #31d
  qalysVividos_total_d <- resultados_nuevo$QALYsVividos[2]-resultados_baseline$QALYsVividos[2]
  
  incProgress(0.2) #[LEAN2 tuve que sacar esto.]
  
  #32
  
  LY_perdidos_MP_total <- resultados_nuevo$LyPerdidos[1]-resultados_baseline$LyPerdidos[1]
  LY_perdidos_MP_total_d <- resultados_nuevo$LyPerdidos[2] - resultados_baseline$LyPerdidos[2]
  
  #33
  
  qalys_perdidos_disc_total <-  resultados_nuevo$QALYsPerdidosDIS[1]-resultados_baseline$QALYsPerdidosDIS[1]
  qalys_perdidos_disc_total_d <-  resultados_nuevo$QALYsPerdidosDIS[2]-resultados_baseline$QALYsPerdidosDIS[2]
  #34
  
  qalys_perdidos_mp_total <-  resultados_nuevo$QALYsPerdidosMP[1]-resultados_baseline$QALYsPerdidosMP[1]
  
  #35
  
  qalys_perdidos_total <-  resultados_nuevo$QALYsPerdidos[1]-resultados_baseline$QALYsPerdidos[1]
  
  #36
  
  muertes_hiv_total <-  resultados_nuevo$MuertesHIV[1]-resultados_baseline$MuertesHIV[1]
  
  
  #37
  
  muertes_hiv_prop_total <-  resultados_nuevo$ProporcionMuertesHIV[1]-resultados_baseline$ProporcionMuertesHIV[1]
  #38
  
  casos_hiv_nuevos_total <-  resultados_nuevo$CasosHIV[1]-resultados_baseline$CasosHIV[1]
  
  #39
  nuevos_casos_HIV_dx_total= nuevosCasosHivDx2 - nuevosCasosHivDx
  
  incProgress(0.2) #[LEAN2 tuve que sacar esto.]
  
  # 40
  casos_totales_HIV_total <- casosTotalesHIV2 - casosTotalesHIV
  
  #41
  tiempo_sin_dx_total <-  resultados_nuevo$TiempoSinDx[1]-resultados_baseline$TiempoSinDx[1]
  
  #42
  costo_sano_testeo_total <-  resultados_nuevo$CostoSano[1]-resultados_baseline$CostoSano[1]
  #43
  costo_prep_total <-  resultados_nuevo$CostoPrep[1]-resultados_baseline$CostoPrep[1]
  #44
  costo_hiv_total <-  resultados_nuevo$CostoHIV[1]-resultados_baseline$CostoHIV[1]
  
  #45
  costo_total <-  resultados_nuevo$CostoTotal[1]-resultados_baseline$CostoTotal[1]
  
  
  costo_sano_testeo_total_d <- resultados_nuevo$CostoSano[2]-resultados_baseline$CostoSano[2]
  costo_prep_total_d <- resultados_nuevo$CostoPrep[2]-resultados_baseline$CostoPrep[2]
  costo_hiv_total_d <- resultados_nuevo$CostoHIV[2]-resultados_baseline$CostoHIV[2]
  costo_total_d <- resultados_nuevo$CostoTotal[2] - resultados_baseline$CostoTotal[2]
  
  incProgress(0.2)
  
  #46
  
  Costo_incremental_Qaly <- costo_total/qalysVividos_total
  Costo_incremental_Qaly_d <- costo_total_d/qalysVividos_total_d
  Costo_incremental_AVAD <- costo_total / -(qalys_perdidos_disc_total + LY_perdidos_MP_total)
  Costo_incremental_AVAD_d <- costo_total_d / -(qalys_perdidos_disc_total_d + LY_perdidos_MP_total_d)
  
  #47  
  Costo_incremental_Ano_vida <- costo_total/ -(LY_perdidos_MP_total)
  Costo_incremental_Ano_vida_d <- costo_total_d/ -(LY_perdidos_MP_total_d)
  
  Costo_Incremental_VidaSalvada <- costo_total / -(muertes_hiv_total)
  Costo_Incremental_VidaSalvada_d <- costo_total_d / -(muertes_hiv_total)
  
  
  ROI <- -costo_total/costo_prep_total*100
  
  ROI_d <- -costo_total_d/costo_prep_total_d*100
  
  
  # Crear una lista para almacenar todos los resultados
  lista_resultados1 <<- data.frame(
    #epis
    casosEvitados = -(casos_hiv_nuevos_total), #Casos de HIV evitados
    muertesEvitadas = -(muertes_hiv_total), # Muertes evitadas
    AnosVidaSalvados = -LY_perdidos_MP_total, # Años de vida salvados
    AnosVidaSalvados_d = -LY_perdidos_MP_total_d,
    AVADEvitados = -(qalys_perdidos_disc_total + LY_perdidos_MP_total),# Años de vida ajustados por discapacidad evitados
    AVADEvitados_d = -(qalys_perdidos_disc_total_d + LY_perdidos_MP_total_d),
    #economicos
    costoIntervencion = costo_prep_total, #Costo total de la intervención
    costoIntervencion_d = costo_prep_total_d, #Costo total de la intervención descontado
    costosEvitados = -(costo_hiv_total + costo_sano_testeo_total), #Costos evitados atribuibles a la intervención
    costosEvitados_d = -(costo_hiv_total_d + costo_sano_testeo_total_d), #Costos evitados atribuibles a la intervención descontados
    diferenciaCostos = costo_total,#Diferencia de costos respecto al escenario basal
    diferenciaCostos_d = costo_total_d, #Diferencia de costos descontados
    
    RCEIVidaSalvada = Costo_Incremental_VidaSalvada, #Razon de costo-efectividad incremental por vida salvada
    RCEIVidaSalvada_d = Costo_Incremental_VidaSalvada_d, #Razon de costo-efectividad incremental por vida salvada descontada
    RCEIAnoVidaSalvado = Costo_incremental_Ano_vida, #Razon de costo-efectividad incremental por año de vida salvado 
    RCEIAnoVidaSalvado_d = Costo_incremental_Ano_vida_d, #Razon de costo-efectividad incremental por año de vida salvado descontada
    RCEIAVAD = Costo_incremental_AVAD, #RCEI por año de vida ajustado por discapacidad evitado
    RCEIAVAD_d = Costo_incremental_Qaly_d, #Razon de costo-efectividad incremental por año de vida ajustado por discapacidad evitado descontada
    
    ROI = ROI, #Retorno sobre inversión
    ROI_d = ROI_d #Retorno sobre inversión
  )
  
  # Convertir el dataframe a formato largo
  lista_resultados <<- stack(lista_resultados1)
  
  # Renombrar las columnas
  names(lista_resultados) <<- c("Valor", "Parametro")
  
  # Ordena las columnas para que "Parametro" esté primero
  lista_resultados<<- lista_resultados[, c("Parametro", "Valor")]
  
  })#[LEAN2 tuve que sacar esto.]
  
  
  return(lista_resultados)
}

