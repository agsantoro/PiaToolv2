library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
#Modificado 23/12 <<<--
INFO_ROI_NOAPLICA <- "El retorno de inversión no fue estimado debido a que la intervención es menos costosa que el comparador."
ICER_C_SUP_IZQ <- "La intervención es menos costosa y menos efectiva, el valor presente representa el RCEI del comparador contra la intervención. Por tanto, la intervención será costo-efectiva si RCEI está por encima del umbral."
#Modificado 23/12 -->>>

MAX_AÑOS_MUERTES <- 5

formatear_pesos <- function(x, decimales = 0) {
  if (is.numeric(x)){
  return(formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales))} else {
    return(x)
  }
}

formatear_pesos2 <- function(x, decimales = 0) {
  if (is.numeric(x)){
    return(paste0("$", formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales)))} else {
    return(x)
  }
}
formatear_epi <- function(x, decimales = 2) {
  formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales)
}
formatear_porcentaje <- function(x, decimales = 0) {
  if (is.numeric(x)){
    return(paste0(formatC(x*100, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales), " %"))} else 
  { return(x)}
}
procesarResultados <- function(basal, proyectado) {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Recibe los resultados de cada escenario y prepara los outputs%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

  #Indicadores principales de ejemplo para el visualizador.
  indicadores <- list(deltaTratadas = formatear_epi(proyectado$gestantesTratadas - basal$gestantesTratadas),
                      deltaMortinatos = formatear_epi(proyectado$mortinatos - basal$mortinatos),
                      deltaMuerteNeonatal = formatear_epi(proyectado$muertesNeonatales - basal$muertesNeonatales),
                      deltaSifilisCongenita = formatear_epi(proyectado$sifilisCongenitas - basal$sifilisCongenitas) , 
                      deltaCostoTesteo = formatear_pesos2(proyectado$costosTesteo - basal$costosTesteo),
                      deltaCostoTratamiento = formatear_pesos2(proyectado$costosTratamiento - basal$costosTratamiento)
  )

  #Eventos sanitarios
  tabla_sanitaria <- data.frame(
    Categorias = c("Gestantes Testeadas", "Gestantes Correctamente Tratadas", "Gestantes Incorrectamente Tratadas", "Mortinatos", "Muertes Neonatales", "Niños en Seguimiento", "Sífilis Congénita"),
    Escenario_Actual = c(basal$gestantesTesteadas, basal$VPTratados, basal$FPTratados, basal$mortinatos, basal$muertesNeonatales, basal$seguimientos, basal$sifilisCongenitas),
    Escenario_Proyectado = c(proyectado$gestantesTesteadas, proyectado$VPTratados, proyectado$FPTratados, proyectado$mortinatos, proyectado$muertesNeonatales, proyectado$seguimientos, proyectado$sifilisCongenitas),
    Diferencia =  c(proyectado$gestantesTesteadas - basal$gestantesTesteadas, proyectado$VPTratados - basal$VPTratados, proyectado$FPTratados - basal$FPTratados, proyectado$mortinatos - basal$mortinatos, proyectado$muertesNeonatales - basal$muertesNeonatales, proyectado$seguimientos - basal$seguimientos, proyectado$sifilisCongenitas - basal$sifilisCongenitas)
  )
  #Formateamos la tabla
  tabla_sanitaria[] <- lapply(tabla_sanitaria, function(col) {
    if (is.numeric(col)) formatear_epi(col) else col
  })
  #Nombramos los headers
  colnames(tabla_sanitaria) <- c("Categorias", 
                                 "Comparador", 
                                 "Intervención", 
                                 "Diferencia")  
  
  #Resultados Costos
  tabla_costos <- data.frame(
    Categorias = c("Costos de Testeo", "Costos de Tratamiento de Gestantes", "Costos diagnosticos al nacimiento", "Costos terapeuticos al nacimiento", "Costos de hospitalizacion al nacimiento"),
    Escenario_Actual = c(basal$costosTesteo, basal$costosTratamiento, basal$costosDiagnosticosNacimiento, basal$costosTratamientoNacimiento, basal$costosHospNacimiento),
    Escenario_Proyectado = c(proyectado$costosTesteo, proyectado$costosTratamiento, proyectado$costosDiagnosticosNacimiento, proyectado$costosTratamientoNacimiento, proyectado$costosHospNacimiento),
    Diferencia =  c(proyectado$costosTesteo - basal$costosTesteo, proyectado$costosTratamiento - basal$costosTratamiento, proyectado$costosDiagnosticosNacimiento - basal$costosDiagnosticosNacimiento, proyectado$costosTratamientoNacimiento - basal$costosTratamientoNacimiento, proyectado$costosHospNacimiento - basal$costosHospNacimiento)
  )
  #Formateamos la tabla
  tabla_costos[] <- lapply(tabla_costos, function(col) {
    if (is.numeric(col)) formatear_pesos2(col) else col
  })
  #Nombramos Headers
  colnames(tabla_costos) <- c("Categorias", 
                                 "Comparador", 
                                 "Intervención", 
                                 "Diferencia")
  
  
  #Resultados DALYS
  tabla_dalys <- data.frame(
    Categorias = c("Años de vida perdidos por muerte prematura", "Años de vida perdidos por muerte prematura (d)", "Años de vida Ajustados por Discapacidad por vivir con Sifilis Congenita", "Años de vida Ajustados por Discapacidad por vivir con Sifilis Congenita (d)", "Años de Vida Ajustados por Discapacidad", "Años de Vida Ajustados por Discapacidad (d)"),
    Escenario_Actual = c(basal$añosVidaPerdidos, basal$añosVidaPerdidosDesc, basal$dalysSifilisCongenita, basal$dalysSifilisCongenitaDesc, basal$añosVidaPerdidos + basal$dalysSifilisCongenita, basal$añosVidaPerdidosDesc + basal$dalysSifilisCongenitaDesc),
    Escenario_Proyectado = c(proyectado$añosVidaPerdidos, proyectado$añosVidaPerdidosDesc, proyectado$dalysSifilisCongenita, proyectado$dalysSifilisCongenitaDesc, proyectado$añosVidaPerdidos + proyectado$dalysSifilisCongenita, proyectado$añosVidaPerdidosDesc + proyectado$dalysSifilisCongenitaDesc),
    Diferencia =  c(proyectado$añosVidaPerdidos - basal$añosVidaPerdidos, proyectado$añosVidaPerdidosDesc - basal$añosVidaPerdidosDesc, proyectado$dalysSifilisCongenita - basal$dalysSifilisCongenita, proyectado$dalysSifilisCongenitaDesc - basal$dalysSifilisCongenitaDesc,  (proyectado$añosVidaPerdidos + proyectado$dalysSifilisCongenita) - (basal$añosVidaPerdidos + basal$dalysSifilisCongenita),(proyectado$añosVidaPerdidosDesc + proyectado$dalysSifilisCongenitaDesc) - (basal$añosVidaPerdidosDesc + basal$dalysSifilisCongenitaDesc))
  )
  #Formateamos la tabla
   tabla_dalys[] <- lapply(tabla_dalys, function(col) {
     if (is.numeric(col)) formatear_epi(col) else col
   })
  #Nombramos headers
  colnames(tabla_dalys) <- c("Categorias",
                              "Comparador",
                              "Intervención",
                              "Diferencia")
  
  #Calculamos delta costos para estimar ICERS y ROI
  costo_total_intervencion <- proyectado$costosTesteo + proyectado$costoProgramatico
  diferencia_otros_costos <- (basal$costosTratamiento + basal$costosDiagnosticosNacimiento + basal$costosTratamientoNacimiento + basal$costosHospNacimiento - proyectado$costosTratamiento - proyectado$costosDiagnosticosNacimiento - proyectado$costosTratamientoNacimiento - proyectado$costosHospNacimiento)
  diferencia_costos <- (costo_total_intervencion - basal$costosTesteo) - diferencia_otros_costos
  
  
  
  inversion <- costo_total_intervencion - basal$costosTesteo
  
  #Modificado 23/12 <<<--
  
  #[ROI/ICER] Modifique esta parte para implementarlo en una funcion simple
  estimarRoi <- function(inversion, ganancia){
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Estima el ROI, solo si corresponde.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 23/12 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
    if (inversion > 0)
    { 
      return(list(valor = ((diferencia_otros_costos - inversion) / inversion), info = ""))
    } else {
      return(list(valor = "-", info = INFO_ROI_NOAPLICA))
    }
  }
  roi <- estimarRoi(inversion, diferencia_otros_costos)
  
  
  interpretacionDecision <- function(deltaCosto, deltaBeneficio) {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Estima el ICER, si es costo-ahorrativo devuelve Dominante para evitar confusión con ICER negativo.%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
    if (deltaCosto <= 0 & deltaBeneficio > 0)
    {
        return(list(valor = "Dominante", info = ""))
    } else {
      if (deltaCosto <= 0 & deltaBeneficio < 0) {
        return(list(valor = "Menos Efectiva", info = ICER_C_SUP_IZQ))
        #[ROI/ICER]
        #Lo correcto aca sería devolver return(deltaCosto/deltaBeneficio) pero con mención tal vez poner un *
      } else {
        if (deltaCosto > 0 & deltaBeneficio < 0) {
          return(list(valor = "Dominada", info = ""))
        } else {
          if (deltaCosto > 0 & deltaBeneficio > 0) {
            return(list(valor = deltaCosto/deltaBeneficio, info = ""))
          }
        }
      }
    }
  }
  
    #Estima los ICERS.
    icer_añosVida <- interpretacionDecision(diferencia_costos, (basal$añosVidaPerdidos - proyectado$añosVidaPerdidos))
    icer_añosVidaDesc <- interpretacionDecision(diferencia_costos, (basal$añosVidaPerdidosDesc - proyectado$añosVidaPerdidosDesc))
    icer_dalys <- interpretacionDecision(diferencia_costos, (basal$añosVidaPerdidos + basal$dalysSifilisCongenita - proyectado$añosVidaPerdidos - proyectado$dalysSifilisCongenita))
    icer_dalysDesc <- interpretacionDecision(diferencia_costos, (basal$añosVidaPerdidosDesc + basal$dalysSifilisCongenitaDesc - proyectado$añosVidaPerdidosDesc - proyectado$dalysSifilisCongenitaDesc))
  
    
  #[ROI/ICER] en la tabla ahora mostramos el objeto valor dentro de la lista que representa el ICER y el ROI
  #Preparamos la tabla MAIN 
  tabla_main <- data.frame(
    Categorias = c("Gestantes Testeadas Adicionales", "Sifilis Gestacionales Tratadas Adicionales", "Mortinatos Evitados", "Muertes Neonatales Evitadas", "Casos confirmados de Sifilis Congenita Evitados", "Muertes Evitadas", "Años de Vida Salvados", "Años de Vida Ajustados por Discapacidad Evitados", "Costo Total de la Intervención (USD)", "Costos Evitados atribuibles a la intervención (USD)", "Diferencia de costos respecto al escenario basal (USD)", "Razón de costo-efectividad incremental por año de vida salvado (USD)", "Razón de costo-efectividad incremental por año de vida ajustado por discapacidad evitado (USD)", "Retorno de Inversión (%)"),
    Valor = c((proyectado$gestantesTesteadas - basal$gestantesTesteadas), (proyectado$VPTratados - basal$VPTratados), (basal$mortinatos - proyectado$mortinatos), (basal$muertesNeonatales - proyectado$muertesNeonatales), ((basal$sifilisCongenitas + basal$sifilisCongenitasAsin) - (proyectado$sifilisCongenitas + proyectado$sifilisCongenitasAsin)), (basal$muertes - proyectado$muertes), (basal$añosVidaPerdidos - proyectado$añosVidaPerdidos), (basal$añosVidaPerdidos + basal$dalysSifilisCongenita - proyectado$añosVidaPerdidos - proyectado$dalysSifilisCongenita),
              costo_total_intervencion, diferencia_otros_costos, diferencia_costos, icer_añosVida$valor, icer_dalys$valor , roi$valor),
    Valor_descontado = c("-", "-", "-", "-", "-", "-", (basal$añosVidaPerdidosDesc - proyectado$añosVidaPerdidosDesc), (basal$añosVidaPerdidosDesc + basal$dalysSifilisCongenitaDesc - proyectado$añosVidaPerdidosDesc - proyectado$dalysSifilisCongenitaDesc),
                         "-", "-", "-", icer_añosVidaDesc$valor, icer_dalysDesc$valor, roi$valor)
   )
  
  #Modificado 23/12 -->>>
  #Formateamos cada valor de la tabla
  func_format <- c(
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_pesos2,
    formatear_pesos2,
    formatear_pesos2,
    formatear_pesos2,
    formatear_pesos2,
    formatear_porcentaje
  )
  tabla_main$Valor <- mapply(
    function(f, v) {
      num <- suppressWarnings(as.numeric(v))
        if (!is.na(num)) {
          f(num)
        } else {
          v
        }
      },
    func_format,
    tabla_main$Valor
  )
  tabla_main$Valor_descontado <- mapply(
    function(f, v) {
      num <- suppressWarnings(as.numeric(v))
      if (!is.na(num)) {
        f(num)
      } else {
        v
      }
    },
    func_format,
    tabla_main$Valor_descontado
  )
  #Renombramos headers
  colnames(tabla_main) <- c("Indicador",
                             "Valor",
                             "Valor Descontado")
  
  #Modificado 23/12 <<<--
  #[ROI/ICER] devolvemos infoICER cuando alguno de los ICER tiene el problema en cuestión e infoROI
  incersInfo <-  c(icer_dalys$info, icer_dalysDesc$info, icer_añosVida$info, icer_añosVidaDesc$info)
  #Devolvemos resultados
  resultado = list(
    indicadores = indicadores,
    tablaCostos = tabla_costos,
    tablaSanitaria = tabla_sanitaria,
    tablaDalys = tabla_dalys,
    tablaMain = tabla_main,
    infoICER = ifelse(any(incersInfo != ""), incersInfo[incersInfo != ""][1], ""),
    infoROI = roi$info
  )
  #Modificado 23/12 -->>>
  return(resultado)  
  
  
}




correrFuncion <- function() {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Función dummy, carga valores del excel, seleciona argentina y corre el modelo%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
  
  parametros <- cargar()
  
  resultados <- correrModelo(parametros[['ARGENTINA']])
  
}
correrModelo <- function(parametros) {

  

  
  descontar <- function(años, tasa)
  {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Trae al presente valor futuro de x cantidad de años%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
        return(1 + ((1 - (1 + tasa) ^ (- (años - 1) )) / tasa))
  }
  descontarValorCiclo <- function(valor, tasa, ciclo) {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Descuento clásico de un valor x al ciclo y.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    return(valor / ((1 + tasa) ^ ciclo))
  }
  descontarAñosFuturos <- function(valor, tasa, cicloInicial, duracion, valorInicial) {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Descuenta un valor en el tiempo, por una duración dada.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Sirve para calcular años de vida perdido que empiezan a contar mas adelante que el primer año%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    return(
      
      (valorInicial / ((1 + tasa) ^ (cicloInicial - 1))) + 
      (valor / ((1 + tasa) ^ (cicloInicial - 1)) * (1 - (1 + tasa) ^ -(duracion - cicloInicial)) / tasa)
      
    )
    
  }
  
  calcularEventos <- function(gestantes, tieneSifilis, pTratados, parametros) {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Calcula los outcomes gestacionales de un subgrupo de gestantes%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if (tieneSifilis) {
      pMortinato <- parametros$pMortinatoSG
      pMuerteNeonatal <- parametros$pMuerteNeonatalSG 
      pMortinatoTTO <- parametros$pMortinatoSG * (1 - parametros$efPenicilinaMortinato)
      pMuerteNeonatalTTO <- parametros$pMuerteNeonatalSG * (1 - parametros$efPenicilinaMuerteNeonatal)
      pSCSintomatica <- parametros$pSifilisCongenitaSG
      pSCAsintomatica <- parametros$pSifilisCongenitaSGAsin
      pSCSintomaticaTTO <- parametros$pSifilisCongenitaSG * (1 - parametros$efPenicilinaSifilisCongenita)
      pSCAsintomaticaTTO <- parametros$pSifilisCongenitaSGAsin * (1 - parametros$efPenicilinaSifilisCongenita)
      
      pSano = 0
      pSeguimiento <- (1 - (pMuerteNeonatal + pSCSintomatica + pSCAsintomatica))
      pSeguimientoTTO <- (1 - (pMuerteNeonatalTTO + pSCSintomaticaTTO + pSCAsintomaticaTTO))
      
    } else {
      #Esto en realidad habría que ajustarlo por la cantidad de sifilis y sacar una probabilidad de mortinato basal
      pMortinato <- parametros$rMortinato / 1000
      pMuerteNeonatal <- parametros$rMuerteNeonatal / 1000
      pMortinatoTTO <- 0
      pMuerteNeonatalTTO <- 0
      pSCSintomatica <- 0
      pSCAsintomatica <- 0
      pSCSintomaticaTTO <- 0
      pSCAsintomaticaTTO <- 0
      
      pSano = (1 - pMuerteNeonatal)
      pSeguimiento = 0
      pSeguimientoTTO = 0
    }
    
    nVivosTTO <- gestantes * pTratados * (1 - pMortinatoTTO)
    nVivos <- gestantes * (1 - pTratados) * (1 - pMortinato)
    

    
    res <- list(
      sanos = nVivos * pSano + nVivosTTO * pSano,
      seguimientos = nVivos * pSeguimiento,
      seguimientosTratados = nVivosTTO * pSeguimientoTTO,
      mortinatos = gestantes * pTratados * pMortinatoTTO + gestantes * (1 - pTratados) * pMortinato,
      muertesNeonatales = nVivos * pMuerteNeonatal + nVivosTTO * pMuerteNeonatalTTO,
      sifilisCongenitas = nVivos * pSCSintomatica + nVivosTTO * pSCSintomaticaTTO,
      sifilisCongenitasAsin = nVivos * pSCAsintomatica + nVivosTTO * pSCAsintomaticaTTO
    )

    return(res)
  }
  
  agruparResultados <- function(listas) {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Agrupa los outcomes gestacionales en una sola lista%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    return(
      list(
        sanos = sum(sapply(listas, '[[', 'sanos')),
        seguimientos = sum(sapply(listas, '[[', 'seguimientos')),
        seguimientosTratados = sum(sapply(listas, '[[', 'seguimientosTratados')),
        mortinatos = sum(sapply(listas, '[[', 'mortinatos')),
        muertesNeonatales = sum(sapply(listas, '[[', 'muertesNeonatales')),
        sifilisCongenitas = sum(sapply(listas, '[[', 'sifilisCongenitas')),
        sifilisCongenitasAsin = sum(sapply(listas, '[[', 'sifilisCongenitasAsin'))
      )
    )
    
  }
  
  calcularResultados <- function(pTesteo, pTratamiento, cCostoTest, cCostoTTOGestante, sensibilidad, especificidad, nacimientos, nacimientosAtendidos, sifilisGestacionales, sifilisGestacionalesAtendidas, parametros) {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Calcula eventos, dalys y costos por escenario%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    

    nacimientosNoAtendidos <- (nacimientos - nacimientosAtendidos)

    sifilisGestacionalesNoAtendidas <- (sifilisGestacionales - sifilisGestacionalesAtendidas)
    
    
    #Calcula los outcomes gestacionales de cada subgrupo de pacientes
    #No Atendidos (con y sin Sifilis), Atendidos No Testeados (con y sin sifilis), Atendidos y Testeados (con y sin Sifilis)
    #Nacimientos no Atendidos sin Sifilis
    sinSifilisNoAtendidos <- calcularEventos(gestantes =  (nacimientosNoAtendidos - sifilisGestacionalesNoAtendidas), tieneSifilis = 0, pTratados = 0, parametros = parametros)

    #Nacimientos no atendidos con sifilis
    sifilisNoAtendidos <- calcularEventos(gestantes = sifilisGestacionalesNoAtendidas, tieneSifilis = 1, pTratados =  0, parametros = parametros)

    #Nacimientos atendidos pero no Testeados sin Sifilis
    sinSifilisAtendidosNoTesteados <- calcularEventos(gestantes = (nacimientosAtendidos - sifilisGestacionalesAtendidas) * (1 - pTesteo), tieneSifilis =  0, pTratados = 0, parametros = parametros)

    #Nacimientos atendidos pero no Testeados con Sifilis
    sifilisAtendidosNoTesteados <- calcularEventos(gestantes = sifilisGestacionalesAtendidas * (1 - pTesteo), tieneSifilis =  1, pTratados = 0, parametros = parametros)
    


    
    #Nacimientos atendidos y Testeados con Sifilis
      #Test Positivos, weighted average con tratamiento
    sifilisTestPositivos <- calcularEventos(gestantes = sifilisGestacionalesAtendidas * pTesteo * sensibilidad, tieneSifilis = 1, pTratados =  pTratamiento, parametros = parametros)
      #Test Negativos, no tratados
    sifilisTestNegativos <- calcularEventos(gestantes = sifilisGestacionalesAtendidas * pTesteo *  (1 - sensibilidad), tieneSifilis = 1 , pTratados = 0, parametros = parametros)
    
    
      #Test Positivos
    sinSifilisTestPositivos <- calcularEventos(gestantes = (nacimientosAtendidos - sifilisGestacionalesAtendidas) * pTesteo * (1 - especificidad), tieneSifilis = 0, pTratados = 0, parametros = parametros)
      #Test Negativos
    sinSifilisTestNegativos <- calcularEventos(gestantes = (nacimientosAtendidos - sifilisGestacionalesAtendidas) * pTesteo * especificidad, tieneSifilis = 0, pTratados = 0, parametros = parametros)
    

    
    nGestantesTesteadas <- nacimientosAtendidos * pTesteo
    
    nGestantesCorrectamenteTratadas <- sifilisGestacionalesAtendidas * pTesteo * sensibilidad * pTratamiento
    
    nGestantesIncorrectamenteTratadas <- (nacimientosAtendidos - sifilisGestacionalesAtendidas) * pTesteo * (1 - especificidad) * pTratamiento
    
    nGestantesTratadas <- nGestantesCorrectamenteTratadas + nGestantesIncorrectamenteTratadas
    
    #Sumamos todos los eventos del escenario
    eventos <- agruparResultados(list(sinSifilisNoAtendidos, sifilisNoAtendidos, sinSifilisAtendidosNoTesteados, sifilisAtendidosNoTesteados, sifilisTestPositivos, sifilisTestNegativos, sinSifilisTestPositivos, sinSifilisTestNegativos))
    
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Estimamos DALYS y MUERTES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Descontamos la expectativa de vida al nacer
    tExpectativaVidaNacerDesc <- descontar((parametros$tExpectativaVida), parametros$pDescuento)
    
    #Los que nacen muertos y los que mueren en etapa neonatal pierden toda la expectativa de vida.
    añosVidaPerdidosNeontales <- eventos$mortinatos * parametros$tExpectativaVida + eventos$muertesNeonatales * parametros$tExpectativaVida
    añosVidaPerdidosNeonatalesDesc <- eventos$mortinatos * tExpectativaVidaNacerDesc + eventos$muertesNeonatales * tExpectativaVidaNacerDesc
    #Los que nacen con sifilis congenita...
    
    # Una cierta parte muere en la etapa neonatal, ya están contemplados en las muertes neonatales incrementales por sifilis
    # ya sea por infección o placentitis. Pero, si tenia sifilis y se muere no aporta disutilidad adicional por tanto
    # hay que excluirlos.
    
    sobrevivientesSifilisSintomaticos <- eventos$sifilisCongenitas * (1 - parametros$pMuerteNeonatalSC)
    sobrevivientesSifilisAsintomaticos <- eventos$sifilisCongenitasAsin *(1 - parametros$pMuerteNeonatalSCAsin)
    
    
    #Si vamos a tener en cuenta las muertes adicionales de estos chicos que tienen sifilis congenita
    #y tenemos un estudio en brasil que muestra que se mueren mas
    tmpSobrevivientesAsintomaticos <- sobrevivientesSifilisAsintomaticos
    tmpSobrevivientesSintomaticos <- sobrevivientesSifilisSintomaticos
    
    añosVidaPerdidosSC <- 0
    añosVidaPerdidosSCDesc <- 0
    muertesSC <- 0
    muertesSintomaticos <- numeric(MAX_AÑOS_MUERTES)
    muertesAsintomaticos <- numeric(MAX_AÑOS_MUERTES)
    sobrevivientesSintomaticos <- numeric(MAX_AÑOS_MUERTES)
    sobrevivientesAsintomaticos <- numeric(MAX_AÑOS_MUERTES)
    tExpectativaVidaDesc <- numeric(MAX_AÑOS_MUERTES)
    
    #Estimamos años de vida perdidos por SC dado muerte prematura y estimamos sobrevivientes.
    if (parametros$tConsideraMuertes > 0) {
      for(t in 1:parametros$tConsideraMuertes)
      {
        #Estimamos las muertes incrementales (Multiplicamos las tasas de los paises * (RR - 1))
        muertesSintomaticos[t] <- tmpSobrevivientesSintomaticos * parametros[[paste0("tMuerte", t - 1)]] * (parametros[[paste0('rrMuerteSintomaticos', t - 1)]] - 1)
        muertesAsintomaticos[t] <- tmpSobrevivientesAsintomaticos * parametros[[paste0("tMuerte", t - 1)]] * (parametros[[paste0('rrMuerteAsintomaticos', t - 1)]] - 1)
        #Estimamos los que sobreviven al año t; o siguen vivos al inicio del t + 1.
        sobrevivientesSintomaticos[t] <- tmpSobrevivientesSintomaticos - (tmpSobrevivientesSintomaticos * parametros[[paste0("tMuerte", (t - 1))]] * parametros[[paste0('rrMuerteSintomaticos', t - 1)]])
        sobrevivientesAsintomaticos[t] <- tmpSobrevivientesAsintomaticos - (tmpSobrevivientesAsintomaticos * parametros[[paste0("tMuerte", (t - 1))]] * parametros[[paste0('rrMuerteAsintomaticos', t - 1)]])
        
        #Actualizamos las variables temporales
        tmpSobrevivientesSintomaticos <- sobrevivientesSintomaticos[t]
        tmpSobrevivientesAsintomaticos <- sobrevivientesAsintomaticos[t]
        
        #Estimamos la expectativa de vida descontada
        tExpectativaVidaDesc[t] <-  descontarAñosFuturos(1, parametros$pDescuento, t, parametros$tExpectativaVida, 0.5)
        
        muertesSC <- muertesSC + muertesSintomaticos[t] + muertesAsintomaticos[t]
        #Estimamos los años de vida perdidos por muertes incrementales asumiendo que mueren a mitad del año.
       añosVidaPerdidosSC <- añosVidaPerdidosSC + (parametros$tExpectativaVida - (t - 0.5)) *  (muertesSintomaticos[t]  + muertesAsintomaticos[t])
       añosVidaPerdidosSCDesc <- añosVidaPerdidosSCDesc + tExpectativaVidaDesc[t] * (muertesSintomaticos[t]  + muertesAsintomaticos[t])
      
 
     }
    }
    #Ajustamos los sobrevivientes para cerrar las muertes hasta donde querian valorarlo.
    if (parametros$tConsideraMuertes < MAX_AÑOS_MUERTES) {
      for (t in (parametros$tConsideraMuertes + 1):MAX_AÑOS_MUERTES) {
        sobrevivientesSintomaticos[t] <- tmpSobrevivientesSintomaticos
        sobrevivientesAsintomaticos[t] <- tmpSobrevivientesAsintomaticos
      } 
    }
    
    añosVidaConDiscapacidad <- 0
    añosVidaConDiscapacidadDesc <- 0
    
    #Estimamos la disutilidad por secuelas de SC
    if (parametros$tConsideraDisutilidad > 0) {
      for (t in 1:parametros$tConsideraDisutilidad)
      {
        if (t <= MAX_AÑOS_MUERTES)
        {
          discapacitados <- sobrevivientesSintomaticos[t] * parametros$pSecuelasSintomatico + sobrevivientesAsintomaticos[t] * parametros$pSecuelasAsintomatico
        } else {
          discapacitados <- sobrevivientesSintomaticos[MAX_AÑOS_MUERTES] * parametros$pSecuelasSintomatico + sobrevivientesAsintomaticos[MAX_AÑOS_MUERTES] * parametros$pSecuelasAsintomatico
        }
        #Sumamos la disutilidad por año de los sobrevivientes
        añosVidaConDiscapacidad <- añosVidaConDiscapacidad + discapacitados * parametros$uSifilisCongenita
        
        #Sumamos la disutilidad descontada por año de los sobrevivientes, si estamos mirando el primer año no descuenta porque la potencia de algo a la 0 es 1.
        añosVidaConDiscapacidadDesc <- añosVidaConDiscapacidadDesc + descontarValorCiclo(discapacitados * parametros$uSifilisCongenita, parametros$pDescuento, t - 1)
      }
    }
    
    #Tenemos los DALYS por sifilis congenita
    dalysSifilisCongenita <-  añosVidaConDiscapacidad
    dalysSifilisCongenitaDesc <- añosVidaConDiscapacidadDesc 

    
    
    #Calculamos los años de vida perdidos por muerte prematura (mortinato, neonatales y por SC)
    añosVidaPerdidos <- añosVidaPerdidosSC + añosVidaPerdidosNeontales
    añosVidaPerdidosDesc <- añosVidaPerdidosSCDesc + añosVidaPerdidosNeonatalesDesc

    #sumamos las muertes
    muertes <- muertesSC + eventos$mortinatos + eventos$muertesNeonatales
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Estimamos costos%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    costosTesteo = nGestantesTesteadas * cCostoTest
    
    costosTratamiento <- nGestantesTratadas * parametros$cPeniBenza * 3
    
    costosDiagnosticosNacimiento <- (
      (eventos$seguimientos * (parametros$cDxNeonatoSospechoso + parametros$cLNTP * 6)) +
      (eventos$sifilisCongenitas + eventos$sifilisCongenitasAsin) * (parametros$cDxNeonatoCS + parametros$cLNTP * 6) +
      (eventos$seguimientosTratados * parametros$cLNTP * 6)
    )
    
    #Aquellos con evaluacion anormal o sintomaticos reciben 7 dias de peni sodica a 50.000ui/kg cada 12 seguidos de 50.000ui/kg cada 8 por 3 dias
    costosTratamientoNacimiento <- (
      #La ampolla viene de 2.400.000 UI, la dosis es 50.000UI/KG 
      (eventos$seguimientos * (parametros$cPeniBenza / 2400000 * 50000 * parametros$nPesoSC)) +
      (eventos$sifilisCongenitas + eventos$sifilisCongenitasAsin) * (parametros$cPeniSodica / 2 * (2 * 7 + 3 * 3) * parametros$nPesoSC)
    )
    
    costosHospitalizacion <- (eventos$sifilisCongenitas + eventos$sifilisCongenitasAsin) * parametros$tDiasHospSifilisCongenita * parametros$cDiaHospNeo
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    return(c(
      gestantesTesteadas = nGestantesTesteadas,
      gestantesTratadpas = nGestantesTratadas,
      VPTratados = sifilisGestacionalesAtendidas * pTesteo * sensibilidad * pTratamiento,
      FPTratados = (nacimientosAtendidos - sifilisGestacionalesAtendidas) * pTesteo * (1 - especificidad) * pTratamiento,
      eventos,
      costosTesteo = costosTesteo,
      costosTratamiento = costosTratamiento,
      costosDiagnosticosNacimiento = costosDiagnosticosNacimiento,
      costosTratamientoNacimiento = costosTratamientoNacimiento,
      costosHospNacimiento = costosHospitalizacion,
      muertes = muertes,
      añosVidaPerdidos = añosVidaPerdidos,
      añosVidaPerdidosDesc = añosVidaPerdidosDesc,
      dalysSifilisCongenita = dalysSifilisCongenita,
      dalysSifilisCongenitaDesc = dalysSifilisCongenitaDesc
      ))
      

    
  }

  #Inicia correr modelo
  
  
  #  Ajustamos nacimientos dada la tasa de mortinatos
  nNacimientosTotales <- parametros$nNacimientos / ((1000 - parametros$rMortinato) / 1000)
  

  #nNacimientosTotales <- parametros$nNacimientos
  
  # Cuantos de esos nacimientos son atendidos
  nNacimientosAtendidos <- nNacimientosTotales * parametros$pPrenatalCare
  
  # Cuantos nacimientos estan afectados por sifilis gestacional
  nSifilisGestacional <- nNacimientosTotales * parametros$pSifilisGestacional
  
  # Cuantas sifilis gestacionales son atendidas.
  nSifilisGestacionalAtendidas <- nSifilisGestacional * parametros$pPrenatalCare
  

  resultadosBasal <- calcularResultados(parametros$pTesteoSGestacional, parametros$pSGTratadas, parametros$cLNTP + parametros$cLTP, parametros$cPeniBenzatinica, 1, 1, nNacimientosTotales, nNacimientosAtendidos, nSifilisGestacional, nSifilisGestacionalAtendidas, parametros)

  resultadosProyectados <- calcularResultados(parametros$pTesteoSGestacionalObj, parametros$pSGTratadasObj, parametros$cRTP, parametros$cPeniBenzatinica, parametros$pRTPSensibilidad, parametros$pRTPEspecificidad, nNacimientosTotales, nNacimientosAtendidos, nSifilisGestacional, nSifilisGestacionalAtendidas, parametros)
  resultadosProyectados$costoProgramatico <- parametros$cCostoProgramatico
  return(procesarResultados(resultadosBasal, resultadosProyectados))

  
}
#cargarDatos()


cargar <- function(country) {
  data <- read_excel("models/sifilis/data/lparametros.xlsx", sheet = "parametros", col_types = c("text","text","text","text","numeric","skip","skip","skip","skip","skip"))
  datafiltrada <- data[data$Pais %in% c(country, "GLOBAL"), ]
  PARAMETROS <- as.list(datafiltrada$Valor)
  names(PARAMETROS) <- datafiltrada$Parametro  
  return(PARAMETROS)
}

##### para shiny #####

sifilisInputList = function() {
  data <- read_excel("models/sifilis/data/lparametros.xlsx", sheet = "Clasificacion Parametros", col_names = F, )
  colnames(data) = c("grupo","var","label","tipo")
  data$porc = F
  data$porc[substring(data$var,1,1) == "p"] = T
  data
}