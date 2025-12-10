library(dplyr)

load("models/hepC/data/prob.RData")
load("models/hepC/data/datosPais.RData")
load("models/hepC/data/backgroundMortality.RData")
load("models/hepC/data/util.RData")

##### FUNCION CON TRATAMIENTO ####
hepC_conTrat = function (
    input_pais = input$country,
    input_cohorte = input$cohorte,
    input_AtasaDescuento = input$AtasaDescuento,
    input_F0 = input$F0,
    input_F1 = input$F1,
    input_F2 = input$F2,
    input_F3 = input$F3,
    input_F4 = input$F4,
    input_aCostoF0F2 = input$aCostoF0F2,
    input_aCostoF3 = input$aCostoF3,
    input_aCostoF4 = input$aCostoF4,
    input_aCostoDC = input$aCostoDC,
    input_aCostoHCC = input$aCostoHCC,
    input_pSVR = input$pSVR,
    input_tDuracion_Meses = input$tDuracion_Meses,
    input_pAbandono = input$pAbandono,
    input_Costo_Tratamiento = input$Costo_Tratamiento,
    input_Costo_Evaluacion = input$Costo_Evaluacion
) {
  
  pais = input_pais
  edadFinal = 85
  ciclosPorAño = 12
  añoRatio = 1 / ciclosPorAño
  
  
  cohorte = input_cohorte
  AtasaDescuento = input_AtasaDescuento
  
  # Convertimos la tasa de descuento a mensual para los descuentos por ciclo
  tasaDescuento = AtasaDescuento / ciclosPorAño
  
  # EFECTIVIDAD TRATAMIENTO
  pSVR = input_pSVR
  
  # DURACION TTO
  tDuracion_Meses = input_tDuracion_Meses
  
  # PROBABILIDAD DE DISCONTINUACION
  pAbandono = input_pAbandono
  
  # Cargamos probabilidades de transición anual.
  pAF0_F1 = prob$probabilidad[prob$estado=="F0_F1"]
  pAF1_F2 = prob$probabilidad[prob$estado=="F1_F2"]
  pAF2_F3 = prob$probabilidad[prob$estado=="F2_F3"]
  pAF3_F4 = prob$probabilidad[prob$estado=="F3_F4"]
  pAF4_HCC = prob$probabilidad[prob$estado=="F4_HCC"]
  pAF4_DC = prob$probabilidad[prob$estado=="F4_DC"]
  pADC_HCC = prob$probabilidad[prob$estado=="DC_HCC"]
  pADC_LDR = prob$probabilidad[prob$estado=="DC_LDR"]
  pADC1_HCC = prob$probabilidad[prob$estado=="DC1_HCC"]
  pADC1_LDR = prob$probabilidad[prob$estado=="DC1_LDR"]
  pAHCC_LDR = prob$probabilidad[prob$estado=="HCC_LDR"]
  pASVRF4_HCC = prob$probabilidad[prob$estado=="SVRF4_HCC"]
  pASVRF4_DC = prob$probabilidad[prob$estado=="SVRF4_DC"]
  
  # Convertimos las probabilidades en mensuales porque es el tamaño del ciclo.
  pF0_F1 = 1 - exp(-(-log(1 - pAF0_F1 / 1)) * (1 / ciclosPorAño))
  pF1_F2 = 1 - exp(-(-log(1 - pAF1_F2 / 1)) * (1 / ciclosPorAño))
  pF2_F3 = 1 - exp(-(-log(1 - pAF2_F3 / 1)) * (1 / ciclosPorAño))
  pF3_F4 = 1 - exp(-(-log(1 - pAF3_F4 / 1)) * (1 / ciclosPorAño))
  pF4_HCC = 1 - exp(-(-log(1 - pAF4_HCC / 1)) * (1 / ciclosPorAño))
  pF4_DC = 1 - exp(-(-log(1 - pAF4_DC / 1)) * (1 / ciclosPorAño))
  pDC_HCC = 1 - exp(-(-log(1 - pADC_HCC / 1)) * (1 / ciclosPorAño))
  pDC_LDR = 1 - exp(-(-log(1 - pADC_LDR / 1)) * (1 / ciclosPorAño))
  pDC1_HCC = 1 - exp(-(-log(1 - pADC1_HCC / 1)) * (1 / ciclosPorAño))
  pDC1_LDR = 1 - exp(-(-log(1 - pADC1_LDR / 1)) * (1 / ciclosPorAño))
  pHCC_LDR = 1 - exp(-(-log(1 - pAHCC_LDR / 1)) * (1 / ciclosPorAño))
  pSVRF4_DC = 1 - exp(-(-log(1 - pASVRF4_DC / 1)) * (1 / ciclosPorAño))
  pSVRF4_HCC = 1 - exp(-(-log(1 - pASVRF4_HCC / 1)) * (1 / ciclosPorAño))
  
  # Cargamos las UTILIDADES
  uF0a = util$utilidad[util$estado=="F0"]
  uF1a = util$utilidad[util$estado=="F1"]
  uF2a = util$utilidad[util$estado=="F2"]
  uF3a = util$utilidad[util$estado=="F3"]
  uF4a = util$utilidad[util$estado=="F4"]
  uDCa = util$utilidad[util$estado=="DC"]
  uHCCa = util$utilidad[util$estado=="HCC"]
  uF0SVRa = util$utilidad[util$estado=="F0SVR"]
  uF1SVRa = util$utilidad[util$estado=="F1SVR"]
  uF2SVRa = util$utilidad[util$estado=="F2SVR"]
  uF3SVRa = util$utilidad[util$estado=="F3SVR"]
  uF4SVRa = util$utilidad[util$estado=="F4SVR"]
  
  # # Convertimos las utilidades en mensuales ; ESTO PUEDE BORRARSE no se usa.
  uF0 = uF0a / ciclosPorAño
  uF1 = uF1a / ciclosPorAño
  uF2 = uF2a / ciclosPorAño
  uF3 = uF3a / ciclosPorAño
  uF4 = uF4a / ciclosPorAño
  uDC = uDCa / ciclosPorAño
  uHCC = uHCCa / ciclosPorAño
  uF0SVR = uF0SVRa / ciclosPorAño
  uF1SVR = uF1SVRa / ciclosPorAño
  uF2SVR = uF2SVRa / ciclosPorAño
  uF3SVR = uF3SVRa / ciclosPorAño
  uF4SVR = uF4SVRa / ciclosPorAño

  # Cargamos los costos anuales.
  aCostoF0F2 = input_aCostoF0F2
  aCostoF3 = input_aCostoF3
  aCostoF4 = input_aCostoF4
  aCostoDC = input_aCostoDC
  aCostoHCC = input_aCostoHCC
  
  # Calculamos los costos mensuales.
  costoF0F2 = aCostoF0F2 / ciclosPorAño
  CostoF3 = aCostoF3 / ciclosPorAño
  CostoF4 = aCostoF4 / ciclosPorAño
  CostoDC = aCostoDC / ciclosPorAño
  CostoHCC = aCostoHCC / ciclosPorAño 
  
  # Calculamos los costos mensuales
  Costo_Tratamiento = input_Costo_Tratamiento 
  Costo_Evaluacion = input_Costo_Evaluacion
  
  #Obtenemos las probabilidades de muerte general
  ABM = backgroundMortality$backgroundMortality[backgroundMortality$pais==pais]
  BM = backgroundMortality$backgroundMortality[backgroundMortality$pais==pais] / ciclosPorAño
  
  
  # Redimensionar el vector arrayModelo
  
  edadInicio = 45
  inicial_f0 <- input_F0 * 100 * 0.01 * cohorte
  inicial_f1 <- input_F1 * 100 * 0.01 * cohorte
  inicial_f2 <- input_F2 * 100 * 0.01 * cohorte
  inicial_f3 <- input_F3 * 100 * 0.01 * cohorte
  inicial_f4 <- input_F4 * 100 * 0.01 * cohorte
  
  
  
  lengthArray = (edadFinal - edadInicio) * ciclosPorAño + 1
  
  tratados_F0 = inicial_f0 * (1 - pAbandono)
  tratados_F1 = inicial_f1 * (1 - pAbandono)
  tratados_F2 = inicial_f2 * (1 - pAbandono)
  tratados_F3 = inicial_f3 * (1 - pAbandono)
  tratados_F4 = inicial_f4 * (1 - pAbandono)
  costoTTO = Costo_Tratamiento * tDuracion_Meses
  CostoTratar_F0 = (tratados_F0 * costoTTO) + (tratados_F0 * Costo_Evaluacion)
  CostoTratar_F1 = (tratados_F1 * costoTTO) + (tratados_F1 * Costo_Evaluacion)
  CostoTratar_F2 = (tratados_F2 * costoTTO) + (tratados_F2 * Costo_Evaluacion)
  CostoTratar_F3 = (tratados_F3 * costoTTO) + (tratados_F3 * Costo_Evaluacion)
  CostoTratar_F4 = (tratados_F4 * costoTTO) + (tratados_F4 * Costo_Evaluacion)
  SVR_F0 = tratados_F0 * pSVR
  SVR_F1 = tratados_F1 * pSVR
  SVR_F2 = tratados_F2 * pSVR
  SVR_F3 = tratados_F3 * pSVR
  SVR_F4 = tratados_F4 * pSVR
  resSVR = SVR_F0 + SVR_F1 + SVR_F2 + SVR_F3 + SVR_F4
  conTTo_CostoTTO = CostoTratar_F0 + CostoTratar_F1 + CostoTratar_F2 + CostoTratar_F3 + CostoTratar_F4
  # Se quedan en F0 los que fallaron la pSVR y los que discontinuaron.
  
  arrayModelo = data.frame(
    F0 = (inicial_f0 - tratados_F0) + (tratados_F0 - SVR_F0),
    F1 = (inicial_f1 - tratados_F1) + (tratados_F1 - SVR_F1),
    F2 = (inicial_f2 - tratados_F2) + (tratados_F2 - SVR_F2),
    F3 = (inicial_f3 - tratados_F3) + (tratados_F3 - SVR_F3),
    F4 = (inicial_f4 - tratados_F4) + (tratados_F4 - SVR_F4),
    CostoSVRF0F2 = (CostoTratar_F0 + CostoTratar_F1 + CostoTratar_F2),
    
    CostoSVRF3 = CostoTratar_F3,
    CostoSVRF4 = CostoTratar_F4,
    SVRF0F2 = SVR_F0 + SVR_F1 + SVR_F2,
    SVRF3 = SVR_F3,
    SVRF4 = SVR_F4,
    ccAcumulados = inicial_f4,
    ano = edadInicio
  )
  
  arrayModelo$costoF0 = arrayModelo$F0 * costoF0F2
  arrayModelo$costoF1 = arrayModelo$F1 * costoF0F2
  arrayModelo$CostoF2 = arrayModelo$F2 * costoF0F2
  arrayModelo$CostoF3 = arrayModelo$F3 * CostoF3
  arrayModelo$CostoF4 = arrayModelo$F4 * CostoF4
  arrayModelo$DC = 0
  arrayModelo$DC1 = 0
  arrayModelo$HCC = 0
  arrayModelo$CostoCiclo = arrayModelo$costoF0 + arrayModelo$costoF1 + arrayModelo$CostoF2 + arrayModelo$CostoF3 + arrayModelo$CostoF4 + arrayModelo$CostoSVRF0F2 + arrayModelo$CostoSVRF3 + arrayModelo$CostoSVRF4
  arrayModelo$costoAcumulado = arrayModelo$CostoCiclo
  arrayModelo$dCostoAcumulado = arrayModelo$CostoCiclo
  arrayModelo$utilidadCiclo = (arrayModelo$F0 * uF0 + arrayModelo$F1 * uF1 + arrayModelo$F2 * uF2 + arrayModelo$F3 * uF3 + arrayModelo$F4 * uF4) + (arrayModelo$SVRF0F2 * uF0SVR + arrayModelo$SVRF3 * uF3SVR + arrayModelo$SVRF4 * uF4SVR) + (arrayModelo$DC * uDC + arrayModelo$DC1 * uDC + arrayModelo$HCC * uHCC)
  arrayModelo$addDC = 0
  arrayModelo$dcAcumulados = 0
  
  arrayModelo$addDC1 = 0
  
  
  arrayModelo$hccAcumulados = 0
  arrayModelo$LDR = 0
  arrayModelo$BM = 0
  # arrayModelo$utilidadCiclo = 0
  arrayModelo$utilidadAcumuladoDescontado = 0
  arrayModelo$disutilidadAcumuladaDescontado = 0
  arrayModelo$añosVidaDescontados = 0
  arrayModelo$CostoDC <- 0
  arrayModelo$CostoHCC <-  0
  
  add_rows = data.frame(matrix(NA, nrow=lengthArray-1, ncol=ncol(arrayModelo)))
  colnames(add_rows) = colnames(arrayModelo)
  
  arrayModelo = rbind(
    arrayModelo,
    add_rows
  ) %>% as.data.frame()
  
  arrayModelo[is.na(arrayModelo)] = 0
  
  
  arrayModelo$bm_debug = NA
  
  for (i  in 2:lengthArray) {
    arrayModelo$ano[i] = edadInicio + ((i-1) %/% ciclosPorAño) #
    bm_actual = BM[arrayModelo$ano[i - 1]+1] 
    arrayModelo$bm_debug[i] = BM[arrayModelo$ano[i - 1]+1] 
    #print(paste(i," ", bm_actual))
    arrayModelo$F0[i] <- arrayModelo$F0[i - 1] - (arrayModelo$F0[i - 1] * pF0_F1 + arrayModelo$F0[i - 1] * bm_actual)
    arrayModelo$F1[i] <- arrayModelo$F1[i - 1] - (arrayModelo$F1[i - 1] * pF1_F2 + arrayModelo$F1[i - 1] * bm_actual) + arrayModelo$F0[i - 1] * pF0_F1
    arrayModelo$F2[i] <- arrayModelo$F2[i - 1] - (arrayModelo$F2[i - 1] * pF2_F3 + arrayModelo$F2[i - 1] * bm_actual) + arrayModelo$F1[i - 1] * pF1_F2
    arrayModelo$F3[i] <- arrayModelo$F3[i - 1] - (arrayModelo$F3[i - 1] * pF3_F4 + arrayModelo$F3[i - 1] * bm_actual) + arrayModelo$F2[i - 1] * pF2_F3
    arrayModelo$F4[i] <- arrayModelo$F4[i - 1] - (arrayModelo$F4[i - 1] * pF4_DC + arrayModelo$F4[i - 1] * pF4_HCC + arrayModelo$F4[i - 1] * bm_actual) + arrayModelo$F3[i - 1] * pF3_F4
    arrayModelo$ccAcumulados[i] <- arrayModelo$ccAcumulados[i - 1] + arrayModelo$F3[i - 1] * pF3_F4
    arrayModelo$SVRF0F2[i] <- arrayModelo$SVRF0F2[i - 1] - (arrayModelo$SVRF0F2[i - 1] * bm_actual)
    arrayModelo$SVRF3[i] <- arrayModelo$SVRF3[i - 1] - (arrayModelo$SVRF3[i - 1] * bm_actual)
    arrayModelo$SVRF4[i] <- arrayModelo$SVRF4[i - 1] - (arrayModelo$SVRF4[i - 1] * bm_actual + arrayModelo$SVRF4[i - 1] * pSVRF4_DC + arrayModelo$SVRF4[i - 1] * pSVRF4_HCC)
    arrayModelo$addDC[i] <- arrayModelo$F4[i - 1] * pF4_DC + arrayModelo$SVRF4[i - 1] * pSVRF4_DC
    arrayModelo$dcAcumulados[i] <- arrayModelo$dcAcumulados[i - 1] + arrayModelo$addDC[i]
    arrayModelo$DC[i] <- arrayModelo$DC[i - 1] - (arrayModelo$DC[i - 1] * bm_actual + arrayModelo$DC[i - 1] * pDC_LDR + arrayModelo$DC[i - 1] * pDC_HCC) + arrayModelo$addDC[i]
    
    #print(paste(i," ", arrayModelo$DC[i]))
    #browser(expr = {i==100})
    #################################
    
    #Vemos si paso 1 año desde que empezo el modelo
    
    if (i > ciclosPorAño) {
      #browser(expr = {i==14})
      #Restamos a los que ingresaron hace 1 año los que pasaron a BM, LDR o HCC porque los que quedaron van a pasar a DC1.
      arrayModelo$addDC1[i] <- arrayModelo$addDC[i - ciclosPorAño] - (arrayModelo$addDC[i - ciclosPorAño] * (ABM[(arrayModelo$ano[i - ciclosPorAño])+1]) + arrayModelo$addDC[i - ciclosPorAño] * pADC_HCC + arrayModelo$addDC[i - ciclosPorAño] * pADC_LDR)
      
      
      if (i==13) {arrayModelo$addDC1[i]=0} 
      #if (i==15) {browser()} 
      arrayModelo$DC[i] <- arrayModelo$DC[i] - arrayModelo$addDC1[i]
      
    }
    
    if (is.na(arrayModelo$addDC1[i])) {arrayModelo$addDC1[i]=0}
    
    arrayModelo$DC1[i] <- arrayModelo$DC1[i - 1] - (arrayModelo$DC1[i - 1] * pDC1_HCC + arrayModelo$DC1[i - 1] * pDC1_LDR + arrayModelo$DC1[i - 1] * bm_actual) + arrayModelo$addDC1[i]
    arrayModelo$HCC[i] <- arrayModelo$HCC[i - 1] - (arrayModelo$HCC[i - 1] * pHCC_LDR + arrayModelo$HCC[i - 1] * bm_actual) + (arrayModelo$DC1[i - 1] * pDC1_HCC + arrayModelo$F4[i - 1] * pF4_HCC + arrayModelo$DC[i - 1] * pDC_HCC + arrayModelo$SVRF4[i - 1] * pSVRF4_HCC)
    arrayModelo$hccAcumulados[i] <- arrayModelo[i - 1, "hccAcumulados"] + (arrayModelo[i - 1, "DC"] * pDC_HCC + arrayModelo[i - 1, "DC1"] * pDC1_HCC + arrayModelo[i - 1, "F4"] * pF4_HCC + arrayModelo[i - 1, "SVRF4"] * pSVRF4_HCC)
    arrayModelo$LDR[i] <- arrayModelo$LDR[i - 1] + (arrayModelo$HCC[i - 1] * pHCC_LDR + arrayModelo$DC[i - 1] * pDC_LDR + arrayModelo$DC1[i - 1] * pDC1_LDR)
    # Sumamos los muertos desde F0 a F4
    arrayModelo$BM[i] <- arrayModelo$BM[i - 1] + (arrayModelo$F0[i - 1] * bm_actual + arrayModelo$F1[i - 1] * bm_actual + arrayModelo$F2[i - 1] * bm_actual + arrayModelo$F3[i - 1] * bm_actual + arrayModelo$F4[i - 1] * bm_actual + arrayModelo$SVRF0F2[i - 1] * bm_actual + arrayModelo$SVRF3[i - 1] * bm_actual + arrayModelo$SVRF4[i - 1] * bm_actual)
    # Sumamos a lo que hicimos recién los muertos de DC, DC1 y HCC. Lo hago separado solo por claridad
    arrayModelo$BM[i] <- arrayModelo$BM[i] + (arrayModelo$DC[i - 1] * bm_actual + arrayModelo$DC1[i - 1] * bm_actual + arrayModelo$HCC[i - 1] * bm_actual)
    #arrayModelo$utilidadCiclo[i] <- (arrayModelo$F0[i] * uF0 + arrayModelo$F1[i] * uF1 + arrayModelo$F2[i] * uF2 + arrayModelo$F3[i] * uF3 + arrayModelo$F4[i] * uF4) + (arrayModelo$SVRF0F2[i] * uF0SVR + arrayModelo$SVRF3[i] * uF3SVR + arrayModelo$SVRF4[i] * uF4SVR) + (arrayModelo$DC[i] * uDC + arrayModelo$DC1[i] * uDC + arrayModelo$HCC[i] * uHCC)
    # Asignación para arrayModelo en R
    arrayModelo$costoF0[i] <- arrayModelo$F0[i] * costoF0F2
    arrayModelo$costoF1[i] <- arrayModelo$F1[i] * costoF0F2
    arrayModelo$CostoF2[i] <- arrayModelo$F2[i] * costoF0F2
    arrayModelo$CostoF3[i] <- arrayModelo$F3[i] * CostoF3
    arrayModelo$CostoF4[i] <- arrayModelo$F4[i] * CostoF4
    arrayModelo$CostoDC[i] <- arrayModelo$DC[i] * CostoDC
    arrayModelo$CostoDC[i] <- arrayModelo$CostoDC[i] + arrayModelo$DC1[i] * CostoDC
    arrayModelo$CostoHCC[i] <- arrayModelo$HCC[i] * CostoHCC
    arrayModelo$CostoCiclo[i] <- arrayModelo$costoF0[i] + arrayModelo$costoF1[i] + arrayModelo$CostoF2[i] + arrayModelo$CostoF3[i] + arrayModelo$CostoF4[i] + arrayModelo$CostoDC[i] + arrayModelo$CostoHCC[i]
    arrayModelo$costoAcumulado[i] <- arrayModelo$costoAcumulado[i - 1] + arrayModelo$CostoCiclo[i]
    arrayModelo$dCostoAcumulado[i] <- arrayModelo$dCostoAcumulado[i - 1] + (arrayModelo$CostoCiclo[i] / ((1 + tasaDescuento) ^ (i-1)))
  }
  
  # primer año 
  i=13
  arrayModelo$utilidadAcumuladoDescontado[i] = ((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa))
  arrayModelo$disutilidadAcumuladaDescontado[i] <- arrayModelo$disutilidadAcumuladaDescontado[i-1] + (((arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
  arrayModelo$añosVidaDescontados[i] <- (arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i])
  arrayModelo$añosVida[i] <- arrayModelo$añosVida[i - 1] + ((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i]))
  
  j=i+11
  arrayModelo$utilidadAcumuladoDescontado[(i+1):j] <- arrayModelo$utilidadAcumuladoDescontado[i]
  arrayModelo$disutilidadAcumuladaDescontado[(i+1):j] <- arrayModelo$disutilidadAcumuladaDescontado[i]
  arrayModelo$añosVidaDescontados[(i+1):j] <- arrayModelo$añosVidaDescontados[i]
  arrayModelo$añosVida[(i+1):j] <- arrayModelo$añosVida[i]
  # resto años
  
  loop = seq(24,nrow(arrayModelo), by=12)
  loop = loop + 1
  loop = loop[loop %in% 12:(max(loop)-12)] 
  
  for (i in loop) {
    print(i)
    # Asignación para arrayModelo$utilidadAcumuladoDescontado
    
    #browser(expr = {i==24} )
    arrayModelo$utilidadAcumuladoDescontado[i] <- 
      arrayModelo$utilidadAcumuladoDescontado[i - 1] + 
      (((arrayModelo$F0[i] * 
           uF0a + 
           arrayModelo$F1[i] * 
           uF1a + 
           arrayModelo$F2[i] * 
           uF2a + 
           arrayModelo$F3[i] * 
           uF3a + 
           arrayModelo$F4[i] * 
           uF4a) + 
          (arrayModelo$SVRF0F2[i] * 
             uF0SVRa + 
             arrayModelo$SVRF3[i] * 
             uF3SVRa + 
             arrayModelo$SVRF4[i] * 
             uF4SVRa) + (
               arrayModelo$DC[i] * 
                 uDCa + 
                 arrayModelo$DC1[i] * 
                 uDCa + 
                 arrayModelo$HCC[i] * 
                 uHCCa)) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
    
    
    arrayModelo$disutilidadAcumuladaDescontado[i] <- arrayModelo$disutilidadAcumuladaDescontado[i-1] + (((arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
    arrayModelo$añosVidaDescontados[i] <- arrayModelo$añosVidaDescontados[i - 1] + (((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i])) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
    arrayModelo$añosVida[i] <- arrayModelo$añosVida[i - 1] + ((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i]))
    arrayModelo$utilidadAcumulado[i] <- arrayModelo$utilidadAcumulado[i - 1] + ((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa))
    arrayModelo$disutilidadAcumulada[i] <- arrayModelo$disutilidadAcumulada[i-1] + (arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))
    
    
    j=i+11
    
    arrayModelo$utilidadAcumuladoDescontado[(i+1):j] <- arrayModelo$utilidadAcumuladoDescontado[i]
    arrayModelo$disutilidadAcumuladaDescontado[(i+1):j] = arrayModelo$disutilidadAcumuladaDescontado[i]
    arrayModelo$añosVidaDescontados[(i+1):j] <- arrayModelo$añosVidaDescontados[i]
    arrayModelo$añosVida[(i+1):j] <- arrayModelo$añosVida[i]
    arrayModelo$utilidadAcumulado[(i+1):j] <- arrayModelo$utilidadAcumulado[i]
    arrayModelo$disutilidadAcumulada[(i+1):j] <- arrayModelo$disutilidadAcumulada[i]
    
    
    if (i==max(loop)) {
      i=nrow(arrayModelo)
      arrayModelo$utilidadAcumuladoDescontado[i] <- arrayModelo$utilidadAcumuladoDescontado[i - 1] + (((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa)) / ((1 + AtasaDescuento) ^ ((i / 12) - 1)))
      arrayModelo$añosVidaDescontados[i] <- arrayModelo$añosVidaDescontados[i - 1] + (((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i])) / ((1 + AtasaDescuento) ^ ((i / 12) - 1)))
      arrayModelo$añosVida[i] <- arrayModelo$añosVida[i - 1] + ((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i]))  
      arrayModelo$utilidadAcumulado[i] <- arrayModelo$utilidadAcumulado[i - 1] + ((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa))
      arrayModelo$disutilidadAcumulada[i] <- arrayModelo$disutilidadAcumulada[i-1] + (arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))
      
    }
    
  }
  
  ultimo = nrow(arrayModelo)
  resLRD <- arrayModelo[ultimo, "LDR"]
  resDC <- arrayModelo[ultimo, "dcAcumulados"]
  resHcc <- arrayModelo[ultimo, "hccAcumulados"]
  resCostoD <- arrayModelo[ultimo, "dCostoAcumulado"]
  resQalyD <- arrayModelo[ultimo, "utilidadAcumuladoDescontado"]
  resLYD <- arrayModelo[ultimo, "añosVidaDescontados"]
  resCosto <- arrayModelo[ultimo, "costoAcumulado"]
  resQaly <- arrayModelo[ultimo, "utilidadAcumulado"]
  resLY <- arrayModelo[ultimo, "añosVida"]
  resCC <- arrayModelo[ultimo, "ccAcumulados"]
  costoTrat <- conTTo_CostoTTO
  
  resultados = list(
    resCC,
    resDC,
    resHcc,
    resLRD,
    resQaly,
    resQalyD,
    resLY,
    resLYD,
    resCosto,
    resCostoD,
    resSVR,
    costoTrat
  )
  
  names(resultados) = c(
    "Cirrosis",
    "Cirrosis Descompensadas",
    "Hepatocarcinomas",
    "Muertes asociadas a Higado",
    "Qalys vividos",
    "Qalys vividos Descontados",
    "Años de vida vividos",
    "Años de vida vividos Descontados",
    "Costos",
    "Costos Descontados",
    "SVR Logradas",
    "Costos Tratamiento"
    
  )
  return(resultados)
}


##### FUNCION SIN TRATAMIENTO #####

hepC_sinTrat = function (
    input_pais = input$country,
    input_cohorte = input$cohorte,
    input_AtasaDescuento = input$AtasaDescuento,
    input_F0 = input$F0,
    input_F1 = input$F1,
    input_F2 = input$F2,
    input_F3 = input$F3,
    input_F4 = input$F4,
    input_aCostoF0F2 = input$aCostoF0F2,
    input_aCostoF3 = input$aCostoF3,
    input_aCostoF4 = input$aCostoF4,
    input_aCostoDC = input$aCostoDC,
    input_aCostoHCC = input$aCostoHCC
) {
  pais = input_pais
  edadFinal = 85
  ciclosPorAño = 12
  añoRatio = 1 / ciclosPorAño
  
  
  cohorte = input_cohorte
  AtasaDescuento = input_AtasaDescuento
  
  # Convertimos la tasa de descuento a mensual para los descuentos por ciclo
  tasaDescuento = AtasaDescuento / ciclosPorAño
  
  # Cargamos probabilidades de transición anual.
  pAF0_F1 = prob$probabilidad[prob$estado=="F0_F1"]
  pAF1_F2 = prob$probabilidad[prob$estado=="F1_F2"]
  pAF2_F3 = prob$probabilidad[prob$estado=="F2_F3"]
  pAF3_F4 = prob$probabilidad[prob$estado=="F3_F4"]
  pAF4_HCC = prob$probabilidad[prob$estado=="F4_HCC"]
  pAF4_DC = prob$probabilidad[prob$estado=="F4_DC"]
  pADC_HCC = prob$probabilidad[prob$estado=="DC_HCC"]
  pADC_LDR = prob$probabilidad[prob$estado=="DC_LDR"]
  pADC1_HCC = prob$probabilidad[prob$estado=="DC1_HCC"]
  pADC1_LDR = prob$probabilidad[prob$estado=="DC1_LDR"]
  pAHCC_LDR = prob$probabilidad[prob$estado=="HCC_LDR"]
  
  # Convertimos las probabilidades en mensuales porque es el tamaño del ciclo.
  pF0_F1 = 1 - exp(-(-log(1 - pAF0_F1 / 1)) * (1 / ciclosPorAño))
  pF1_F2 = 1 - exp(-(-log(1 - pAF1_F2 / 1)) * (1 / ciclosPorAño))
  pF2_F3 = 1 - exp(-(-log(1 - pAF2_F3 / 1)) * (1 / ciclosPorAño))
  pF3_F4 = 1 - exp(-(-log(1 - pAF3_F4 / 1)) * (1 / ciclosPorAño))
  pF4_HCC = 1 - exp(-(-log(1 - pAF4_HCC / 1)) * (1 / ciclosPorAño))
  pF4_DC = 1 - exp(-(-log(1 - pAF4_DC / 1)) * (1 / ciclosPorAño))
  pDC_HCC = 1 - exp(-(-log(1 - pADC_HCC / 1)) * (1 / ciclosPorAño))
  pDC_LDR = 1 - exp(-(-log(1 - pADC_LDR / 1)) * (1 / ciclosPorAño))
  pDC1_HCC = 1 - exp(-(-log(1 - pADC1_HCC / 1)) * (1 / ciclosPorAño))
  pDC1_LDR = 1 - exp(-(-log(1 - pADC1_LDR / 1)) * (1 / ciclosPorAño))
  pHCC_LDR = 1 - exp(-(-log(1 - pAHCC_LDR / 1)) * (1 / ciclosPorAño))
  
  
  # Cargamos las UTILIDADES
  uF0a = util$utilidad[util$estado=="F0"]
  uF1a = util$utilidad[util$estado=="F1"]
  uF2a = util$utilidad[util$estado=="F2"]
  uF3a = util$utilidad[util$estado=="F3"]
  uF4a = util$utilidad[util$estado=="F4"]
  uDCa = util$utilidad[util$estado=="DC"]
  uHCCa = util$utilidad[util$estado=="HCC"]
  uF0SVRa = util$utilidad[util$estado=="F0SVR"]
  uF1SVRa = util$utilidad[util$estado=="F1SVR"]
  uF2SVRa = util$utilidad[util$estado=="F2SVR"]
  uF3SVRa = util$utilidad[util$estado=="F3SVR"]
  uF4SVRa = util$utilidad[util$estado=="F4SVR"]
  
  # Convertimos las utilidades en mensuales ; ESTO PUEDE BORRARSE no se usa.
  uF0 = uF0a / ciclosPorAño
  uF1 = uF1a / ciclosPorAño
  uF2 = uF2a / ciclosPorAño
  uF3 = uF3a / ciclosPorAño
  uF4 = uF4a / ciclosPorAño
  uDC = uDCa / ciclosPorAño
  uHCC = uHCCa / ciclosPorAño
  uF0SVR = uF0SVRa / ciclosPorAño
  uF1SVR = uF1SVRa / ciclosPorAño
  uF2SVR = uF2SVRa / ciclosPorAño
  uF3SVR = uF3SVRa / ciclosPorAño
  uF4SVR = uF4SVRa / ciclosPorAño
  
  # Cargamos los costos anuales.
  aCostoF0F2 = input_aCostoF0F2
  aCostoF3 = input_aCostoF3
  aCostoF4 = input_aCostoF4
  aCostoDC = input_aCostoDC
  aCostoHCC = input_aCostoHCC
  
  # Calculamos los costos mensuales.
  costoF0F2 = aCostoF0F2 / ciclosPorAño
  CostoF3 = aCostoF3 / ciclosPorAño
  CostoF4 = aCostoF4 / ciclosPorAño
  CostoDC = aCostoDC / ciclosPorAño
  CostoHCC = aCostoHCC / ciclosPorAño 
  
  #Obtenemos las probabilidades de muerte general
  ABM = backgroundMortality$backgroundMortality[backgroundMortality$pais==pais]
  BM = backgroundMortality$backgroundMortality[backgroundMortality$pais==pais] / ciclosPorAño
  
  
  # Redimensionar el vector arrayModelo
  
  edadInicio = 45
  
  inicial_f0 <- input_F0 * cohorte
  inicial_f1 <- input_F1 * cohorte
  inicial_f2 <- input_F2 * cohorte
  inicial_f3 <- input_F3 * cohorte
  inicial_f4 <- input_F4 * cohorte
  
  lengthArray = (edadFinal - edadInicio) * ciclosPorAño + 1
  
  arrayModelo = data.frame(
    F0 = inicial_f0,
    F1 = inicial_f1,
    F2 = inicial_f2,
    F3 = inicial_f3,
    F4 = inicial_f4,
    ccAcumulados = inicial_f4,
    ano = edadInicio
  )
  
  arrayModelo$costoF0 = arrayModelo$F0 * costoF0F2
  arrayModelo$costoF1 = arrayModelo$F1 * costoF0F2
  arrayModelo$CostoF2 = arrayModelo$F2 * costoF0F2
  arrayModelo$CostoF3 = arrayModelo$F3 * CostoF3
  arrayModelo$CostoF4 = arrayModelo$F4 * CostoF4
  arrayModelo$CostoCiclo = arrayModelo$costoF0 + arrayModelo$costoF1 + arrayModelo$CostoF2 + arrayModelo$CostoF3 + arrayModelo$CostoF4
  arrayModelo$costoAcumulado = arrayModelo$CostoCiclo
  arrayModelo$dCostoAcumulado = arrayModelo$CostoCiclo
  arrayModelo$addDC = 0
  arrayModelo$dcAcumulados = 0
  arrayModelo$DC = 0
  arrayModelo$addDC1 = 0
  arrayModelo$DC1 = 0
  arrayModelo$HCC = 0
  arrayModelo$hccAcumulados = 0
  arrayModelo$LDR = 0
  arrayModelo$BM = 0
  # arrayModelo$utilidadCiclo = 0
  arrayModelo$utilidadAcumuladoDescontado = 0
  arrayModelo$disutilidadAcumuladaDescontado = 0
  arrayModelo$SVRF0F2 = 0
  arrayModelo$SVRF3 = 0
  arrayModelo$SVRF4 = 0
  arrayModelo$añosVidaDescontados = 0
  arrayModelo$CostoDC <- 0
  arrayModelo$CostoHCC <-  0
  arrayModelo$disutilidadAcumulada <-  0
  
  add_rows = data.frame(matrix(NA, nrow=lengthArray-1, ncol=ncol(arrayModelo)))
  colnames(add_rows) = colnames(arrayModelo)
  
  arrayModelo = rbind(
    arrayModelo,
    add_rows
  ) %>% as.data.frame()
  
  arrayModelo[is.na(arrayModelo)] = 0
  arrayModelo$bm_actual_debug = NA
  
  
  for (i  in 2:lengthArray) {
    
    arrayModelo$ano[i] = edadInicio + ((i-1) %/% ciclosPorAño)
    
    
    arrayModelo$bm_actual_debug[i] = BM[arrayModelo$ano[i - 1]+1] 
    bm_actual = BM[arrayModelo$ano[i - 1]+1] 
    
    
    arrayModelo$F0[i] <- arrayModelo$F0[i - 1] - (arrayModelo$F0[i - 1] * pF0_F1 + arrayModelo$F0[i - 1] * bm_actual)
    
    
    arrayModelo$F1[i] <- arrayModelo$F1[i - 1] - (arrayModelo$F1[i - 1] * pF1_F2 + arrayModelo$F1[i - 1] * bm_actual) + arrayModelo$F0[i - 1] * pF0_F1
    arrayModelo$F2[i] <- arrayModelo$F2[i - 1] - (arrayModelo$F2[i - 1] * pF2_F3 + arrayModelo$F2[i - 1] * bm_actual) + arrayModelo$F1[i - 1] * pF1_F2
    arrayModelo$F3[i] <- arrayModelo$F3[i - 1] - (arrayModelo$F3[i - 1] * pF3_F4 + arrayModelo$F3[i - 1] * bm_actual) + arrayModelo$F2[i - 1] * pF2_F3
    arrayModelo$F4[i] <- arrayModelo$F4[i - 1] - (arrayModelo$F4[i - 1] * pF4_DC + arrayModelo$F4[i - 1] * pF4_HCC + arrayModelo$F4[i - 1] * bm_actual) + arrayModelo$F3[i - 1] * pF3_F4
    arrayModelo$ccAcumulados[i] <- arrayModelo$ccAcumulados[i - 1] + arrayModelo$F3[i - 1] * pF3_F4
    arrayModelo$addDC[i] <- arrayModelo$F4[i - 1] * pF4_DC
    arrayModelo$dcAcumulados[i] <- arrayModelo$dcAcumulados[i - 1] + arrayModelo$addDC[i]
    
    
    arrayModelo$DC[i] <- arrayModelo$DC[i - 1] - (arrayModelo$DC[i - 1] * bm_actual + arrayModelo$DC[i - 1] * pDC_LDR + arrayModelo$DC[i - 1] * pDC_HCC) + arrayModelo$addDC[i]
    
    
    
    #Vemos si paso 1 año desde que empezo el modelo
    
    if (i > ciclosPorAño) {
      #Restamos a los que ingresaron hace 1 año los que pasaron a BM, LDR o HCC porque los que quedaron van a pasar a DC1.
      arrayModelo$addDC1[i] <- arrayModelo$addDC[i - ciclosPorAño] - (arrayModelo$addDC[i - ciclosPorAño] * (ABM[arrayModelo$ano[i - ciclosPorAño]+1]) + arrayModelo$addDC[i - ciclosPorAño] * pADC_HCC + arrayModelo$addDC[i - ciclosPorAño] * pADC_LDR)
      if (i==13) {arrayModelo$addDC1[i]=0} 
      #if (i==15) {browser()} 
      arrayModelo$DC[i] <- arrayModelo$DC[i] - arrayModelo$addDC1[i]
      
    }
    
    if (is.na(arrayModelo$addDC1[i])) {arrayModelo$addDC1[i]=0}
    
    
    arrayModelo$DC1[i] <- arrayModelo$DC1[i - 1] - (arrayModelo$DC1[i - 1] * pDC1_HCC + arrayModelo$DC1[i - 1] * pDC1_LDR + arrayModelo$DC1[i - 1] * bm_actual) + arrayModelo$addDC1[i]
    arrayModelo$HCC[i] <- arrayModelo$HCC[i - 1] - (arrayModelo$HCC[i - 1] * pHCC_LDR + arrayModelo$HCC[i - 1] * bm_actual) + (arrayModelo$DC1[i - 1] * pDC1_HCC + arrayModelo$F4[i - 1] * pF4_HCC + arrayModelo$DC[i - 1] * pDC_HCC)
    arrayModelo$hccAcumulados[i] <- arrayModelo$hccAcumulados[i - 1] + (arrayModelo$DC[i - 1] * pDC_HCC + arrayModelo$DC1[i - 1] * pDC1_HCC + arrayModelo$F4[i - 1] * pF4_HCC)
    arrayModelo$LDR[i] <- arrayModelo$LDR[i - 1] + (arrayModelo$HCC[i - 1] * pHCC_LDR + arrayModelo$DC[i - 1] * pDC_LDR + arrayModelo$DC1[i - 1] * pDC1_LDR)
    arrayModelo$BM[i] <- arrayModelo$BM[i - 1] + (arrayModelo$F0[i - 1] * bm_actual + arrayModelo$F1[i - 1] * bm_actual + arrayModelo$F2[i - 1] * bm_actual + arrayModelo$F3[i - 1] * bm_actual + arrayModelo$F4[i - 1] * bm_actual)
    arrayModelo$BM[i] <- arrayModelo$BM[i] + (arrayModelo$DC[i - 1] * bm_actual + arrayModelo$DC1[i - 1] * bm_actual + arrayModelo$HCC[i - 1] * bm_actual)
    #arrayModelo$utilidadCiclo[i] <- (arrayModelo$F0[i] * uF0 + arrayModelo$F1[i] * uF1 + arrayModelo$F2[i] * uF2 + arrayModelo$F3[i] * uF3 + arrayModelo$F4[i] * uF4) + (arrayModelo$SVRF0F2[i] * uF0SVR + arrayModelo$SVRF3[i] * uF3SVR + arrayModelo$SVRF4[i] * uF4SVR) + (arrayModelo$DC[i] * uDC + arrayModelo$DC1[i] * uDC + arrayModelo$HCC[i] * uHCC)
    # Asignación para arrayModelo en R
    arrayModelo$costoF0[i] <- arrayModelo$F0[i] * costoF0F2
    arrayModelo$costoF1[i] <- arrayModelo$F1[i] * costoF0F2
    arrayModelo$CostoF2[i] <- arrayModelo$F2[i] * costoF0F2
    arrayModelo$CostoF3[i] <- arrayModelo$F3[i] * CostoF3
    arrayModelo$CostoF4[i] <- arrayModelo$F4[i] * CostoF4
    arrayModelo$CostoDC[i] <- arrayModelo$DC[i] * CostoDC
    arrayModelo$CostoDC[i] <- arrayModelo$CostoDC[i] + arrayModelo$DC1[i] * CostoDC
    arrayModelo$CostoHCC[i] <- arrayModelo$HCC[i] * CostoHCC
    arrayModelo$CostoCiclo[i] <- arrayModelo$costoF0[i] + arrayModelo$costoF1[i] + arrayModelo$CostoF2[i] + arrayModelo$CostoF3[i] + arrayModelo$CostoF4[i] + arrayModelo$CostoDC[i] + arrayModelo$CostoHCC[i]
    arrayModelo$costoAcumulado[i] <- arrayModelo$costoAcumulado[i - 1] + arrayModelo$CostoCiclo[i]
    arrayModelo$dCostoAcumulado[i] <- arrayModelo$dCostoAcumulado[i - 1] + (arrayModelo$CostoCiclo[i] / ((1 + tasaDescuento) ^ (i-1)))
  }
  
  
  # primer año 
  i=13
  arrayModelo$utilidadAcumuladoDescontado[i] = ((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa))
  arrayModelo$disutilidadAcumuladaDescontado[i] <- arrayModelo$disutilidadAcumuladaDescontado[i-1] + (((arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
  arrayModelo$añosVidaDescontados[i] <- (arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i])
  arrayModelo$añosVida[i] <- arrayModelo$añosVida[i - 1] + ((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i]))
  arrayModelo$disutilidadAcumulada[i] <- arrayModelo$disutilidadAcumulada[i-1] + (arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))
  
  j=i+11
  arrayModelo$utilidadAcumuladoDescontado[(i+1):j] <- arrayModelo$utilidadAcumuladoDescontado[i]
  arrayModelo$disutilidadAcumuladaDescontado[(i+1):j] <- arrayModelo$disutilidadAcumuladaDescontado[i]
  arrayModelo$añosVidaDescontados[(i+1):j] <- arrayModelo$añosVidaDescontados[i]
  arrayModelo$añosVida[(i+1):j] <- arrayModelo$añosVida[i]
  arrayModelo$disutilidadAcumulada[(i+1):j] <- arrayModelo$disutilidadAcumulada[i]
  
  
  # resto años
  
  loop = seq(24,nrow(arrayModelo), by=12)
  loop = loop + 1
  loop = loop[loop %in% 12:(max(loop)-12)] 
  
  for (i in loop) {
    # Asignación para arrayModelo$utilidadAcumuladoDescontado
    arrayModelo$utilidadAcumuladoDescontado[i] <- arrayModelo$utilidadAcumuladoDescontado[i - 1] + (((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa)) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
    arrayModelo$disutilidadAcumuladaDescontado[i] <- arrayModelo$disutilidadAcumuladaDescontado[i-1] + (((arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
    arrayModelo$añosVidaDescontados[i] <- arrayModelo$añosVidaDescontados[i - 1] + (((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i])) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
    arrayModelo$añosVida[i] <- arrayModelo$añosVida[i - 1] + ((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i]))
    arrayModelo$utilidadAcumulado[i] <- arrayModelo$utilidadAcumulado[i - 1] + ((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa))
    arrayModelo$disutilidadAcumulada[i] <- arrayModelo$disutilidadAcumulada[i-1] + (arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))
    
    j=i+11
    arrayModelo$utilidadAcumuladoDescontado[(i+1):j] <- arrayModelo$utilidadAcumuladoDescontado[i]
    arrayModelo$disutilidadAcumuladaDescontado[(i+1):j] <- arrayModelo$disutilidadAcumuladaDescontado[i]
    arrayModelo$añosVidaDescontados[(i+1):j] <- arrayModelo$añosVidaDescontados[i]
    arrayModelo$añosVida[(i+1):j] <- arrayModelo$añosVida[i]
    arrayModelo$utilidadAcumulado[(i+1):j] <- arrayModelo$utilidadAcumulado[i]
    arrayModelo$disutilidadAcumulada[(i+1):j] <- arrayModelo$disutilidadAcumulada[i]
    
    if (i==max(loop)) {
      i=nrow(arrayModelo)
      arrayModelo$utilidadAcumuladoDescontado[i] <- arrayModelo$utilidadAcumuladoDescontado[i - 1] + (((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa)) / ((1 + AtasaDescuento) ^ ((i / 12) - 1)))
      arrayModelo$disutilidadAcumuladaDescontado[i] <- arrayModelo$disutilidadAcumuladaDescontado[i-1] + (((arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))) / ((1 + AtasaDescuento) ^ (((i-1) / 12) - 1)))
      arrayModelo$añosVidaDescontados[i] <- arrayModelo$añosVidaDescontados[i - 1] + (((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i])) / ((1 + AtasaDescuento) ^ ((i / 12) - 1)))
      arrayModelo$añosVida[i] <- arrayModelo$añosVida[i - 1] + ((arrayModelo$F0[i] + arrayModelo$F1[i] + arrayModelo$F2[i] + arrayModelo$F3[i] + arrayModelo$F4[i]) + (arrayModelo$SVRF0F2[i] + arrayModelo$SVRF3[i] + arrayModelo$SVRF4[i]) + (arrayModelo$DC[i] + arrayModelo$DC1[i] + arrayModelo$HCC[i]))  
      arrayModelo$utilidadAcumulado[i] <- arrayModelo$utilidadAcumulado[i - 1] + ((arrayModelo$F0[i] * uF0a + arrayModelo$F1[i] * uF1a + arrayModelo$F2[i] * uF2a + arrayModelo$F3[i] * uF3a + arrayModelo$F4[i] * uF4a) + (arrayModelo$SVRF0F2[i] * uF0SVRa + arrayModelo$SVRF3[i] * uF3SVRa + arrayModelo$SVRF4[i] * uF4SVRa) + (arrayModelo$DC[i] * uDCa + arrayModelo$DC1[i] * uDCa + arrayModelo$HCC[i] * uHCCa))
      arrayModelo$disutilidadAcumulada[i] <- arrayModelo$disutilidadAcumulada[i-1] + (arrayModelo$DC1[i] * (1 - uDCa)) + (arrayModelo$DC[i] * (1 - uDCa)) + (arrayModelo$HCC[i] * (1 - uHCCa))
    }
    
  }

  ultimo = nrow(arrayModelo)
  resLRD <- arrayModelo[ultimo, "LDR"]
  resDC <- arrayModelo[ultimo, "dcAcumulados"]
  resHcc <- arrayModelo[ultimo, "hccAcumulados"]
  resCostoD <- arrayModelo[ultimo, "dCostoAcumulado"]
  resQalyD <- arrayModelo[ultimo, "utilidadAcumuladoDescontado"]
  resLYD <- arrayModelo[ultimo, "añosVidaDescontados"]
  resCosto <- arrayModelo[ultimo, "costoAcumulado"]
  resQaly <- arrayModelo[ultimo, "utilidadAcumulado"]
  resLY <- arrayModelo[ultimo, "añosVida"]
  resCC <- arrayModelo[ultimo, "ccAcumulados"]
  
  resultados = list(
    resCC,
    resDC,
    resHcc,
    resLRD,
    resQaly,
    resQalyD,
    resLY,
    resLYD,
    resCosto,
    resCostoD,
    resSVR = NA,
    costoTrat = 0
    
  )
  names(resultados) = c(
    "Cirrosis",
    "Cirrosis Descompensadas",
    "Hepatocarcinomas",
    "Muertes asociadas a Higado",
    "Qalys vividos",
    "Qalys vividos Descontados",
    "Años de vida vividos",
    "Años de vida vividos Descontados",
    "Costos",
    "Costos Descontados",
    "SVR",
    "Costos Tratamiento"
  )
  
  return(resultados)
}



##### MODELO FULL #####

hepC_full = function(
    input,
    output,
    input_pais = input$country,
    input_cohorte = input$cohorte,
    input_AtasaDescuento = input$AtasaDescuento,
    input_F0 = input$F0,
    input_F1 = input$F1,
    input_F2 = input$F2,
    input_F3 = input$F3,
    input_F4 = input$F4,
    input_aCostoF0F2 = input$aCostoF0F2,
    input_aCostoF3 = input$aCostoF3,
    input_aCostoF4 = input$aCostoF4,
    input_aCostoDC = input$aCostoDC,
    input_aCostoHCC = input$aCostoHCC,
    input_pSVR = input$pSVR,
    input_tDuracion_Meses = input$tDuracion_Meses,
    input_pAbandono = input$pAbandono,
    input_Costo_Tratamiento = input$Costo_Tratamiento,
    input_Costo_Evaluacion = input$Costo_Evaluacion) {
  
  sinTrat = hepC_sinTrat(
    input_pais,
    input_cohorte,
    input_AtasaDescuento,
    input_F0,
    input_F1,
    input_F2,
    input_F3,
    input_F4,
    input_aCostoF0F2,
    input_aCostoF3,
    input_aCostoF4,
    input_aCostoDC,
    input_aCostoHCC
  )
  conTrat = hepC_conTrat(
    input_pais,
    input_cohorte,
    input_AtasaDescuento,
    input_F0,
    input_F1,
    input_F2,
    input_F3,
    input_F4,
    input_aCostoF0F2 = input$aCostoF0F2,
    input_aCostoF3,
    input_aCostoF4,
    input_aCostoDC,
    input_aCostoHCC,
    input_pSVR,
    input_tDuracion_Meses,
    input_pAbandono,
    input_Costo_Tratamiento,
    input_Costo_Evaluacion
  )
  
  resultados = list(
    "Sin tratamiento" = sinTrat,
    "Con tratamiento" = conTrat
  )
  resultados$Comparacion = list(
    'Cirrosis evitadas (n)' = round(resultados$`Sin tratamiento`$Cirrosis - resultados$`Con tratamiento`$Cirrosis,0),
    'Carcinomas hepatocelulares evitados (n)' = round(resultados$`Sin tratamiento`$Hepatocarcinomas - resultados$`Con tratamiento`$Hepatocarcinomas,0),
    'Muertes evitadas (n)'	= round(resultados$`Sin tratamiento`$`Muertes asociadas a Higado` - resultados$`Con tratamiento`$`Muertes asociadas a Higado`,0)#,
  #  'SVR logradas' = round(resultados$`Con tratamiento`$`SVR Logradas`,0),
  #  'Cirrosis Descompensadas Evitadas' = round(resultados$`Sin tratamiento`$`Cirrosis Descompensadas` - resultados$`Con tratamiento`$`Cirrosis Descompensadas`,0)	
    
    
  )
  
  dalysDiscEvitados = (resultados$`Con tratamiento`$`Qalys vividos` - resultados$`Con tratamiento`$`Años de vida vividos`) - (resultados$`Sin tratamiento`$`Qalys vividos` - resultados$`Sin tratamiento`$`Años de vida vividos`)
  dalysDiscEvitadosDesc = (resultados$`Con tratamiento`$`Qalys vividos Descontados` - resultados$`Con tratamiento`$`Años de vida vividos Descontados`) - (resultados$`Sin tratamiento`$`Qalys vividos Descontados` - resultados$`Sin tratamiento`$`Años de vida vividos Descontados`)
  anosVidaSalvados = resultados$`Con tratamiento`$`Años de vida vividos` - resultados$`Sin tratamiento`$`Años de vida vividos`
  anosVidaSalvadosDesc = resultados$`Con tratamiento`$`Años de vida vividos Descontados` - resultados$`Sin tratamiento`$`Años de vida vividos Descontados`
  diferenciaCostos = resultados$`Con tratamiento`$Costos - resultados$`Sin tratamiento`$Costos
  diferenciaCostosDesc = resultados$`Con tratamiento`$`Costos Descontados` - resultados$`Sin tratamiento`$`Costos Descontados`
  icerSVR = diferenciaCostos/resultados$`Con tratamiento`$SVR 
  icerSVRDesc = diferenciaCostosDesc/resultados$`Con tratamiento`$SVR 
  icerCirrosisEvitada = diferenciaCostos / (resultados$`Sin tratamiento`$Cirrosis - resultados$`Con tratamiento`$Cirrosis)
  icerCirrosisEvitadaDesc = diferenciaCostosDesc / (resultados$`Sin tratamiento`$`Cirrosis` - resultados$`Con tratamiento`$`Cirrosis`)
  icerDCEvitada = diferenciaCostos / (resultados$`Sin tratamiento`$`Cirrosis Descompensadas` - resultados$`Con tratamiento`$`Cirrosis Descompensadas`)
  icerDCEvitadaDesc = diferenciaCostosDesc / (resultados$`Sin tratamiento`$`Cirrosis Descompensadas` - resultados$`Con tratamiento`$`Cirrosis Descompensadas`)
  icerHCCEvitada = diferenciaCostos / (resultados$`Sin tratamiento`$Hepatocarcinomas - resultados$`Con tratamiento`$Hepatocarcinomas)
  icerHCCEvitadaDesc = diferenciaCostosDesc / (resultados$`Sin tratamiento`$Hepatocarcinomas - resultados$`Con tratamiento`$Hepatocarcinomas)
  icerLDREvitada = diferenciaCostos / (resultados$`Sin tratamiento`$`Muertes asociadas a Higado` - resultados$`Con tratamiento`$`Muertes asociadas a Higado`)
  icerLDREvitadaDesc = diferenciaCostosDesc / (resultados$`Sin tratamiento`$`Muertes asociadas a Higado` - resultados$`Con tratamiento`$`Muertes asociadas a Higado`)
  icerQalyEvitada = diferenciaCostos / (resultados$`Sin tratamiento`$`Qalys vividos` - resultados$`Con tratamiento`$`Qalys vividos`)
  icerQalyEvitadaDesc = diferenciaCostosDesc / (resultados$`Sin tratamiento`$`Qalys vividos Descontados` - resultados$`Con tratamiento`$`Qalys vividos Descontados`)
  icerLYEvitada = diferenciaCostos / (resultados$`Sin tratamiento`$`Años de vida vividos` - resultados$`Con tratamiento`$`Años de vida vividos`)
  icerLYEvitadaDesc = diferenciaCostosDesc / (resultados$`Sin tratamiento`$`Años de vida vividos Descontados` - resultados$`Con tratamiento`$`Años de vida vividos Descontados`)
  ROI = (resultados$`Sin tratamiento`$Costos - resultados$`Con tratamiento`$Costos) / resultados$`Con tratamiento`$`Costos Tratamiento`  * 100
  ROIDesc = (resultados$`Sin tratamiento`$`Costos Descontados` - resultados$`Con tratamiento`$`Costos Descontados`) / resultados$`Con tratamiento`$`Costos Tratamiento` * 100
  
  
  resultados$Comparacion[["Años de vida salvados"]] = anosVidaSalvados
  #resultados$Comparacion[["Años de vida salvados (AVS) (descontado)"]] = anosVidaSalvadosDesc
  
  resultados$Comparacion[["Años de Vida Ajustados por Discapacidad evitados"]] = dalysDiscEvitados
  resultados$Comparacion[["Costo total de la intervención (USD)"]] = resultados$`Con tratamiento`$`Costos Tratamiento`
  
  resultados$Comparacion[["Costos evitados atribuibles a la intervención (USD)"]] = resultados$`Sin tratamiento`$Costos - (resultados$`Con tratamiento`$Costos - resultados$`Con tratamiento`$`Costos Tratamiento`)
  
  #resultados$Comparacion[["Años de Vida Ajustados por Discapacidad evitados (AVAD) (descontado)"]] = dalysDiscEvitadosDesc
  
  resultados$Comparacion[["Diferencia de costos respecto al escenario basal (USD)"]] = diferenciaCostos
  resultados$Comparacion[["Razón de costo-efectividad incremental por cirrosis evitada (USD)"]] = icerCirrosisEvitada
  resultados$Comparacion[["Razón de costo-efectividad incremental por vida salvada (USD)"]] = icerLDREvitada
  resultados$Comparacion[["Razón de costo-efectividad incremental por Año de vida salvado (USD)"]] = icerLYEvitada
  resultados$Comparacion[["Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitada (USD)"]] = icerQalyEvitada
  resultados$Comparacion[["Retorno de Inversión (%)"]] = ROI
  
  # resultados$Comparacion[["Diferencia de costos (USD) (descontado)"]] = diferenciaCostosDesc
  # resultados$Comparacion[["ICER por SVR"]] = icerSVR
  # resultados$Comparacion[["ICER por SVR (descontado)"]] = icerSVRDesc
  # resultados$Comparacion[["Razón de costo-efectividad incremental por cirrosis evitada (USD) (descontado)"]] = icerCirrosisEvitadaDesc
  # resultados$Comparacion[["ICER por HCC Evitada"]] = icerHCCEvitada
  # resultados$Comparacion[["ICER por HCC Evitada (descontado)"]] = icerHCCEvitadaDesc
  # resultados$Comparacion[["ICER por DC Evitada"]] = icerDCEvitada
  # resultados$Comparacion[["ICER por DC Evitada (descontado)"]] = icerDCEvitadaDesc
  # resultados$Comparacion[["ICER por LRD Evitada"]] = icerLDREvitada
  # resultados$Comparacion[["ICER por LRD Evitada (descontado)"]] = icerLDREvitadaDesc
  # resultados$Comparacion[["Razón de costo-efectividad incremental por Años de Vida Ajustados por Discapacidad evitada (USD) (descontado)"]] = icerQalyEvitadaDesc
  # resultados$Comparacion[["Razón de costo-efectividad incremental por año de vida salvado por muerte por hepatopatía evitada (USD) (descontado)"]] = icerLYEvitadaDesc
  # resultados$Comparacion[["Retorno de Inversión (%) (descontado)"]] = ROIDesc
  
  return(resultados)
}

