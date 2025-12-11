descontarValor <- function(tasa_descuento_anual,num_periodos) {
  va <- ((1 - (1 + tasa_descuento_anual) ^ (-num_periodos+1)) / tasa_descuento_anual)
  va <- (va + 1) * -1
  return(va)
  
}

hpp = function (pais, 
                usoOxitocina, 
                partos_anuales,
                edad_al_parto,
                partos_institucionales,
                eficacia_Intervencion,
                mortalidad_materna,
                mortalidad_hpp,
                costo_oxitocina,
                pHPP, #Probabilidad de tener una hemorragia post parto.
                pHPP_Severa, #Probabilidad de que esa hemorragia post parto sea severa.
                pHisterectomia, #Probabilidad de tener una histerectomia ante una HPP severa.
                eficaciaOxitocina, #reducción de la probabilidad de HPP ante oxitocina.
                uHisterectomia, #Utilidad de una histerectomia.
                descuento, #Tasa de descuento (INPUT)
                costoIntervencion = 0, #Costo de la intervención  (INPUT)
                base
) {
  if (base) {
    usoOxitocina = round(usoOxitocina * partos_institucionales,3)
  }
  # Carga información de países
  # load("hpp/data/datosPais.RData")
  # write.xlsx(datosPais,file="hpp/data/datosPais.xlsx")
  datosPais = read_xlsx("models/hpp/data/datosPais.xlsx")
  
  # filtra por país seleccionado
  datosPais = datosPais[datosPais$pais==pais,]
  
  #Calculamos el costo promedio de un caso de HPP ponderado por el porcentaje de casos severos y no severos.
  costoHPPNoSevera = datosPais$value[datosPais$indicador == 'COSTO.mHPP']
  costoHPPSevera = datosPais$value[datosPais$indicador == 'COSTO.sHPP']
  Costo_HPP = (costoHPPNoSevera * (1 - pHPP_Severa) + costoHPPSevera * pHPP_Severa)
  
  #Calculamos los dalys por muerte prematura.
  #Calculamos la perdida de qalys promedio de un caso de HPP ponderado por la probabilidad de muerte y de histerectomia (dado que el caso sea severo).
  #para eso estimamos la probabilidad actual de HPP del pais que estara determinado por la probabilidad de HPP con y sin oxitocina ponderado por el uso de oxitocina basal del pais
  
  pHPPCountry = (usoOxitocina * eficaciaOxitocina * pHPP) + ((1 - usoOxitocina) * pHPP)
  #Estimamos el numero de HPP para el pais
  numeroPartos = partos_anuales 
  nHPPCountry = pHPPCountry * numeroPartos 
  
  #Calculamos el Case Fatality Ratio.
  mortalidadMaterna = mortalidad_materna
  propMortalidadHPP = mortalidad_hpp
  pCFR = (mortalidadMaterna * propMortalidadHPP) / (100000 * pHPPCountry)
  
  
  
  #Calculamos la cantidad de años descontados desde la edad al parto y la expectativa de vida.
  expectativaAlParto = datosPais$value[datosPais$indicador == "EXPECTATIVA.DE.VIDA.A.LA.EDAD.DE.PARTO"]  
  edadAlParto = edad_al_parto
  añosDescontados = descontarValor(descuento, (expectativaAlParto - edadAlParto))*-1
  
  
  #Calculamos el valor descontado de qalys perdidos por una histerectomia
  Dalys_Histerectomia = (1 - uHisterectomia) * añosDescontados
  Dalys_HisterectomiaNoD = (1 - uHisterectomia) * (datosPais$value[datosPais$indicador=="EXPECTATIVA.DE.VIDA.A.LA.EDAD.DE.PARTO"] - datosPais$value[datosPais$indicador=="EDAD.AL.PARTO"])
  
  #Dalys por muerte prematuro al valer 0 la utilidad se pierde 1 por año asique es igual al valor de años descontados.
  Dalys_MuertePrematura = añosDescontados
  Dalys_MuertePrematuraNoD = (datosPais$value[datosPais$indicador=='EXPECTATIVA.DE.VIDA.A.LA.EDAD.DE.PARTO'] - datosPais$value[datosPais$indicador=='EDAD.AL.PARTO'])
  
  #Dalys por histerectomia.
  Dalys_Total = Dalys_Histerectomia * (pHPP_Severa * pHisterectomia) + (Dalys_MuertePrematura * pCFR)
  
  ####################
  
  ramaSecundaria = function (poblacion, pHPP) {
    list (
      poblacion = poblacion,
      pHPP = pHPP,
      noHPP = poblacion * (1 - pHPP),
      siHPP = poblacion * pHPP
    )
  }
  
  
  
  # función almacena datos de corrida
  
  rama = function (pUsoOxitocina, poblacion, pHPP, eficaciaOxitocina) {
    list(
      usoOxitocina = pUsoOxitocina,
      poblacion = poblacion,
      pHPP = pHPP,
      eficaciaOxitocina = eficaciaOxitocina,
      costoHPP = 0,
      qalyLost = 0,
      costoIntervencion = costoIntervencion,
      recibeOxitocina = ramaSecundaria(poblacion * pUsoOxitocina, pHPP * eficaciaOxitocina),
      noRecibeOxitocina = ramaSecundaria(poblacion * (1-pUsoOxitocina), pHPP)
    )
  }
  
  if (usoOxitocina == datosPais$value[datosPais$indicador=="USO.DE.OXITOCINA"]) {
    runRama = rama(usoOxitocina,numeroPartos,pHPP,eficaciaOxitocina)
  } else {
    runRama = rama(usoOxitocina * partos_institucionales,numeroPartos, pHPP, eficaciaOxitocina)
  }
  
  # setea parámetros escenario
  
  setearCostos = function(costoHPP, costoOxitocina) {
    list(
      costoHPP = costoHPP,
      costoOxitocina = costoOxitocina
    )
  }
  
  runSetearCostos = setearCostos(Costo_HPP,costo_oxitocina)
  
  setearCostoIntervencion = function(costoIntervencion) {
    list(
      costoIntervencion = costoIntervencion
    )
  }  
  
  
  runSetearCostoIntervencion = setearCostoIntervencion(costoIntervencion)
  
  
  setearQalyLost = function(qalyLost) {
    list(
      qalyLost = qalyLost
    )
  }
  
  runSetearQalyLost = setearQalyLost(Dalys_Total)    
  
  # Calcula indicadores para resultados
  getCosto = function() {
    (runRama$recibeOxitocina$siHPP * runSetearCostos$costoHPP + runRama$noRecibeOxitocina$siHPP * runSetearCostos$costoHPP) + (runSetearCostos$costoOxitocina * runRama$poblacion * runRama$usoOxitocina) + runSetearCostoIntervencion[[1]]
  }
  
  getCostoMedico = function() {
    runRama$recibeOxitocina$siHPP * runSetearCostos$costoHPP + runRama$noRecibeOxitocina$siHPP * runSetearCostos$costoHPP
  }
  
  getQalyLost = function() {
    runRama$recibeOxitocina$siHPP * runSetearQalyLost$qalyLost + runRama$noRecibeOxitocina$siHPP * runSetearQalyLost$qalyLost
  }
  
  
  getHPP = function() {
    runRama$recibeOxitocina$siHPP + runRama$noRecibeOxitocina$siHPP
  }
  
  getMuertes = function() {
    runRama$recibeOxitocina$siHPP + runRama$noRecibeOxitocina$siHPP * pCFR
  }
  
  # Resultados corrida
  resultados = list(
    costoIntervencion = costoIntervencion,
    Dalys_Histerectomia = Dalys_Histerectomia,
    Dalys_HisterectomiaNoD = Dalys_HisterectomiaNoD,
    mHPP = getMuertes(),
    hpp = getHPP(),
    nHPPCountry = nHPPCountry,
    pCFR = pCFR,
    QalyLost=getQalyLost(),
    Costo_HPP = Costo_HPP,
    Dalys_Total = Dalys_Total,
    pHPP_Severa=pHPP_Severa,
    pHisterectomia=pHisterectomia,
    Dalys_MuertePrematura=Dalys_MuertePrematura,
    Dalys_MuertePrematuraNoD=Dalys_MuertePrematuraNoD,
    getCosto = getCosto(),
    getCostoMedico = getCostoMedico(),
    poblacionAfectada = 
      if (base) {
        partos_anuales * usoOxitocina   
      } else {usoOxitocina * partos_institucionales * partos_anuales}
    
  )
  return(resultados)
}


resultados_comparados = function(pais, 
                                 usoOxitocina_base,
                                 usoOxitocina_target,
                                 partos_anuales,
                                 edad_al_parto,
                                 partos_institucionales,
                                 eficacia_Intervencion,
                                 mortalidad_materna,
                                 mortalidad_hpp,
                                 costo_oxitocina,
                                 pHPP, #Probabilidad de tener una hemorragia post parto.
                                 pHPP_Severa, #Probabilidad de que esa hemorragia post parto sea severa.
                                 pHisterectomia, #Probabilidad de tener una histerectomia ante una HPP severa.
                                 eficaciaOxitocina, #reducción de la probabilidad de HPP ante oxitocina.
                                 uHisterectomia, #Utilidad de una histerectomia.
                                 descuento, #Tasa de descuento (INPUT)
                                 costoIntervencion = 0 #Costo de la intervención  (INPUT)
                                 ) {
  load("models/hpp/data/datosPais.RData")
  datosPais = datosPais[datosPais$pais==pais,]
  
  base = hpp(
    pais, 
    usoOxitocina_base, 
    partos_anuales,
    edad_al_parto,
    partos_institucionales,
    eficacia_Intervencion,
    mortalidad_materna,
    mortalidad_hpp,
    costo_oxitocina,
    pHPP, #Probabilidad de tener una hemorragia post parto.
    pHPP_Severa, #Probabilidad de que esa hemorragia post parto sea severa.
    pHisterectomia, #Probabilidad de tener una histerectomia ante una HPP severa.
    eficaciaOxitocina, #reducción de la probabilidad de HPP ante oxitocina.
    uHisterectomia, #Utilidad de una histerectomia.
    descuento, #Tasa de descuento (INPUT)
    costoIntervencion = 0, #Costo de la intervención  (INPUT)
    base = T
  )
  
  target = hpp(
    pais, 
    usoOxitocina_target, 
    partos_anuales,
    edad_al_parto,
    partos_institucionales,
    eficacia_Intervencion,
    mortalidad_materna,
    mortalidad_hpp,
    costo_oxitocina,
    pHPP, #Probabilidad de tener una hemorragia post parto.
    pHPP_Severa, #Probabilidad de que esa hemorragia post parto sea severa.
    pHisterectomia, #Probabilidad de tener una histerectomia ante una HPP severa.
    eficaciaOxitocina, #reducción de la probabilidad de HPP ante oxitocina.
    uHisterectomia, #Utilidad de una histerectomia.
    descuento, #Tasa de descuento (INPUT)
    costoIntervencion = 0, #Costo de la intervención  (INPUT)
    base = F
  )
  
  comparacionSinDesc = list()
  comparacionSinDesc["Hemorragias posparto evitadas (n)"] = base$hpp-target$hpp
  comparacionSinDesc["Muertes evitadas (n)"] = (base$hpp-target$hpp) * base$pCFR
  comparacionSinDesc["Años de vida salvados"] = -1*base$Dalys_MuertePrematuraNoD * ((target$hpp - base$hpp) * base$pCFR)
  comparacionSinDesc["Años de vida ajustados por discapacidad evitados"] = ((base$Dalys_MuertePrematuraNoD * ((-target$hpp + base$hpp) * base$pCFR)) + (base$Dalys_HisterectomiaNoD * ((-target$hpp + base$hpp) * (base$pHPP_Severa * base$pHisterectomia))))
  comparacionSinDesc["Diferencia de costos respecto al escenario basal (USD)"] = target$getCosto - base$getCosto
  comparacionSinDesc["Razón de costo-efectividad incremental por año de vida salvado (USD)"] = comparacionSinDesc$`Diferencia de costos respecto al escenario basal (USD)` / (base$Dalys_MuertePrematuraNoD * ((base$hpp - target$hpp) * base$pCFR))
  comparacionSinDesc["Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)"] = comparacionSinDesc[["Diferencia de costos respecto al escenario basal (USD)"]]  / comparacionSinDesc[["Años de vida ajustados por discapacidad evitados"]]
  comparacionSinDesc["Costo total de la intervención (USD)"] = -1 * (costoIntervencion + ((base$poblacionAfectada - target$poblacionAfectada) * costo_oxitocina))
  comparacionSinDesc["Retorno de Inversión (%)"] = -comparacionSinDesc$`Diferencia de costos respecto al escenario basal (USD)` / comparacionSinDesc$`Costo total de la intervención (USD)` * 100
  comparacionSinDesc["Costos evitados atribuibles a la intervención (USD)"] = base$getCostoMedico - target$getCostoMedico
  comparacionSinDesc["Razón de costo-efectividad incremental por vida salvada (USD)"] = (comparacionSinDesc$`Diferencia de costos respecto al escenario basal (USD)` / comparacionSinDesc$`Muertes evitadas (n)`)
  
  comparacionDesc = list()
  comparacionDesc["Hemorragias posparto evitadas (n)"] = NA
  comparacionDesc["Muertes evitadas (n)"] = NA
  comparacionDesc["Años de vida salvados"] = base$Dalys_MuertePrematura * ((base$hpp - target$hpp) * base$pCFR)
  comparacionDesc["Años de vida ajustados por discapacidad evitados"] = (base$Dalys_Histerectomia * ((-target$hpp + base$hpp) * (base$pHPP_Severa * base$pHisterectomia))) + (base$Dalys_MuertePrematura * ((-target$hpp + base$hpp) * base$pCFR))
  comparacionDesc["Costo total de la intervención (USD)"] = -1 * (costoIntervencion + ((base$poblacionAfectada - target$poblacionAfectada) * costo_oxitocina))
  comparacionDesc["Diferencia de costos respecto al escenario basal (USD)"] = target$getCosto - base$getCosto
  comparacionDesc["Razón de costo-efectividad incremental por año de vida salvado (USD)"] = comparacionSinDesc[["Diferencia de costos respecto al escenario basal (USD)"]] / (base$Dalys_MuertePrematura * ((-target$hpp + base$hpp) * base$pCFR))
  comparacionDesc["Razón de costo-efectividad incremental por Año de Vida Ajustado por Discapacidad evitado (USD)"] = comparacionSinDesc[["Diferencia de costos respecto al escenario basal (USD)"]] / comparacionDesc[["Años de vida ajustados por discapacidad evitados"]]

  comparacionDesc["Retorno de Inversión (%)"] = -comparacionDesc$`Diferencia de costos respecto al escenario basal (USD)` / comparacionDesc$`Costo total de la intervención (USD)`* 100
  comparacionDesc["Costos evitados atribuibles a la intervención (USD)"] = base$getCostoMedico - target$getCostoMedico
  comparacionDesc["Razón de costo-efectividad incremental por vida salvada (USD)"] = (comparacionDesc$`Diferencia de costos respecto al escenario basal (USD)` / comparacionSinDesc$`Muertes evitadas (n)`)
  
  comparacion = data.frame(
    indicador = names(comparacionSinDesc),
    valor = unlist(comparacionSinDesc),
    valor_desc = unlist(comparacionDesc)
  )
  
  return(comparacion)
  
}

# resultados_comparados(
  # pais = "Argentina",
  # usoOxitocina_base = 0.711,
  # usoOxitocina_target = 0.80087,
  # partos_anuales = 752059,
  # edad_al_parto = 28.7,
  # partos_institucionales = 0.995,
  # eficacia_Intervencion = 0.30230,
  # mortalidad_materna = 44.9,
  # mortalidad_hpp = 0.1,
  # costo_oxitocina = 2.97,
  # pHPP = 0.108, #Probabilidad de tener una hemorragia post parto.
  # pHPP_Severa = 0.1759, #Probabilidad de que esa hemorragia post parto sea severa.
  # pHisterectomia = 0.03, #Probabilidad de tener una histerectomia ante una HPP severa.
  # eficaciaOxitocina = 0.51, #reducción de la probabilidad de HPP ante oxitocina.
  # uHisterectomia = 0.985, #Utilidad de una histerectomia.
  # descuento = 0.05, #Tasa de descuento (INPUT)
  # costoIntervencion = 0 #Costo de la intervención  (INPUT)
# )
