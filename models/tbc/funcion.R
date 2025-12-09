library(tidyverse)

inputs_tbc <- readxl::read_excel("models/tbc/data/inputs_tbc.xlsx")

modelo_tbc <- function(pais_seleccionado,
                       VOTrrExito,
                       VOTadherencia,
                       costo_evento_VOT,
                       cantidad_vot_semana,
                       ttoExitoso_Duracion,
                       pExitoso,
                       pFalla,
                       pMuerte,
                       VOTrrFalla,
                       VOTrrMuerte,
                       DOTrrExito,
                       DOTrrFalla,
                       DOTrrMuerte,
                       DOTadherencia,
                       cantidad_dot_semana,
                       mediana_edad_paciente,
                       cohorte,
                       utilidad_pob_gral,
                       disutilidad_tbc_activa,
                       prob_internacion_con_falla,
                       cantidadDiasInternacion,
                       costo_trat_induccion,
                       costo_trat_consolidacion,
                       costo_seguimiento,
                       costo_examenes_complemen,
                       costo_evento_DOT,
                       costo_internacion,
                       costoConsulta,
                       costo_trat_multires_induccion,
                       costo_trat_multires_consolidacion,
                       tasa_descuento_anual,
                       costo_intervencion_vDOT
){
  pais_seleccionado = str_to_title(pais_seleccionado)
  
  inputs_tbc <- readxl::read_excel("models/tbc/data/inputs_tbc.xlsx")
  
  inputs_tbc <- inputs_tbc %>%
    mutate(PAIS = ifelse(PAIS == "GLOBAL", "GLOBAL",  str_to_title(tolower(PAIS))))
  
  inputs_tbc <- inputs_tbc %>%
    mutate(PAIS = case_when(
      PAIS == "México" ~ "Mexico",
      PAIS == "Perú" ~ "Peru",
      PAIS == "Brasil" ~ "Brazil",
      TRUE ~ iconv(PAIS, to = "ASCII//TRANSLIT")
    ))
  
  
  
  ## parametro que no estan en tabla de inputs: 
  
  costo_intervencion_VOT <- 0#Costo programático de vDOT
  
  tasa_descuento_anual <- 0.03 # TASA DE DESCUENTO
  
  Horizonte_temporal_meses <- 12
  
  cantidad_vot_semana <- 5
  cantidad_dot_semana <- 5
  #----------
  #Cantidad de meses que dura la inducción
  Induccion_duracion <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Induccion duracion:") %>%
    select(VALOR) %>%
    as.numeric()
  
  #Costo programático DOT
  costo_intervencion_DOT<- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Costo de la intervención DOT") %>%
    select(VALOR) %>%
    as.numeric()
  
  
  
  
  #Esperanza de vida
  esperanza_vida <-  inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Esperanza de vida:") %>%
    select(VALOR) %>%
    as.numeric()
  
  costo_internacion_falla_terapeu <- costo_internacion*cantidadDiasInternacion 
  
  
  
  ## utilidades-----
  
  # calculo de utilidad_restante_descontada
  
  # Calcula las filas de inicio y fin del rango
  inicio_rango <- mediana_edad_paciente + 1
  
  fin_rango <- esperanza_vida
  
  valores_rango <- inputs_tbc %>%
    filter(PAIS==pais_seleccionado & tipo=="UTILIDADES") %>% 
    mutate(PARAMETRO=as.numeric(PARAMETRO)) %>% 
    filter(PARAMETRO<=fin_rango&PARAMETRO>=inicio_rango)%>%
    select(VALOR) %>% as.vector()
  
  # Calcula el VNA
  vna <- sum(valores_rango$VALOR / (1 + tasa_descuento_anual)^(1:length(valores_rango$VALOR)))
  
  # Suma el valor mediana de utilidad
  utilidad_restante_descontada <- vna +  inputs_tbc %>%
    filter(PAIS==pais_seleccionado & tipo=="UTILIDADES" & PARAMETRO==mediana_edad_paciente) %>%
    select(VALOR) %>% 
    as.numeric()
  
  # calculo de utilidad_restante
  
  # epi demo-----
  utilidad_restante <- sum(valores_rango$VALOR)+  inputs_tbc %>%
    filter(PAIS==pais_seleccionado & tipo=="UTILIDADES" & PARAMETRO==mediana_edad_paciente) %>%
    select(VALOR) %>% 
    as.numeric()
  
  anos_vida_restantes <- esperanza_vida - mediana_edad_paciente
  pago <- 1
  num_periodos <- anos_vida_restantes - 1
  
  # Calcular el Valor Actual (VA), en una fn que intenta aproximar a la VA del excel
  va <- pago * ((1 - (1 + tasa_descuento_anual) ^ (-num_periodos)) / tasa_descuento_anual)
  
  
  # Ajusta el resultado sumando 1
  anos_vida_restantes_descontados <- va + 1
  
  
  #asunciones#-----
  
  pFallaResistencia <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Porcentaje de pacientes con falla debido a multiresistencia") %>%
    select(VALOR) %>%
    as.numeric()
  
  
  fallaDuracion <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Meses que reciben tratamiento el grupo que falla:") %>%
    select(VALOR) %>%
    as.numeric()
  
  
  pFallaAdherencia <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Porcentaje de pacientes con falla debido a mala adherencia") %>%
    select(VALOR) %>%
    as.numeric()
  
  
  
  pFallaReinicia <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Porcentaje de pacientes que ante fallas reinician") %>%
    select(VALOR) %>%
    as.numeric()
    
    
  pReinicia <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Porcentaje que reinicia el tratamiento en el año:") %>%
    select(VALOR) %>%
    as.numeric()
    
  
  ttoPerdido_Duracion <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Duración del tratamiento de los que pierden seguimiento:") %>%
    select(VALOR) %>%
    as.numeric()
    
  
  
  pPerdidoConsulta <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Porcentaje de perdida de tratamiento consulta emergencias:") %>%
    select(VALOR) %>%
    as.numeric()
    
    
  PerdidoCantidadConsultas <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Cantidad de consultas por paciente con perdida de tratamiento:") %>%
    select(VALOR) %>%
    as.numeric()
    
    
  
  GrupoMuerte_Meses_Vividos <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Meses que sobreviven el grupo de pacientes que mueren:") %>%
    select(VALOR) %>%
    as.numeric()
    
    
  
  GrupoMuerte_Meses_Tratados <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Meses que reciben tratamiento el grupo de pacientes que mueren:") %>%
    select(VALOR) %>%
    as.numeric()
    
  
  costo_ponderado_trat_falla <- (pFallaResistencia * ((Induccion_duracion * costo_trat_multires_induccion) + ((12 - fallaDuracion - Induccion_duracion) * costo_trat_multires_consolidacion)) +
                                   pFallaAdherencia * ((Induccion_duracion * costo_trat_induccion + (12 - fallaDuracion - Induccion_duracion) * costo_trat_consolidacion) * pFallaReinicia + 
                                                         (12 - fallaDuracion) * costo_trat_consolidacion * (1 - pFallaReinicia))) / (12 - fallaDuracion)
  
  
  costo_ponderado_trat_falla
  
  
  #comienzo el arbol------
  ##Sat------
  trat_exitoso <- cohorte*pExitoso
  trat_no_exitoso <- cohorte-trat_exitoso
  falla_terapeutica <- trat_no_exitoso*pFalla
  
  muerte <- trat_no_exitoso*pMuerte
  
  perdida_seguimiento <- trat_no_exitoso-(falla_terapeutica +muerte)
  
  ##
  
  costo_a <-trat_exitoso*((costo_trat_induccion*Induccion_duracion)+
                            (costo_trat_consolidacion*(ttoExitoso_Duracion-Induccion_duracion)) +
                            (costo_seguimiento * ttoExitoso_Duracion) +
                            (costo_examenes_complemen * ttoExitoso_Duracion))
  
  utilidad_a <- trat_exitoso*(((12-ttoExitoso_Duracion) * (utilidad_pob_gral/12))+
                                (ttoExitoso_Duracion*((utilidad_pob_gral-disutilidad_tbc_activa)/12)))
  
  ##
  costo_b <- falla_terapeutica * ((((Induccion_duracion * costo_trat_induccion) + ((fallaDuracion-Induccion_duracion) * costo_trat_consolidacion)) +
                                     ((12-fallaDuracion) *costo_ponderado_trat_falla))+
                                    (12 * (costo_examenes_complemen + costo_seguimiento)  ) + (prob_internacion_con_falla * costo_internacion_falla_terapeu ))
  
  
  utilidad_b <- falla_terapeutica*(utilidad_pob_gral-disutilidad_tbc_activa)
  
  
  ##
  costo_c <- perdida_seguimiento*  ( (Induccion_duracion*costo_trat_induccion) +
                                       ((ttoPerdido_Duracion-Induccion_duracion)*costo_trat_consolidacion) +
                                       ( (costo_seguimiento + costo_examenes_complemen)*ttoPerdido_Duracion)    +
                                       ( ((Induccion_duracion*costo_trat_induccion) + 
                                            ((ttoPerdido_Duracion-Induccion_duracion)*costo_trat_consolidacion) +
                                            ( (costo_seguimiento + costo_examenes_complemen)*ttoPerdido_Duracion) ) *pReinicia) +
                                       (pPerdidoConsulta * PerdidoCantidadConsultas * costoConsulta ) )
  
  
  
  utilidad_c <- perdida_seguimiento* (utilidad_pob_gral-disutilidad_tbc_activa)
  
  #
  costo_d <- muerte *((costo_trat_induccion*(Induccion_duracion))+
                        (costo_trat_consolidacion*(GrupoMuerte_Meses_Tratados-Induccion_duracion)) +
                        (costo_seguimiento * GrupoMuerte_Meses_Tratados) +
                        (costo_examenes_complemen * GrupoMuerte_Meses_Tratados))
  
  
  
  utilidad_d <- muerte*((utilidad_pob_gral-disutilidad_tbc_activa)/12)*GrupoMuerte_Meses_Vividos
  
  ##total SAT------------------
  
  costo <- costo_a+costo_b+costo_c+costo_d
  QALYS <- utilidad_a+utilidad_b+utilidad_c+utilidad_d
  AVP <- muerte* anos_vida_restantes
  AVP_ajustados_MP <- muerte*utilidad_restante
  AVP_descontado <- muerte*anos_vida_restantes_descontados
  AVP_ajustados_MP_descontado <- muerte*utilidad_restante_descontada
  AVP_ajustados_discapacidad <- ((utilidad_pob_gral*trat_exitoso)-utilidad_a)+
    ((utilidad_pob_gral*muerte* (6/12))-utilidad_d)+
    ((falla_terapeutica *utilidad_pob_gral)-utilidad_b)+
    ((perdida_seguimiento*utilidad_pob_gral)-utilidad_c)
  
  AVP_ajustados <- AVP_ajustados_discapacidad+AVP_ajustados_MP
  AVP_ajustados_descontado <- AVP_ajustados_MP_descontado+AVP_ajustados_discapacidad
  costo_VOT_DOT <- 0
  
  Años_vida_ajustados_discapacidad <- AVP+AVP_ajustados_discapacidad
  Años_vida_ajustados_discapacidad_descontado <- AVP_descontado+AVP_ajustados_discapacidad
  
  
  ## armo vector
  
  SAT <- c(trat_exitoso,
           trat_exitoso/cohorte,
           round(perdida_seguimiento,2),
           muerte,
           QALYS,
           AVP,
           round(AVP_ajustados,2) ,
           round(AVP_ajustados_descontado,2),
           round(AVP_descontado,2),
           round(costo_VOT_DOT,2),
           "referencia",
           round(costo,2),
           "referencia",
           "referencia",
           "referencia",
           "referencia",
           "referencia",
           "referencia",
           Años_vida_ajustados_discapacidad,
           Años_vida_ajustados_discapacidad_descontado
           
  )
  
  ##DOT--------
  
  
  ##
  trat_exitoso <- (cohorte*pExitoso*DOTadherencia*DOTrrExito) +
    (cohorte*pExitoso*(1-DOTadherencia))
  
  
  ##
  
  trat_no_exitoso <- cohorte-trat_exitoso
  
  falla_terapeutica <- (trat_no_exitoso*pFalla*DOTadherencia*DOTrrFalla)+
    (trat_no_exitoso*pFalla*(1-DOTadherencia))
  
  muerte <- (trat_no_exitoso*pMuerte*DOTadherencia*DOTrrMuerte)+
    (trat_no_exitoso*pMuerte*(1-DOTadherencia))
  
  perdida_seguimiento <- trat_no_exitoso-(falla_terapeutica+muerte)
  
  ##
  
  costo_a <-trat_exitoso*((costo_trat_induccion*Induccion_duracion)+
                            (costo_trat_consolidacion*(ttoExitoso_Duracion-Induccion_duracion)) +
                            (costo_evento_DOT*cantidad_dot_semana*4*ttoExitoso_Duracion) +
                            (costo_seguimiento * ttoExitoso_Duracion)+
                            (costo_examenes_complemen * ttoExitoso_Duracion))
  
  utilidad_a <- trat_exitoso*(((12-ttoExitoso_Duracion) * (utilidad_pob_gral/12))+
                                (ttoExitoso_Duracion*((utilidad_pob_gral-disutilidad_tbc_activa)/12)))
  
  
  ##
  costo_b <- falla_terapeutica * ((((Induccion_duracion * costo_trat_induccion) +
                                      ((fallaDuracion-Induccion_duracion) * costo_trat_consolidacion)) +
                                     ((12-fallaDuracion) *costo_ponderado_trat_falla))+
                                    (12 * (costo_examenes_complemen + costo_seguimiento+(cantidad_dot_semana * 4  *costo_evento_DOT))  ) +
                                    (prob_internacion_con_falla * costo_internacion_falla_terapeu ))
  
  
  utilidad_b <- falla_terapeutica*(utilidad_pob_gral-disutilidad_tbc_activa)
  
  ##
  
  costo_c <- perdida_seguimiento*  ( (Induccion_duracion*costo_trat_induccion) +
                                       ((ttoPerdido_Duracion-Induccion_duracion)*costo_trat_consolidacion) +
                                       ( (costo_seguimiento + costo_examenes_complemen + (4*cantidad_dot_semana*costo_evento_DOT))*ttoPerdido_Duracion)    +
                                       ( ((Induccion_duracion*costo_trat_induccion) + 
                                            ((ttoPerdido_Duracion-Induccion_duracion)*costo_trat_consolidacion) +
                                            ( (costo_seguimiento + costo_examenes_complemen+(4*cantidad_dot_semana*costo_evento_DOT))*ttoPerdido_Duracion) ) *pReinicia) +
                                       (pPerdidoConsulta * PerdidoCantidadConsultas * costoConsulta ) )
  
  
  
  utilidad_c <- perdida_seguimiento* (utilidad_pob_gral-disutilidad_tbc_activa)
  
  
  ##
  costo_d <- muerte *((costo_trat_induccion*(Induccion_duracion))+
                        (costo_trat_consolidacion*
                           (GrupoMuerte_Meses_Tratados-Induccion_duracion)) +
                        (costo_evento_DOT * cantidad_dot_semana*4*GrupoMuerte_Meses_Tratados) +
                        ((costo_examenes_complemen + costo_seguimiento)* GrupoMuerte_Meses_Tratados))
  
  #=($G$32*$G$20*4*GrupoMuerte_Meses_Tratados)+(($G$29+$G$30))
  
  utilidad_d <- muerte*((utilidad_pob_gral-disutilidad_tbc_activa)/12)*GrupoMuerte_Meses_Vividos
  
  ##DOT total
  
  costo <- costo_a+costo_b+costo_c+costo_d
  QALYS <- utilidad_a+utilidad_b+utilidad_c+utilidad_d
  AVP <- muerte* anos_vida_restantes
  AVP_ajustados_MP <- muerte*utilidad_restante
  AVP_descontado <- muerte*anos_vida_restantes_descontados
  AVP_ajustados_MP_descontado <- muerte*utilidad_restante_descontada
  AVP_ajustados_discapacidad <- ((utilidad_pob_gral*trat_exitoso)-utilidad_a)+
    ((utilidad_pob_gral*muerte* (6/12))-utilidad_d)+
    ((falla_terapeutica *utilidad_pob_gral)-utilidad_b)+
    ((perdida_seguimiento*utilidad_pob_gral)-utilidad_c)
  
  AVP_ajustados <- AVP_ajustados_discapacidad+AVP_ajustados_MP
  AVP_ajustados_descontado <- AVP_ajustados_MP_descontado+AVP_ajustados_discapacidad
  costo_VOT_DOT <- ((trat_exitoso*ttoExitoso_Duracion) +
                      (falla_terapeutica *12) +
                      (perdida_seguimiento * ttoPerdido_Duracion) +
                      (muerte* GrupoMuerte_Meses_Tratados)) * (4 * costo_evento_DOT * cantidad_dot_semana)
  
  
  
  Años_vida_ajustados_discapacidad <- AVP+AVP_ajustados_discapacidad
  Años_vida_ajustados_discapacidad_descontado <- AVP_descontado+AVP_ajustados_discapacidad
  
  
  ### creo vector
  costos_evitados <- (as.numeric(SAT[12])-(costo-costo_VOT_DOT))
  DOT <- c(trat_exitoso,
           trat_exitoso/cohorte,
           perdida_seguimiento,
           muerte,
           QALYS,
           AVP,
           AVP_ajustados ,
           AVP_ajustados_descontado,
           AVP_descontado,
           costo_VOT_DOT,
           costos_evitados,
           costo,
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[19])-Años_vida_ajustados_discapacidad)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[6])-AVP)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[20])-Años_vida_ajustados_discapacidad_descontado)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[8])-AVP_ajustados_descontado)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[4])-muerte)),
           (costos_evitados-costo_VOT_DOT)/costo_VOT_DOT*100,
           Años_vida_ajustados_discapacidad,
           Años_vida_ajustados_discapacidad_descontado
  )
  DOT
  
  ##VOT--------
  
  ##
  trat_exitoso <- (cohorte*pExitoso*VOTadherencia*DOTrrExito*VOTrrExito) +
    (cohorte*pExitoso*(1-VOTadherencia))
  
  
  ##
  
  trat_no_exitoso <- cohorte-trat_exitoso
  
  falla_terapeutica <- (trat_no_exitoso*pFalla*VOTadherencia*DOTrrFalla*VOTrrFalla)+
    (trat_no_exitoso*pFalla*(1-VOTadherencia))
  
  muerte <- (trat_no_exitoso*pMuerte*VOTadherencia*DOTrrMuerte*VOTrrMuerte)+
    (trat_no_exitoso*pMuerte*(1-VOTadherencia))
  
  perdida_seguimiento <- trat_no_exitoso-(falla_terapeutica+muerte)
  
  
  ##
  
  costo_a <-trat_exitoso*((costo_trat_induccion*Induccion_duracion)+
                            (costo_trat_consolidacion*(ttoExitoso_Duracion-Induccion_duracion)) +
                            (costo_evento_VOT*cantidad_dot_semana*4*ttoExitoso_Duracion) +
                            (costo_seguimiento * ttoExitoso_Duracion)+
                            (costo_examenes_complemen * ttoExitoso_Duracion))
  
  utilidad_a <- trat_exitoso*(((12-ttoExitoso_Duracion) * (utilidad_pob_gral/12))+(ttoExitoso_Duracion*((utilidad_pob_gral-disutilidad_tbc_activa)/12)))
  
  
  ##
  costo_b <- falla_terapeutica * ( (((Induccion_duracion * costo_trat_induccion) +
                                      ((fallaDuracion-Induccion_duracion) * 
                                         costo_trat_consolidacion)) +
                                     ((12-fallaDuracion) *costo_ponderado_trat_falla))+
                                    
                                    (12 * (costo_examenes_complemen + costo_seguimiento+
                                    (cantidad_dot_semana * 4  *costo_evento_VOT))) +
                                    (prob_internacion_con_falla *
                                       costo_internacion_falla_terapeu ))
  
  
  utilidad_b <- falla_terapeutica*(utilidad_pob_gral-disutilidad_tbc_activa)
  
  ##
  
  costo_c <- perdida_seguimiento* ( (Induccion_duracion*costo_trat_induccion) +
                                       ((ttoPerdido_Duracion-Induccion_duracion)*
                                          costo_trat_consolidacion) +
                                       ( (costo_seguimiento + costo_examenes_complemen +
                                            (4*cantidad_dot_semana*costo_evento_VOT))*
                                           ttoPerdido_Duracion)    +
                                       ( ((Induccion_duracion*costo_trat_induccion) + 
                                            ((ttoPerdido_Duracion-Induccion_duracion)*
                                               costo_trat_consolidacion) +
                                            ( (costo_seguimiento + 
                                                 costo_examenes_complemen+
                                                 (4*cantidad_dot_semana*costo_evento_VOT))*
                                                ttoPerdido_Duracion) ) *pReinicia) +
                                       (pPerdidoConsulta * PerdidoCantidadConsultas * costoConsulta ) )
  
  
  
  utilidad_c <- perdida_seguimiento* (utilidad_pob_gral-disutilidad_tbc_activa)
  
  
  ##
  
  costo_d <- muerte *((costo_trat_induccion*Induccion_duracion)+
                        (costo_trat_consolidacion*(GrupoMuerte_Meses_Tratados-Induccion_duracion))+
                        (costo_evento_VOT*cantidad_vot_semana*4*GrupoMuerte_Meses_Tratados) + 
                        (costo_seguimiento * GrupoMuerte_Meses_Tratados) + 
                        (costo_examenes_complemen * GrupoMuerte_Meses_Tratados))
  
  
  
  utilidad_d <- muerte*((utilidad_pob_gral-disutilidad_tbc_activa)/
                          12)*GrupoMuerte_Meses_Vividos
  
  ##VOT total ------
  
  costo <- costo_a+costo_b+costo_c+costo_d
  QALYS <- utilidad_a+utilidad_b+utilidad_c+utilidad_d
  AVP <- muerte* anos_vida_restantes
  AVP_ajustados_MP <- muerte*utilidad_restante
  AVP_descontado <- muerte*anos_vida_restantes_descontados
  AVP_ajustados_MP_descontado <- muerte*utilidad_restante_descontada
  AVP_ajustados_discapacidad <- ((utilidad_pob_gral*trat_exitoso)-utilidad_a)+
    ((utilidad_pob_gral*muerte* (6/12))-utilidad_d)+
    ((falla_terapeutica *utilidad_pob_gral)-utilidad_b)+
    ((perdida_seguimiento*utilidad_pob_gral)-utilidad_c)
  
  AVP_ajustados <- AVP_ajustados_discapacidad+AVP_ajustados_MP
  AVP_ajustados_descontado <- AVP_ajustados_MP_descontado+
    AVP_ajustados_discapacidad
  costo_VOT_DOT <- ((trat_exitoso*ttoExitoso_Duracion) +
                      (falla_terapeutica *12) +
                      (perdida_seguimiento * ttoPerdido_Duracion) +
                      (muerte* GrupoMuerte_Meses_Tratados)) * (4 * costo_evento_VOT * cantidad_vot_semana)
  
  Años_vida_ajustados_discapacidad <- AVP+AVP_ajustados_discapacidad
  Años_vida_ajustados_discapacidad_descontado <- AVP_descontado+AVP_ajustados_discapacidad
  
  ### creo vector
  costos_evitados <- (as.numeric(SAT[12])-(costo-costo_VOT_DOT))
  
  VOT <- c(trat_exitoso,
           trat_exitoso/cohorte,
           perdida_seguimiento,
           muerte,
           QALYS,
           AVP,
           AVP_ajustados ,
           AVP_ajustados_descontado,
           AVP_descontado,
           costo_VOT_DOT,#10
           costos_evitados, 
           costo,
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[19])-Años_vida_ajustados_discapacidad)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[6])-AVP)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[20])-Años_vida_ajustados_discapacidad_descontado)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[8])-AVP_ajustados_descontado)),
           ((costo-as.numeric(SAT[12]))/(as.numeric(SAT[4])-muerte)),
           (costos_evitados-costo_VOT_DOT)/costo_VOT_DOT*100,
           Años_vida_ajustados_discapacidad,
           Años_vida_ajustados_discapacidad_descontado
  )
  
  ### resultados -----
  # Nombres de los parámetros, como se ven en tu tabla.
  parametros <- c("Tratamientos exitosos (n)", 
                  "Porcentaje de éxito (%)",
                  "Perdida de seguimiento", 
                  "Muertes evitadas (n) no se usa",
                  "Años de vida ajustados por calidad",
                  "Años de vida perdidos", 
                  "Años de vida ajustados por calidad perdidos",
                  "Años de vida perdidos descontados",
                  "Años de vida ajustados por calidad perdidos descontados",
                  "Costo total de la intervención (USD)",
                  "Costos evitados atribuibles a la intervención (USD)", 
                  "Costos totales de la estrategia",
                  "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido",
                  "Razon de costo-efectividad incremental por año de vida salvado",
                  "Razon de costo-efectividad incremental por año de vida ajustado por discapacidad prevenido descontado",
                  "Razon de costo-efectividad incremental por año de vida salvado descontado",
                  'Razon de costo-efectividad incremental por vida salvada',
                  "Retorno de Inversión (%)",
                  "Años_vida_ajustados_discapacidad",
                  "Años_vida_ajustados_discapacidad_descontado")
  
  # Crear el dataframe
  resultados <- data.frame(Parametro=parametros, SAT=SAT, DOT=DOT, VOT=VOT)
  
  ###agrego caluclo de
  ##Años de vida ajustados por discapacidad descontados prevenidos
  ##Años de vida ajustados por discapacidad descontados prevenidos
  ##Diferencia de costos		
  # Muertes prevenidas		
  # Años de vida salvados		
  # Años de vida salvados descontados		
  
  resultados$SAT <- as.numeric(resultados$SAT)
  new_rows <- data.frame(
    Parametro = c("Años de vida ajustados por discapacidad evitados",
                  "Años de vida ajustados por discapacidad descontados evitados",
                  "Diferencia de costos respecto al escenario basal (USD)",
                  "Muertes evitadas (n)",
                  "Años de vida salvados",
                  "Años de vida salvados descontados"),
    SAT = c(0, 0, 0, 0, 0, 0),
    DOT = c(resultados$SAT[19]-resultados$DOT[19],
            resultados$SAT[20]-resultados$DOT[20], 
            resultados$DOT[10]-resultados$DOT[11],
            resultados$SAT[4]-resultados$DOT[4],
            resultados$SAT[6]-resultados$DOT[6],
            resultados$SAT[8]-resultados$DOT[8]),
    VOT = c(resultados$SAT[19]-resultados$VOT[19],
            resultados$SAT[20]-resultados$VOT[20],
            resultados$VOT[10]-resultados$VOT[11],#dif de costos 
            resultados$SAT[4]-resultados$VOT[4],
            resultados$SAT[6]-resultados$VOT[6],
            resultados$SAT[8]-resultados$VOT[8])
  )
  
  
  resultados <- rbind(resultados, new_rows)
  resultados[-1] <- lapply(resultados[-1], function(x) if(is.numeric(x)) round(x, 2) else x)
  
  
  resultados <- resultados[c(1,24,21,25,10,11,23,14,13,18),]
  names(resultados)[4] <- "vDOT"
  return(
    resultados
    )
  
} 

# 
# modelo_tbc(params$pExitoso,
#            params$pMuerte,
#            params$pFalla, 
#            params$DOTrrMuerte,
#            params$DOTrrFalla,
#            params$DOTadherencia,
#            params$DOTrrExito,
#            params$VOTrrExito,
#            params$VOTadherencia,
#            params$VOTrrFalla,
#            params$VOTrrMuerte,
#            input$country,
#            params$cantidad_vot_semana,
#            params$cantidad_dot_semana,
#            datos_paises,
#            asunciones)

get_tbc_params = function (input) {
  pais_seleccionado = input$country
  params = list()
  params$VOTrrExito = 1
  params$VOTadherencia = 0.78
 
  #Costo de un evento vDOT
  params$costo_evento_VOT<- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo de un evento de VOT:") %>%
    select(VALOR) %>%
    as.numeric()
  params$cantidad_vot_semana <- 5
  
  #Cantidad de meses que dura el esquema básico (Inducción + Consolidación)
  params$ttoExitoso_Duracion <- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Duración tratamiento:") %>%
    select(VALOR) %>%
    as.numeric()
  
  params$pExitoso <- 0.661
  params$pFalla <- 0.11
  params$pMuerte <- 0.23
  params$VOTrrFalla <- 1
  params$VOTrrMuerte <- 1
  params$DOTrrExito = 1.14
  params$DOTrrFalla <- 0.49
  params$DOTrrMuerte  <- 0.45
  params$DOTadherencia <- 0.31
  params$cantidad_dot_semana <- 5
  #Mediana de edad de Tuberculosis
  params$mediana_edad_paciente <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Mediana de edad de paciente con tuberculosis:") %>%
    select(VALOR) %>%
    as.numeric()
  params$cohorte <- 10000
  #Utilidad de la población general
  params$utilidad_pob_gral <- inputs_tbc %>%
    filter(PAIS==pais_seleccionado & tipo=="UTILIDADES" & PARAMETRO==inputs_tbc %>%
             filter(tipo == "PAISES" & PAIS==pais_seleccionado &
                      PARAMETRO == "Mediana de edad de paciente con tuberculosis:") %>%
             select(VALOR) %>%
             as.numeric()) %>%
    select(VALOR) %>% 
    as.numeric()
  #Disutilidad de la tuberculosis activa
  params$disutilidad_tbc_activa <- 0.33 
  #Porcentaje de pacientes que se internan dada la falla terapeutica
  params$prob_internacion_con_falla <- 0.2
  #Duración de la internación por tuberculosis
  params$cantidadDiasInternacion <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Cantidad de dias promedio de una internación por TBC ante falla") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo tratamiento de inducción
  params$costo_trat_induccion <- inputs_tbc %>%
    filter(tipo == "PAISES" &  PAIS==pais_seleccionado &
             PARAMETRO == "Costo mensual del tratamiento inducción:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo tratamiento de consolidación
  params$costo_trat_consolidacion <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo mensual del tratamiento consolidación:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo de seguimiento
  params$costo_seguimiento <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo mensual de seguimiento:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo de examenes complementarios
  params$costo_examenes_complemen <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo mensual de examenes complementarios:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo de un evento DOT
  params$costo_evento_DOT<- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo de un evento de DOT:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo internación por tuberculosis
  params$costo_internacion<- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo de 1 día de internación:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo de consulta a emergencias
  params$costoConsulta <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo consulta a emergencias:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo tratamiento inducción en MR
  params$costo_trat_multires_induccion <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo de tratamiento multiresistente induccion:") %>%
    select(VALOR) %>%
    as.numeric()
  #Costo tratamiento consolidacion en MR
  params$costo_trat_multires_consolidacion <- inputs_tbc %>%
    filter(tipo == "PAISES" & PAIS==pais_seleccionado &
             PARAMETRO == "Costo de tratamiento multiresistente consolidacion:") %>%
    select(VALOR) %>%
    as.numeric()
  
  params$tasa_descuento_anual <- 0.03
  #Costo programático VDOT
  params$costo_intervencion_vDOT<- inputs_tbc %>%
    filter(tipo == "GLOBAL" &
             PARAMETRO == "Costo de la intervención vDOT") %>%
    select(VALOR) %>%
    as.numeric()
  
  
  params
}

get_tbc_params_labels = function () {
  c(
    'Riesgo relativo tratamiento exitoso tratamiento de observación directa por video vs tratamiento de observación directa',
    'Porcentaje de adherencia a tratamiento de observación directa por video',
    'Costo de una consulta de tratamiento directamente observado por video (USD)',
    'Cantidad de dosis supervisadas mediante tratamiento directamente observado por video por semana',
    'Duración del tratamiento completo de la tuberculosis pulmonar (días)',
    'Porcentaje de tratamiento exitoso mediante tratamiento autosupervisado',
    'Porcentaje de falla terapéutica dado tratamiento no exitoso mediante tratamiento autosupervisado',
    'Porcentaje de muerte dado tratamiento no exitoso mediante tratamiento autosupervisado',
    'Riesgo relativo de falla terapéutica con tratamiento directamente observado por video vs tratamiento directamente observado',
    'Riesgo relativo de muerte mediante tratamiento directamente observado por video vs tratamiento directamente observado',
    'Riesgo Relativo del tratamiento exitoso con tratamiento de observación directa vs tratamiento autoadministrado',
    'Riesgo relativo de falla terapéutica del tratamiento directamente observado vs tratamiento autosupervisado',
    'Riesgo relativo de muerte tratamiento directamente observado vs tratamiento autosupervisado',
    'Porcentaje de adherencia al tratamiento directamente observado',
    'Cantidad de dosis supervisadas mediante tratamiento directamente observado por semana',
    'Mediana de edad de diagnóstico de Tuberculosis',
    'Tamaño de la cohorte (n)',
    'Utilidad de la población general',
    'Disutilidad por tuberculosis activa',
    'Porcentaje de pacientes con falla terapéutica que requieren internación',
    'Duración de la internación por tuberculosis (días)',
    'Costo mensual de tratamiento de inducción (USD)',
    'Costo mensual tratamiento de consolidación (USD)',
    'Costo mensual de seguimiento (USD)',
    'Costo promedio mensual de exámenes complementarios (USD)',
    'Costo de una consulta de tratamiento directamente observado (USD)',
    'Costo promedio de 1 día de internación por tuberculosis (USD)',
    'Costo de consulta a emergencias (USD)',
    'Costo de tratamiento mensual de inducción en tuberculosis multirresistente (USD)',
    'Costo de tratamiento mensual de consolidación en tuberculosis MR (USD)',
    'Tasa de descuento (%)',
    'Costo programático anual de tratamiento directamente observado por video (USD)'
  )
}

get_tbc_hover = function () {
  c(
    "Riesgo relativo de éxito en el tratamiento de la tuberculosis al comparar tratamiento de observación directa por video con el tratamiento de observación directa",
    "Porcentaje que refleja la adherencia adecuada al tratamiento de observación directa por video para la TB, estableciendo que un paciente cumple con el tratamiento de manera correcta si asiste y completa al menos el 80% de las sesiones de medicación supervisadas",
    "Costo de una consulta de supervisión de una dosis de terapia directamente observada por video (USD oficial a tasa de cambio nominal de cada país)",
    "Número total de veces que un paciente con tuberculosis recibe supervisión para la toma de sus medicamentos a través de tratamiento directamente observado por video durante una semana",
    "Período total de tiempo requerido para completar un régimen terapéutico estándar para la tuberculosis pulmonar, que típicamente dura alrededor de 6 meses. Sin embargo, esta duración puede variar dependiendo de la resistencia a los medicamentos, la gravedad de la enfermedad, y otras condiciones médicas del paciente",
    "Porcentaje de pacientes que responden positivamente al tratamiento autosupervisado en comparación con el total de pacientes tratados para tuberculosis",
    "Porcentaje de pacientes que responden negativamente al tratamiento autosupervisado",
    "Porcentaje de pacientes que fallecen tras no responder positivamente al tratamiento auto supervisado para tuberculosis",
    "Riesgo relativo de falla terapéutica en pacientes bajo tratamiento directamente observado por video frente a aquellos que reciben tratamiento directamente observado",
    "Riesgo relativo de mortalidad en pacientes bajo tratamiento directamente observado por video frente a aquellos que reciben tratamiento directamente observado",
    "Riesgo relativo de éxito en el tratamiento de la tuberculosis comparando el tratamiento de observación directa con el tratamiento auto supervisado",
    "Riesgo relativo de falla terapéutica en pacientes bajo tratamiento directamente observado frente a aquellos que gestionan su tratamiento autosupervisado",
    "Riesgo relativo de mortalidad en pacientes bajo tratamiento directamente observado frente a aquellos que gestionan su tratamiento auto supervisado",
    "Porcentaje que refleja la adherencia adecuada al tratamiento directamente observado para tuberculosis, estableciendo que un paciente cumple con el tratamiento de manera correcta si asiste y completa al menos el 80% de las sesiones de medicación supervisadas",
    "Número total de veces que un paciente con tuberculosis recibe supervisión para la toma de sus medicamentos a través de tratamiento directamente observado durante una semana",
    "Mediana de edad de los pacientes con diagnóstico de tuberculosis para el país",
    "Número de personas con tuberculosis pulmonar activa sin comorbilidades que se diagnostican en un año e ingresan en el análisis",
    "Utilidad de la población general para la mediana de edad de tuberculosis",
    "Proporción de reducción en la calidad de vida que experimentan las personas afectadas por la tuberculosis activa",
    "Porcentaje de pacientes que, tras no responder satisfactoriamente al tratamiento contra la tuberculosis, requieren hospitalización",
    "Indica el número promedio de días que un paciente con tuberculosis está hospitalizado tras no responder adecuadamente a un tratamiento inicial",
    "Costo mensual del tratamiento farmacológico de inducción para tuberculosis (USD oficial a tasa de cambio nominal de cada país)",
    "Costo mensual del tratamiento de consolidación para tuberculosis (USD oficial a tasa de cambio nominal de cada país)",
    "Costo mensual de controles médicos (no contempla exámenes complementarios) (USD oficial a tasa de cambio nominal de cada país)",
    "Costo promedio mensual de exámenes complementarios (USD oficial a tasa de cambio nominal de cada país)",
    "Costo de una consulta de supervisión de una dosis de tratamiento directamente observado (USD oficial a tasa de cambio nominal de cada país)",
    "Costo promedio de 1 día de internación por falla terapéutica en tuberculosis (por complicación de patología o falta de adherencia al tratamiento) (USD oficial a tasa de cambio nominal de cada país)",
    "Costo de 1 consulta a emergencias en pacientes con tuberculosis activa (USD oficial a tasa de cambio nominal de cada país)",
    "Costo mensual del tratamiento de inducción promedio para tuberculosis multirresistente (USD oficial a tasa de cambio nominal de cada país)",
    "Costo mensual del tratamiento de consolidación promedio para tuberculosis multiresistente (USD oficial a tasa de cambio nominal de cada país)",
    "Se utiliza para traer al presente los costos y beneficios en salud futuros",
    "Costo de implementar y sostener tratamiento directamente observado por video en un año (USD oficial a tasa de cambio nominal de cada país)"
  )
}


## pruebas
# params <- get_tbc_params(list(country="Argentina"))
# 
# TBC <- modelo_tbc("Argentina",
#                   params$VOTrrExito,
#                   params$VOTadherencia,
#                   params$costo_evento_VOT,
#                   params$cantidad_vot_semana,
#                   params$ttoExitoso_Duracion,
#                   params$pExitoso,
#                   params$pFalla,
#                   params$pMuerte,
#                   params$VOTrrFalla,
#                   params$VOTrrMuerte,
#                   params$DOTrrExito,
#                   params$DOTrrFalla,
#                   params$DOTrrMuerte,
#                   params$DOTadherencia,
#                   params$cantidad_dot_semana,
#                   params$mediana_edad_paciente,
#                   params$cohorte,
#                   params$utilidad_pob_gral,
#                   params$disutilidad_tbc_activa,
#                   params$prob_internacion_con_falla,
#                   params$cantidadDiasInternacion,
#                   params$costo_trat_induccion,
#                   params$costo_trat_consolidacion,
#                   params$costo_seguimiento,
#                   params$costo_examenes_complemen,
#                   params$costo_evento_DOT,
#                   params$costo_internacion,
#                   params$costoConsulta,
#                   params$costo_trat_multires_induccion,
#                   params$costo_trat_multires_consolidacion,
#                   params$tasa_descuento_anual,
#                   params$costo_intervencion_vDOT)
# 
# 
# 
