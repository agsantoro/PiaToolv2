
options(scipen=999)
library(tidyverse)
###Leo los datos

source("prep/FUNCIONES.R")

datos_paises <- readxl::read_excel("prep/data/Datos_paises.xlsx")
utilidades <- readxl::read_excel("prep/data/utilidades.xlsx")
utilidades2 <- readxl::read_excel("prep/data/utilidades.xlsx", sheet = "Hoja2")
probabilidades <- readxl::read_excel("prep/data/probabilidades.xlsx")
parametros <- readxl::read_excel("prep/data/parametros.xlsx")
costos <- readxl::read_excel("prep/data/Costos.xlsx")
probabilidad_muerte <- readxl::read_excel("prep/data/PROBABILIDAD_MUERTE.xlsx")
distribucion_cohortes <- readxl::read_excel("prep/data/DISTRIBUCION_COHORTES.xlsx")



pais_select <- "ARGENTINA"
casosIncidentes = datos_paises %>%  filter(pais_select==pais) %>% select(`Nuevos casos HIV en poblacion`) %>% as.numeric()
casosIncidentes
casosPrevalentes = datos_paises %>%  filter(pais_select==pais) %>% select(`Casos Prevalentes HIV en población`) %>% as.numeric()
prevalenciaHIV = datos_paises %>%  filter(pais_select==pais) %>% select(`% de HIV en la población`) %>% as.numeric()
pHIVDiagnosticado =  datos_paises %>%  filter(pais_select==pais) %>% select(`% de hiv diagnosticados`) %>% as.numeric()
pHIVTratado = datos_paises %>%  filter(pais_select==pais) %>% select(`% de hiv tratados`) %>% as.numeric()
pHIVControlado = datos_paises %>%  filter(pais_select==pais) %>% select(`% de hiv controlados`) %>% as.numeric()
rrHIVDiagnosticadoNoTratados = 0.43 # ver luego

esperanzaVida = datos_paises %>%  filter(pais_select==pais) %>% select(`Esperanza de Vida`) %>% as.numeric()

##baseline o nuevo
linea <- "Nuevo"#"nuevo"

#####parametros:#####

descuento<- parametros %>% filter(Parametro=="Tasa descuento:") %>% 
  select(!!sym(linea)) %>% as.numeric()

edadMinima <- parametros %>% filter(Parametro=="Edad Minima:") %>%
  select(!!sym(linea)) %>% as.numeric()

edadFinal <- parametros %>% filter(Parametro=="Edad Final:") %>%
  select(!!sym(linea)) %>% as.numeric()

tipoDuracion <- parametros %>% filter(Parametro=="Tipo Duracion:") %>%
  select(!!sym(linea)) %>% as.numeric()

duracionPrEP <- parametros %>% filter(Parametro=="Duracion PrEP:") %>%
  select(!!sym(linea)) %>% as.numeric()

edadMaximaInicial <- parametros %>% filter(Parametro=="Edad Maxima Inicial:") %>%
  select(!!sym(linea)) %>% as.numeric()

PrEPuptake <- parametros %>% filter(Parametro=="PrEP upTake:") %>%
  select(!!sym(linea)) %>% as.numeric()

cohorteDinamica <- parametros %>% filter(Parametro=="Cohorte Dinamica") %>%
  select(!!sym(linea)) %>% as.numeric()

eficaciaPrEP <- parametros %>% filter(Parametro=="Eficacia PrEP:") %>%
  select(!!sym(linea)) %>% as.numeric()

adherenciaPrEP <- parametros %>% filter(Parametro=="Adherencia PrEP:") %>%
  select(!!sym(linea)) %>% as.numeric()

cohorteSize =parametros %>% filter(Parametro=="Tamaño Cohorte:") %>%
  select(!!sym(linea)) %>% as.numeric()

edadFinPrEP <-  parametros %>% filter(Parametro=="Edad No Indicacion de PrEP") %>%
  select(!!sym(linea)) %>% as.numeric()

limiteEdadRiesgo <- parametros %>% filter(Parametro=="Limite edad Riesgo:") %>%
  select(!!sym(linea)) %>% as.numeric()

  
limiteEdadContagiosos <-parametros %>% filter(Parametro=="Limite edad Contagio:") %>%
  select(!!sym(linea)) %>% as.numeric()


#'Seteos basales
testPorAño = 1 #definodo en hoja 6
escribir = "True"
paisCol ="ARGENTINA" #Hardcodeado Argentina
ciclosPorAño = 4 #El modelo hace ciclos trimestrales
tasaDescuento = descuento / ciclosPorAño #Calculamos la tasa de descuento trimestral.
utilidades$`Early HIV`
uHIV = utilidades %>% select(`Early HIV`) %>% as.numeric()
uHIVTTO = utilidades %>% select(`TTO`) %>% as.numeric()
cuHIV = uHIV / ciclosPorAño 
cuHIVTTO = uHIVTTO / ciclosPorAño 

##1

uPoblacion <- numeric(100)  # Crear un vector para almacenar los datos
cuPoblacion <- numeric(100)

for (z in 0:99) {
  uPoblacion[z + 1] <- utilidades2[z + 2, paisCol]
  cuPoblacion[z + 1] <- uPoblacion[[z + 1]] / ciclosPorAño
}


##
aReiniciarPrEP <- numeric(5)
aAbandonoPrEP <- numeric(5)

for (z in 1:5) {
  aReiniciarPrEP[z] <- probabilidades[6 + z, 2]
}

for (z in 1:5) {
  aAbandonoPrEP[z] <- probabilidades[z, 2]
}

# cargamos costos
# '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

cDiagnostico = costos %>% filter(costo=="DIAGNOSTICO") %>%
  select(!!sym(paisCol)) %>%
  as.numeric()

cSeguimientoHIV = costos %>% filter(costo=="SEGUIMIENTO") %>%
  select(!!sym(paisCol)) %>%
  as.numeric()

cTratamientoHIV =costos %>% filter(costo=="TRATAMIENTO") %>%
select(!!sym(paisCol)) %>%
  as.numeric() 

cPrEPTratamiento = costos %>% filter(costo=="PREP_TTO") %>%
select(!!sym(paisCol)) %>%
  as.numeric() 

cPrEPSeguimiento = costos %>% filter(costo=="PREP_SEGUIMIENTO") %>%
  select(!!sym(paisCol)) %>%
  as.numeric() 

cPrEPTest = costos %>% filter(costo=="PREP_TEST") %>%
  select(!!sym(paisCol)) %>%
  as.numeric() 

cConsulta = costos %>% filter(costo=="CONSULTA") %>%
  select(!!sym(paisCol)) %>%
  as.numeric()
##VER ESTO NO HAY COSOTOS PARA COMPLICACIONES
# cComplicacionesHIV = costos %>% filter(costo=="CONSULTA") %>%
#   select(!!sym(paisCol)) %>%
#   as.numeric()

cComplicacionesHIV =0

# 'Cargamos la array de utilidad restante.
utilidadRestante = calcularUtilidadRestante(ciclosPorAño, esperanzaVida, cuPoblacion)


#'Calculamos cada cuantos ciclos se testea.
testeoNoPrEP = ciclosPorAño / testPorAño

#'Asignamos el tamaño final de la cohorte al tamaño inicial.

cohorteSizeFinal = cohorteSize


casosDiagnosticados = casosPrevalentes * pHIVDiagnosticado
casosNoDiagnosticados = casosPrevalentes - casosDiagnosticados
casosTratados = casosDiagnosticados * pHIVTratado
# 'Estos son los casos no tratados dentro de los diagnosticados...
casosNoTratados = casosDiagnosticados - casosTratados
# 'Casos no controlados dentro de los tratados.
casosNoControlados = casosTratados * (1 - pHIVControlado)

#'Calculamos la tasaTransmision para pacientes infectados no diagnosticados.

tasaTransmision <- (casosIncidentes / pHIVDiagnosticado) / (casosNoDiagnosticados + (rrHIVDiagnosticadoNoTratados * (casosNoTratados + casosNoControlados)))

#'Calculamos la tasa de transmision para pacientes diagnosticados pero no tratados y para aquellos tratados no controlados.
tasaTransmisionNoControlados = tasaTransmision * rrHIVDiagnosticadoNoTratados


#'Definimos el numero de años que durara el modelo, el mismo corre hasta que el menor de la cohorte cumpla 100 años.
añosRango = edadFinal - edadMinima
#Definimos el numero de ciclos.
numeroCiclos = añosRango * ciclosPorAño


# Definimos la configuración
if (tipoDuracion == 0) {
  # Un usuario recibe PrEP por la duración que hayan asignado
  if (duracionPrEP == 99) {
    # La intervención dura hasta la edad de fin de indicación de PrEP
    # El estadio onPrep tiene 5 años de túneles para tener probabilidades distintas
    # Al llegar al último año se asume que es la probabilidad final de abandono de PrEP
    ciclosPrEPON <- 5 * ciclosPorAño
  } else {
    # Si duración PrEP es distinta a 99 se le da un túnel por cada año
    ciclosPrEPON <- duracionPrEP * ciclosPorAño
  }
} else if (tipoDuracion == 1) {
  # La medida dura lo que hayan asignado en duración
  # Es decir, todos reciben PrEP durante el mismo tiempo
  if (duracionPrEP < 5) {
    ciclosPrEPON <- duracionPrEP * ciclosPorAño
  } else {
    ciclosPrEPON <- 5 * ciclosPorAño
  }
}

#ciclosPrEPoff siempre tiene 5 años d tuneles con distintas probabilidades.
ciclosPrEPOFF = 5 * ciclosPorAño

pAbandonoPrEP <- list()
for (z in 1:ciclosPrEPON) {
  rTemp <- -log(1 - as.numeric(aAbandonoPrEP[((z - 1) %/% ciclosPorAño) + 1]))
  pAbandonoPrEP[z] <- 1 - exp(-rTemp * (1 / ciclosPorAño))
}

pReiniciarPrEP <- list()
for (z in 1:ciclosPrEPOFF) {
  rTemp <- -log(1 - as.numeric(aReiniciarPrEP[((z - 1) %/% ciclosPorAño) + 1]))
  pReiniciarPrEP[z] <- 1 - exp(-rTemp * (1 / ciclosPorAño))
}

pMuerteGeneral <- NULL
pMuerteGeneral <- numeric(99)  
for (z in 1:86) {
  pMuerteGeneral[14+z] <- 1 - exp(-probabilidad_muerte[z , paisCol] * (1 / ciclosPorAño))
}

pMuerteGeneral

pMuerteHIV <- numeric(99) 
for (z in 1:86) {
  tasaMortalidad <- probabilidades[z, 6]  
  pMuerteHIV[14 + z] <- 1 - exp(-(-log(1 - tasaMortalidad)) * (1 / ciclosPorAño))
}

##
distribucionCohorte <- cargarDistribucion(edadMinima, edadMaximaInicial)


##creo modelo #######
modelo <- list()
modelo$ciclos <- vector("list", numeroCiclos + 1)  # +1 porque R indexa desde 1



# Inicializar estructuras para el primer ciclo
modelo$ciclos[[1]]$SanoOnPrEP <- vector("list", ciclosPrEPON)
modelo$ciclos[[1]]$SanoOffPrEP <- vector("list", ciclosPrEPOFF)
modelo$ciclos[[1]]$SanoSinPrEP <- list(Personas = numeric(edadFinal- edadMinima+1 ))
modelo$ciclos[[1]]$InfectadoDx <- list(Personas = numeric(edadFinal ))
modelo$ciclos[[1]]$InfectadoNoDx <- list(Personas = numeric(edadFinal ))
modelo$ciclos[[1]]$InfectadoPreDx <- list(Personas = numeric(edadFinal))
modelo$ciclos[[1]]$MuerteGeneral <- list(Personas = numeric(edadFinal))
modelo$ciclos[[1]]$MuerteHIV <- list(Personas = numeric(edadFinal ))

modelo$ciclos[[1]]$nuevosCasos <- 0
modelo$ciclos[[1]]$nuevosCasosDx <- 0
modelo$ciclos[[1]]$costoPrEP <- 0
modelo$ciclos[[1]]$CostoHIV <-0
modelo$ciclos[[1]]$CostoSano <- 0
modelo$ciclos[[1]]$CasosAcumulados <- 0
#modelo$ciclos[[1]]$SanosOffPrEPTotal <- 0
modelo$casosHIVDx <-0
modelo$casosHIV <-0
finalizados=0

i <- 1
# Suponiendo que 'escribir' es una variable lógica (TRUE/FALSE)
modelo$ciclos[[1]]$SanoSinPrEP$Total =0
#modelo$ciclos[[i]]$InfectadoNoDx$Total=0
modelo$ciclos[[i]]$InfectadoPreDx$Total=0
#modelo$ciclos[[i]]$InfectadoDx$Total=0
modelo$ciclos[[i]]$MuerteGeneral$Total=0
modelo$ciclos[[i]]$MuerteHIV$Total=0

###2
#i <- 2
# Suponiendo que 'escribir' es una variable lógica (TRUE/FALSE)
modelo$ciclos[[i]]$SanoSinPrEP$Total =0
#modelo$ciclos[[i]]$InfectadoNoDx$Total=0
modelo$ciclos[[i]]$InfectadoPreDx$Total=0
#modelo$ciclos[[i]]$InfectadoDx$Total=0
modelo$ciclos[[i]]$MuerteGeneral$Total=0
modelo$ciclos[[i]]$MuerteHIV$Total=0
#######

for (z in 1:ciclosPrEPON) {
  modelo$ciclos[[1]]$SanoOnPrEP[[z]]$Personas <- numeric(edadFinal)
}

for (z in 1:ciclosPrEPOFF) {
  modelo$ciclos[[1]]$SanoOffPrEP[[z]]$Personas <- numeric(edadFinal)
  
}


# Asignar valores iniciales
modelo$ciclos[[1]]$SanosTotal <- cohorteSize * (1 - prevalenciaHIV)
modelo$ciclos[[1]]$SanosOnPrEPTotal <- modelo$ciclos[[1]]$SanosTotal * PrEPuptake
modelo$ciclos[[1]]$SanosOffPrEPTotal<- 0
modelo$ciclos[[1]]$SanoSinPrEP$Total <- modelo$ciclos[[1]]$SanosTotal * (1 - PrEPuptake)
modelo$ciclos[[1]]$InfectadosTotal <- cohorteSize * prevalenciaHIV
modelo$ciclos[[1]]$InfectadoDx$Total <- modelo$ciclos[[1]]$InfectadosTotal * pHIVDiagnosticado
modelo$ciclos[[1]]$InfectadoNoDxTotal <- modelo$ciclos[[1]]$InfectadosTotal * (1 - pHIVDiagnosticado)
modelo$ciclos[[1]]$MuerteGeneral$Total <-0
#modelo$ciclos[[2]]$MuerteGeneral$Total <-0
modelo$ciclos[[1]]$InfectadoPreDx$Total <- 0
# Distribuir la población inicial en los estados
totalCheck <- 0
# length(distribucionCohorte)
# 
# edadMinima - edadMaximaInicial
# modelo$ciclos[[2]]$SanosTotal <- cohorteSize * (1 - prevalenciaHIV)
# modelo$ciclos[[2]]$SanosOnPrEPTotal <- modelo$ciclos[[1]]$SanosTotal * PrEPuptake
# modelo$ciclos[[2]]$SanosOffPrEPTotal<- 0
# modelo$ciclos[[2]]$SanoSinPrEP$Total <- modelo$ciclos[[1]]$SanosTotal * (1 - PrEPuptake)
# modelo$ciclos[[2]]$InfectadosTotal <- cohorteSize * prevalenciaHIV
# modelo$ciclos[[2]]$InfectadoDx$Total <- modelo$ciclos[[1]]$InfectadosTotal * pHIVDiagnosticado
# modelo$ciclos[[2]]$InfectadoNoDx$Total <- modelo$ciclos[[1]]$InfectadosTotal * (1 - pHIVDiagnosticado)
#distribucionCohorte <- c(rep(0,17) ,distribucionCohorte,rep(0,32))
for (i in 1 : edadMaximaInicial) {
  
  modelo$ciclos[[1]]$SanoOnPrEP[[1]]$Personas[i] <- modelo$ciclos[[1]]$SanosOnPrEPTotal * distribucionCohorte[i]
  modelo$ciclos[[1]]$SanoSinPrEP$Personas[i] <- modelo$ciclos[[1]]$SanoSinPrEP$Total * distribucionCohorte[i]
  modelo$ciclos[[1]]$InfectadoDx$Personas[i] <- modelo$ciclos[[1]]$InfectadoDx$Total * distribucionCohorte[i]
  modelo$ciclos[[1]]$InfectadoNoDx$Personas[i] <- modelo$ciclos[[1]]$InfectadoNoDxTotal * distribucionCohorte[i]
  totalCheck <- sum(totalCheck , modelo$ciclos[[1]]$SanoOnPrEP[[1]]$Personas[i] , 
    modelo$ciclos[[1]]$SanoSinPrEP$Personas[i],
    modelo$ciclos[[1]]$InfectadoDx$Personas[i] , 
    modelo$ciclos[[1]]$InfectadoNoDx$Personas[i], na.rm = T)
  
  
}




# Manejar la dinámica de la cohorte

if (cohorteDinamica == 1) {
  genteEntrante <- cohorteSize * distribucionCohorte[edadMinima]
}

# Manejar la dinámica de la cohorte
genteEntrante <- ifelse(cohorteDinamica == 1, cohorteSize * distribucionCohorte[edadMinima], 0)

# Calcular total de infectados
modelo$ciclos[[1]]$InfectadosTotal <- modelo$ciclos[[1]]$InfectadoDx$Total + modelo$ciclos[[1]]$InfectadoNoDxTotal

# Las personas en riesgo son los sanos
personasRiesgo <- modelo$ciclos[[1]]$SanosTotal


# Calculamos el modificador de eficacia basado en la adherencia
efModifier <- (0.13 / 0.86) * eficaciaPrEP

# Calculamos el riesgo relativo (rrPrEP) basado en la adherencia
if (adherenciaPrEP >= 0.8) {
  rrPrEP <- 1 - eficaciaPrEP
} else if (adherenciaPrEP <= 0.2) {
  rrPrEP <- 1
} else {
  rrPrEP <- 1 - (eficaciaPrEP - (efModifier * ((0.8 - adherenciaPrEP) / 0.1)))
}

# Aseguramos que rrPrEP esté entre 0 y 1
rrPrEP <- min(max(rrPrEP, 0), 1)

# Asignar la cantidad de individuos diagnosticados contagiosos y no diagnosticados
DiagnosticadosContagiosos <- modelo$ciclos[[1]]$InfectadoDx$Total
NoDiagnosticadosContagiosos <- modelo$ciclos[[1]]$InfectadoNoDxTotal +
  modelo$ciclos[[1]]$InfectadoPreDx$Total

escribir <- TRUE
i <- 1


modelo$costoPrEP <-0
if (escribir) {
  # Llamada a la función 'escribirOutput' con los argumentos correspondientes
  # Aquí asumimos que todas las propiedades (.Total, etc.) están definidas adecuadamente
  escribirOutput(1, ciclosPorAño, modelo$ciclos[[i]]$SanoSinPrEP$Total, modelo$ciclos[[i]]$SanosOffPrEPTotal, 
                 modelo$ciclos[[i]]$SanosOnPrEPTotal, modelo$ciclos[[i]]$InfectadoNoDxTotal, 
                 modelo$ciclos[[i]]$InfectadoPreDx$Total, modelo$ciclos[[i]]$InfectadoDx$Total, 
                 modelo$ciclos[[i]]$MuerteGeneral$Total, modelo$ciclos[[i]]$MuerteHIV$Total, 
                 personasRiesgo, modelo$ciclos[[i]]$InfectadosTotal, 0, 0, modelo$ciclos[[i]]$nuevosCasosDx, 
                 0, 0, 0, 0, 0, 0, 0, 0)
}


# Establecer la edad inicial para el loop

edadLoopInicial <- edadMinima
# Si la cohorte es dinámica, la edad inicial siempre será edadMinima.
# Si no es dinámica, la lógica para ajustar edadLoopInicial deberá ser añadida.

cicloCounter <- 2
finalizados=0
modelo$anosVividos <- 0
modelo$anosVividosD <-0
modelo$qalysVividos <-0
modelo$qalysVividosD <-0
modelo$anosPerdidosMPHIV <-0
modelo$qalysPerdidosMPHIVD <-0
modelo$qalysPerdidosMPHIV <-0
modelo$qalysPerdidosDiscHIV <-0
modelo$qalysPerdidosDiscHIVD <-0
modelo$qalysPerdidos <-0
modelo$TiempoSinDx <- 0
modelo$CostoSano <- 0
modelo$CostoHIV <-0
modelo$costoHIVD <-0
modelo$costoPrEPD <-0
modelo$costoPrEP <-0
modelo$costoSanoD <- 0
modelo$qalysPerdidosD <-0
modelo$anosPerdidosMPHIVD <-0


DiagnosticadosContagiosos <- modelo$ciclos[[1]]$InfectadoDx$Total

NoDiagnosticadosContagiosos <- modelo$ciclos[[1]]$InfectadoNoDxTotal +
  modelo$ciclos[[1]]$InfectadoPreDx$Total


#### inicia loop i #####
anosPasados <- 0
#numeroCiclos

for (i in 2:(numeroCiclos+1)) {
  
  #browser()
  # # Inicializar las estructuras para el i-ésimo ciclo
  
  modelo$ciclos[[i]]$SanoSinPrEP <-
    list(Personas = numeric(edadFinal - edadMinima + 1))
  modelo$ciclos[[i]]$SanoOnPrEP <- vector("list", ciclosPrEPON)
  modelo$ciclos[[i]]$SanoOffPrEP <- vector("list", ciclosPrEPOFF)
  modelo$ciclos[[i]]$InfectadoDx <-
    list(Personas = numeric(edadFinal - edadMinima + 1))
  modelo$ciclos[[i]]$InfectadoPreDx <-
    list(Personas = numeric(edadFinal - edadMinima + 1))
  modelo$ciclos[[i]]$InfectadoNoDx <-
    list(Personas = numeric(edadFinal - edadMinima + 1))
  modelo$ciclos[[i]]$MuerteGeneral <-
    list(Personas = numeric(edadFinal - edadMinima + 1))
  modelo$ciclos[[i]]$MuerteHIV <-
    list(Personas = numeric(edadFinal - edadMinima + 1))
  modelo$ciclos[[i]]$nuevosCasos<- 0
  modelo$ciclos[[i]]$nuevosCasosDx<- 0
  
  #browser(expr={i==50})
  
  

  for (z in 1:(ciclosPrEPON)) {
    modelo$ciclos[[i]]$SanoOnPrEP[[z]]$Personas <-
      numeric(edadFinal - edadMinima + 1)
  }
  
  for (z in 1:(ciclosPrEPOFF)) {
    modelo$ciclos[[i]]$SanoOffPrEP[[z]]$Personas <-
      numeric(edadFinal - edadMinima + 1)
  }
  
  # Lógica para el contador de ciclos
  if (cicloCounter == 4) {
    cicloCounter <- 0
  }
  cicloCounter <- cicloCounter + 1
  
  
  #anoOffset <- 1
  # Si estamos en un ciclo múltiplo de 4, significa que ha pasado 1 año
  if (i %in% seq(5, 328, by = ciclosPorAño)) {
    anoOffset <- 1
    # Establecemos anoOffset en 1 para poder envejecer la población
    anosPasados <- anosPasados + 1
    # Sumamos un año
    # Si la cohorte es dinámica, la edad inicial del bucle sigue siendo la edad mínima
    if (cohorteDinamica == 1) {
      edadLoopInicial <- edadMinima
      # Ha pasado 1 año, necesitamos ingresar nuevas personas a los estados
      if (tipoDuracion == 0 || (tipoDuracion == 1 && i < ciclosPorAño * duracionPrEP)) {
        modelo$ciclos[[i]]$SanoOnPrEP[[1]]$Personas[1] <- genteEntrante * PrEPuptake
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[1] <- genteEntrante * (1 - PrEPuptake)
      } else {
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[1] <- genteEntrante
      }
      # Ajustamos el tamaño de la cohorte final
      cohorteSizeFinal <- cohorteSizeFinal + genteEntrante
    } else if (cohorteDinamica == 0) {
      # Si la cohorte es estática, actualizamos la edad mínima para no iterar en estados vacíos
      edadLoopInicial <- edadMinima + anosPasados
    }
  } else {
    # Si no es múltiplo de 4, anoOffset es 0
    anoOffset <- 0
  }
  

  #browser()
 
        
  #Si hay personas en riesgo de contagiarse tenemos que calcular la probabilidad
  # Cálculo de la probabilidad de tener HIV en el ciclo
  # Verificamos si hay personas en riesgo de contagiarse
  
  # browser(exp= i==2)
  
  if (personasRiesgo > 0) {
    
    # Calculamos la probabilidad anual de tener HIV
    pAnualHIV <- ((tasaTransmision * NoDiagnosticadosContagiosos) +
                    (tasaTransmisionNoControlados * ((DiagnosticadosContagiosos * (1 - pHIVTratado)) +
                                                       (DiagnosticadosContagiosos * pHIVTratado * (1 - pHIVControlado))))) / personasRiesgo
    
    # Limitamos la probabilidad anual a un máximo de 0.99
    if (pAnualHIV >= 1) {
      pAnualHIV <- 0.99
    }
    
    # Convertimos la probabilidad en tasa
    rAnualHIV <- -log(1 - pAnualHIV) # dividido por 1, que es equivalente a no cambiarlo
    
    # Recalculamos la probabilidad en el ciclo
    pCicloHIV <- 1 - exp(-rAnualHIV * (1 / ciclosPorAño))
  }
  
  

  # Inicializar variables para el siguiente ciclo
  DiagnosticadosContagiosos <- 0
  NoDiagnosticadosContagiosos <- 0
  personasRiesgo <- 0
  
 
  # Actualizar muertes totales y casos acumulados
  
    modelo$ciclos[[i]]$MuerteGeneral$Total <-
      modelo$ciclos[[i - 1]]$MuerteGeneral$Total
    modelo$ciclos[[i]]$MuerteHIV$Total <-
      modelo$ciclos[[i - 1]]$MuerteHIV$Total
    modelo$ciclos[[i]]$CasosAcumulados <-
      modelo$ciclos[[i - 1]]$CasosAcumulados
  
    #(edadMaximaInicial + anosPasados)
    #print(paste("anos pasados",anosPasados))
#inicia loop j --------------    
    
    #((edadMaximaInicial+ anosPasados)- 10)
  for (j in edadLoopInicial:(edadMaximaInicial+ anosPasados)) {

   
    if (j > (edadFinal)) {
      # Si la edad es mayor que el límite (ej: 100 años), se finaliza el ciclo de esa edad
      if (anoOffset == 1 ) {
        
        finalizados <-
          finalizados + modelo$ciclos[[i - 1]]$InfectadoDx$Personas[100] +
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[100] +
          modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[100] +
          modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[100]
      }
      break  # Salir del bucle de edad
    }
   
    
    
    # Inicializar contadores para este ciclo
    nuevosInfectadosSinPrEP <- 0
    nuevosInfectadosConPrEP <- 0
    conteoParcialSOFP <- 0
    conteoParcialSOP <- 0
    
    # Establecer la probabilidad de contagio de HIV a 0 si se supera el límite de edad de riesgo
    if (j > limiteEdadRiesgo) {
      pCicloHIV <- 0
    }
    
    # Manejo de cohortes dinámicas
    if (!(cohorteDinamica == 1 &&
          j == edadMinima && anoOffset == 1 && i >1)) {
      
      # Cálculos para los sanos sin PrEP y ajustes por muerte general y contagio de HIV
  
      #browser()
      
      modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
        modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] * pCicloHIV +
            modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
        )
      
      # Continuar con más cálculos y lógica dentro del bucle de edad
    }
    if (j <= edadFinPrEP &&
        !(tipoDuracion == 1 && i >= duracionPrEP * ciclosPorAño)) {
      # Si todavía puede reiniciar PrEP
      
      
      
      modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
        modelo$ciclos[[i-1]]$SanoSinPrEP$Personas[j] +
        
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] -
            (
              modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
                modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * pCicloHIV +
                modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * as.numeric(pReiniciarPrEP[ciclosPrEPOFF])
            )
        
      
      # Para aquellos que reinician PrEP
      modelo$ciclos[[i]]$SanoOnPrEP[[1]]$Personas[j] <-
        modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * as.numeric(pReiniciarPrEP[ciclosPrEPOFF])
      
      # Contador de los que reinician PrEP
      conteoParcialSOP <-
        conteoParcialSOP + modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * as.numeric(pReiniciarPrEP[ciclosPrEPOFF])
      
    } else {
      # Si no pueden reiniciar PrEP
      modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] +
        (
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] -
            (
              modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
                modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * pCicloHIV
            )
        )
    }
    
    # Actualizar las muertes generales y los nuevos infectados
    modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
      modelo$ciclos[[i - 1]]$MuerteGeneral$Personas[j - anoOffset] +
      modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
      modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    nuevosInfectadosSinPrEP <-
      nuevosInfectadosSinPrEP + modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] * pCicloHIV +
      modelo$ciclos[[i - 1]]$SanoOffPrEP[[ciclosPrEPOFF]]$Personas[j - anoOffset] * pCicloHIV
    # '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HANDLING DE SANOS SIN PrEP Y LOS QUE ESTABAN EN EL ULTIMO CICLO DE SANO OFF PrEP.
    #
    #             'Los que estan en PrEP
    # 'Pueden discontinuar PrEP, morir por muerte general, contraer HIV
    #             'Y se sumaran aquellos que habiendo dejado PrEP vuelvan a iniciarla.
    #
    # 'Nos fijamos si estamos evaluando una edad dentro de la indicación de PrEP.
    #             'y si tipoDuracion = 1 nos fijamos que no este fuera de epoca de la indicación.
    if (j <= edadFinPrEP &&
        !(tipoDuracion == 1 && i >= duracionPrEP * ciclosPorAño)) {
      # Verifica si estamos evaluando una edad dentro de la indicación de PrEP
      for (z in 2:(ciclosPrEPON -1 )) {
        # Actualiza las personas en PrEP en el estado actual (z)
        
       # browser()
       
        modelo$ciclos[[i]]$SanoOnPrEP[[z]]$Personas[j] <-
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] -
          (
            modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP +
              modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
          )
        
        # Resta los que abandonaron PrEP
        modelo$ciclos[[i]]$SanoOnPrEP[[z]]$Personas[j] <-
          modelo$ciclos[[i]]$SanoOnPrEP[[z]]$Personas[j] -
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(as.numeric(as.numeric(pAbandonoPrEP[z - 1])))
        
        # Agrega los nuevos abandonadores de PrEP al primer estado de SanoOffPrEP
        modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] <-#aca
         # modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] +
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[z - 1])
        
        # Actualiza muertes generales y nuevos infectados
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
          modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
        
        nuevosInfectadosConPrEP <-
          nuevosInfectadosConPrEP + modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP
        
        
        # Contadores para los que entran y salen de PrEP
        conteoParcialSOP <-
          conteoParcialSOP + modelo$ciclos[[i]]$SanoOnPrEP[[z]]$Personas[j]
        conteoParcialSOFP <-
          conteoParcialSOFP + modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(as.numeric(pAbandonoPrEP[z - 1]))
      }
    }
    
    if (duracionPrEP < 99) {
      # Maneja el último estadio de OnPrEP
      modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
        modelo$ciclos[[i-1]]$SanoSinPrEP$Personas[j] +
        (
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] -
            (
              modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP +
                modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
            )
        )
      
      nuevosInfectadosConPrEP <- nuevosInfectadosConPrEP +
        modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP
      
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
        modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    } else {
      
      modelo$ciclos[[i]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j] <-
        modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP +
            modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
            modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[ciclosPrEPON])
        )
      
      modelo$ciclos[[i]]$SanoOffPrEP[[z]]$Personas[j] <-
        modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] +
        modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[ciclosPrEPON])
      
      conteoParcialSOFP <-
        conteoParcialSOFP + modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[ciclosPrEPON])
      
      
      
      
      nuevosInfectadosConPrEP <-
        nuevosInfectadosConPrEP + modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP
      
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
        modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      
      conteoParcialSOP <-
        conteoParcialSOP + modelo$ciclos[[i]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j]
    }
    
    # Agregar a los que estaban en el estado anterior de OnPrEP
    modelo$ciclos[[i]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j] <-
      modelo$ciclos[[i]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j] +
      modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] -
      (
        modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP +
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      )
    
    # Agregar nuevos infectados y manejar abandonos de PrEP
    nuevosInfectadosConPrEP <- nuevosInfectadosConPrEP +
      modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP
    
    modelo$ciclos[[i]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j] <-
      modelo$ciclos[[i]]$SanoOnPrEP[[ciclosPrEPON]]$Personas[j] -
      modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[ciclosPrEPON - 1])
    
    conteoParcialSOP <- conteoParcialSOP +
      (
        modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] -
          (
            modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP +
              modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
              modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[ciclosPrEPON - 1])
          )
      )
    
    modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] <-
      modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] +
      modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[ciclosPrEPON - 1])
    
    conteoParcialSOFP <- conteoParcialSOFP +
      modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] *as.numeric(pAbandonoPrEP[ciclosPrEPON - 1])
    
    modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
      modelo$ciclos[[i - 1]]$SanoOnPrEP[[ciclosPrEPON - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    if (j == edadFinPrEP + 1 &&
        anoOffset == 1 ||
        (tipoDuracion == 1 && i == duracionPrEP * ciclosPorAño)){
      for (z in 1:ciclosPrEPON) {
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
          modelo$ciclos[[i-1]]$SanoSinPrEP$Personas[j] +
          (
            modelo$ciclos[[i - 1]]$SanoOnPrEP[[z]]$Personas[j - anoOffset] -
              (
                modelo$ciclos[[i - 1]]$SanoOnPrEP[[z]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
                  modelo$ciclos[[i - 1]]$SanoOnPrEP[[z]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP
              )
          )
        
        nuevosInfectadosConPrEP <- nuevosInfectadosConPrEP +
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP
        
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
          modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      }
    }
    
    if (j <= edadFinPrEP &&
        !(tipoDuracion == 1 && i >= duracionPrEP * ciclosPorAño)) {
      for (z in 2:(ciclosPrEPOFF-1)) {
        modelo$ciclos[[i]]$SanoOffPrEP[[z]]$Personas[j] <-
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] -
          (
            modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] * pCicloHIV +
              modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
              modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pReiniciarPrEP[z - 1])
          )
        
        modelo$ciclos[[i]]$SanoOnPrEP[[1]]$Personas[j] <-
          modelo$ciclos[[i]]$SanoOnPrEP[[1]]$Personas[j] +
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pReiniciarPrEP[z - 1])
        
        nuevosInfectadosSinPrEP <- nuevosInfectadosSinPrEP +
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] * pCicloHIV
        
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
          modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
        
        conteoParcialSOFP <-
          conteoParcialSOFP + modelo$ciclos[[i]]$SanoOffPrEP[[z]]$Personas[j]
        conteoParcialSOP <-
          conteoParcialSOP + modelo$ciclos[[i - 1]]$SanoOffPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pReiniciarPrEP[z - 1])
      }
    }
   
    
    if (j == edadFinPrEP + 1 &&
        anoOffset == 1 ||
        (tipoDuracion == 1 && i == duracionPrEP * ciclosPorAño)) {
      for (z in 1:(ciclosPrEPOFF - 1)) {
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
          modelo$ciclos[[i-1]]$SanoSinPrEP$Personas[j] +
          (
            modelo$ciclos[[i - 1]]$SanoOffPrEP[[z]]$Personas[j - anoOffset] -
              (
                modelo$ciclos[[i - 1]]$SanoOffPrEP[[z]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
                  modelo$ciclos[[i - 1]]$SanoOffPrEP[[z]]$Personas[j - anoOffset] * pCicloHIV
              )
          )
        
        nuevosInfectadosSinPrEP <- nuevosInfectadosSinPrEP +
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[z]]$Personas[j - anoOffset] * pCicloHIV
        
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
          modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
          modelo$ciclos[[i - 1]]$SanoOffPrEP[[z]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      }
    }
    
    # Actualizamos los infectados no diagnosticados
    modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] <-
      modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] -
      (
        modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
          modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
      )
    
    # Los que no están en PrEP tienen un porcentaje de ser diagnosticados
    modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] <-
      modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] +
      (nuevosInfectadosSinPrEP * (1 - pHIVDiagnosticado))
    
    # Agregamos las muertes por causas generales y por HIV dentro de los Infectados no dx
    modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
      modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
      modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    
    # Actualizamos los infectados diagnosticados
    modelo$ciclos[[i]]$InfectadoDx$Personas[j] <-
      modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] -
      (
        modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
          modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
      )
    
    # Agregamos las muertes por causas generales y por HIV dentro de los Infectados dx
    modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
      modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
      modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    
    
    if (i %% testeoNoPrEP == 0) {
      # Ciclo diagnóstico
      modelo$ciclos[[i]]$InfectadoDx$Personas[j] <-
        modelo$ciclos[[i-1]]$InfectadoDx$Personas[j] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
            modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
        )
      
      # Sin nuevos casos en este grupo para este ciclo
      modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] <- 0
      
      # Agregamos los nuevos casos diagnosticados
      modelo$ciclos[[i]]$InfectadoDx$Personas[j] <-
        modelo$ciclos[[i-1]]$InfectadoDx$Personas[j] +
        (nuevosInfectadosSinPrEP * pHIVDiagnosticado) + nuevosInfectadosConPrEP
      
      # Agregamos las muertes por causas generales y por HIV dentro de los PreDx
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
        modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    } else {
      # No es un ciclo de diagnóstico
      modelo$ciclos[[i]]$InfectadoDx$Personas[j] <-
        modelo$ciclos[[i-1]]$InfectadoDx$Personas[j] + nuevosInfectadosConPrEP
      
      # Actualizamos los infectados PreDx
      modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] <-
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])+
            modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
        )
      
      # Le sumamos los casos nuevos que vienen de la rama sin PrEP
      modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] <-
        modelo$ciclos[[i-1]]$InfectadoPreDx$Personas[j] +
        nuevosInfectadosSinPrEP * pHIVDiagnosticado
      
      # Agregamos las muertes por causas generales y por HIV dentro de los PreDx
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
        modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    }
    
    # Si mueren antes de la esperanza de vida, agrega utilidad perdida
    if (j - anoOffset < esperanzaVida) {
      modelo$ciclos[[1]]$LyPerdidosHIVMP <-0
      modelo$ciclos[[i]]$LyPerdidosHIVMP <-
        modelo$ciclos[[i-1]]$LyPerdidosHIVMP +
        (modelo$ciclos[[i]]$MuerteHIV$Personas[j] * (esperanzaVida - (j - anoOffset)))
      
      modelo$ciclos[[1]]$QalyPerdidoHIVMP <-0
      modelo$ciclos[[i]]$QalyPerdidoHIVMP <-
        modelo$ciclos[[i-1]]$QalyPerdidoHIVMP +
        (modelo$ciclos[[i]]$MuerteHIV$Personas[j] * utilidadRestante[(((j - anoOffset) * ciclosPorAño) + cicloCounter)])
    }
    
    # Actualizar total de muertes generales
    modelo$ciclos[[i]]$MuerteGeneral$Total <-
      modelo$ciclos[[i-1]]$MuerteGeneral$Total +## aca agregue -1
      (
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] -
          modelo$ciclos[[i - 1]]$MuerteGeneral$Personas[j - anoOffset]
      )
    
    # Si es ciclo de testeo, agregar costos
    if (i %% testeoNoPrEP == 0 && i > 1) {
      modelo$ciclos[[i]]$CostoSano <- modelo$ciclos[[i-1]]$CostoSano +
        (cDiagnostico * (
          modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] + conteoParcialSOFP
        ))
      
      modelo$ciclos[[i]]$CostoHIV <- modelo$ciclos[[i-1]]$CostoHIV +
        ((
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] -
            (
              modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
                modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
            )
        ) *
          (cDiagnostico + cConsulta))
    } else {
      
      modelo$ciclos[[i]]$CostoSano <-modelo$ciclos[[i-1]]$CostoSano
      
      conteoParcialSOP <-
        modelo$ciclos[[i-1]]$SanoOnPrEP[[1]]$Personas[edadMinima]
    }
    # Actualizar costos asociados con el tratamiento y seguimiento de VIH
    modelo$ciclos[[i]]$CostoHIV <- modelo$ciclos[[i-1]]$CostoHIV +
      (modelo$ciclos[[i]]$InfectadoDx$Personas[j] * pHIVTratado * cTratamientoHIV) +
      (modelo$ciclos[[i]]$InfectadoDx$Personas[j] * pHIVTratado * cSeguimientoHIV) +
      ((
        modelo$ciclos[[i]]$InfectadoDx$Personas[j] * (1 - pHIVTratado * pHIVControlado)
      ) +
        modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] + modelo$ciclos[[i]]$InfectadoNoDx$Personas[j]
      ) * cComplicacionesHIV
    
    
    # Actualizar costos de PrEP
    
    modelo$ciclos[[i]]$costoPrEP <- modelo$ciclos[[i-1]]$costoPrEP+
      (conteoParcialSOP * (cPrEPSeguimiento + cPrEPTratamiento + cPrEPTest))
    
    # Agregar costo de consulta para nuevos infectados con PrEP
    modelo$ciclos[[i]]$CostoHIV <- modelo$ciclos[[i]]$CostoHIV +
      nuevosInfectadosConPrEP * cConsulta
    
    # Actualizar la disutilidad
    modelo$ciclos[[1]]$Disutilidad <-0
    modelo$ciclos[[i]]$Disutilidad <-
      modelo$ciclos[[i-1]]$Disutilidad +
      ((
        modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] + modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] +
          (modelo$ciclos[[i]]$InfectadoDx$Personas[j] * (1 - pHIVTratado))
      ) * cuHIV
      ) +
      modelo$ciclos[[i]]$InfectadoDx$Personas[j] * pHIVTratado * cuHIVTTO
    
    #browser()
    
    # Actualizar totales y acumulados
    print(paste("$MuerteHIV$Total",j,modelo$ciclos[[i]]$MuerteHIV$Total))
    modelo$ciclos[[i]]$MuerteHIV$Total <-
      modelo$ciclos[[i]]$MuerteHIV$Total + modelo$ciclos[[i]]$MuerteHIV$Personas[j]
    modelo$ciclos[[i]]$InfectadoDx$Total <-
      modelo$ciclos[[i-1]]$InfectadoDx$Total + modelo$ciclos[[i]]$InfectadoDx$Personas[j]
    modelo$ciclos[[i]]$InfectadoPreDx$Total <-
      modelo$ciclos[[i-1]]$InfectadoPreDx$Total + modelo$ciclos[[i]]$InfectadoPreDx$Personas[j]
    modelo$ciclos[[i]]$InfectadoNoDxTotal <-
      modelo$ciclos[[i-1]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoNoDx$Personas[j]
    modelo$ciclos[[i]]$SanoSinPrEP$Total <-
      modelo$ciclos[[i-1]]$SanoSinPrEP$Total + modelo$ciclos[[i-1]]$SanoSinPrEP$Personas[j]
    modelo$ciclos[[i]]$SanosOffPrEPTotal <-
      modelo$ciclos[[i-1]]$SanosOffPrEPTotal + conteoParcialSOFP
    modelo$ciclos[[i]]$SanosOnPrEPTotal <-
      modelo$ciclos[[i-1]]$SanosOnPrEPTotal + conteoParcialSOP
    
    
    modelo$ciclos[[i]]$SanosTotal <-
      modelo$ciclos[[i]]$SanosOnPrEPTotal + modelo$ciclos[[i]]$SanosOffPrEPTotal + 
      modelo$ciclos[[i]]$SanoSinPrEP$Total
    modelo$ciclos[[i]]$InfectadosTotal <-
      modelo$ciclos[[i]]$InfectadoPreDx$Total + modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoDx$Total
    modelo$ciclos[[i]]$nuevosCasos <-
      modelo$ciclos[[i]]$nuevosCasos + nuevosInfectadosSinPrEP + nuevosInfectadosConPrEP
    modelo$ciclos[[i]]$nuevosCasosDx <-
      modelo$ciclos[[i]]$nuevosCasos
    
    modelo$ciclos[[i]]$CasosAcumulados <-
      modelo$ciclos[[i]]$CasosAcumulados + nuevosInfectadosSinPrEP + nuevosInfectadosConPrEP
    # Calcular QALY para personas con VIH
    
    modelo$ciclos[[1]]$qalyHIV <-0
    
    modelo$ciclos[[i]]$qalyHIV <- modelo$ciclos[[i-1]]$qalyHIV + (
      (
        modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] + modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] +
          (modelo$ciclos[[i]]$InfectadoDx$Personas[j] * (1 - pHIVTratado))
      ) * (cuPoblacion[j] - cuHIV) +
        modelo$ciclos[[i]]$InfectadoDx$Personas[j] * pHIVTratado * (cuPoblacion[j] - cuHIVTTO)
    )
    
    # Calcular QALY para personas sin VIH
    modelo$ciclos[[1]]$qalyNOHIV <-0
    modelo$ciclos[[i]]$qalyNOHIV <- modelo$ciclos[[i-1]]$qalyNOHIV + (
      (
        conteoParcialSOFP + conteoParcialSOP + modelo$ciclos[[i]]$SanoSinPrEP$Personas[j]
      ) * cuPoblacion[j]
    )
    

    
    # Establecer la población en riesgo
    if (j <= limiteEdadRiesgo) {
      
      personasRiesgo <- modelo$ciclos[[i]]$SanosTotal
    }
    
    # Si está por debajo del límite de edad contagiosa, agregar a aquellos que pueden contagiar
    if (j <= limiteEdadContagiosos) {
      DiagnosticadosContagiosos <-
        DiagnosticadosContagiosos + modelo$ciclos[[i-1]]$InfectadoDx$Personas[j]
      NoDiagnosticadosContagiosos <-
        NoDiagnosticadosContagiosos + modelo$ciclos[[i-1]]$InfectadoNoDx$Personas[j] + modelo$ciclos[[i]]$InfectadoPreDx$Personas[j]
    }
    
    
  }##aca cierro loop j
  
    #print(paste("$MuerteHIV$Total",i,modelo$ciclos[[i]]$MuerteHIV$Total))
  # Actualizar el total de casos de VIH diagnosticados y casos de VIH
  modelo$casosHIVDx <-
    modelo$casosHIVDx + modelo$ciclos[[i]]$nuevosCasosDx
  modelo$casosHIV <-
    modelo$casosHIV + modelo$ciclos[[i]]$nuevosCasos
  
  # Asegúrate de que 'i' esté dentro del rango válido para 'modelo$ciclos'
  if (i <= length(modelo$ciclos)) {
    campos <- c("SanoSinPrEP$Total", "SanosOffPrEPTotal", "SanosOnPrEPTotal", "InfectadoNoDx$Total",
                "InfectadoPreDx$Total", "InfectadoDx$Total", "MuerteGeneral$Total", "MuerteHIV$Total",
                "InfectadosTotal", "nuevosCasos", "CasosAcumulados", "nuevosCasosDx", "qalyNOHIV", 
                "qalyHIV", "LyPerdidosHIVMP", "QalyPerdidoHIVMP", "Disutilidad", "CostoSano", 
                "CostoPrep", "CostoHIV")
    
    for (campo in campos) {
      ruta <- paste0("modelo$ciclos[[", i, "]]$", campo)
      # Comprobar primero si la ruta existe
      if (!exists(ruta, where = modelo$ciclos[[i]], inherits = FALSE)) {
        # Si no existe, asignar 0 al campo correspondiente
        eval(parse(text = paste0(ruta, " <- 0")))
      } else {
        # Si existe, entonces comprueba si es NA
        if (is.na(eval(parse(text = ruta)))) {
          eval(parse(text = paste0(ruta, " <- 0")))
        }
      }
    }
    
  }
  
  if (escribir) {

    cat(i,
          anosPasados,
          modelo$ciclos[[i]]$SanoSinPrEP$Total,
          modelo$ciclos[[i]]$SanosOffPrEPTotal, 
          modelo$ciclos[[i]]$SanosOnPrEPTotal,
          modelo$ciclos[[i]]$InfectadoNoDxTotal, 
          modelo$ciclos[[i]]$InfectadoPreDx$Total,
          modelo$ciclos[[i]]$InfectadoDx$Total, 
          (modelo$ciclos[[i]]$MuerteGeneral$Total + finalizados),
          modelo$ciclos[[i-1]]$MuerteHIV$Total,
          personasRiesgo,
          modelo$ciclos[[i]]$InfectadosTotal,
          modelo$ciclos[[i]]$nuevosCasos, 
          modelo$ciclos[[i]]$CasosAcumulados,
          modelo$ciclos[[i]]$nuevosCasosDx,
          modelo$ciclos[[i]]$qalyNOHIV, 
          modelo$ciclos[[i]]$qalyHIV,
          modelo$ciclos[[i]]$LyPerdidosHIVMP,
          modelo$ciclos[[i]]$QalyPerdidoHIVMP, 
          modelo$ciclos[[i]]$Disutilidad, 
          modelo$ciclos[[i]]$CostoSano, 
          modelo$ciclos[[i]]$costoPrEP, 
          modelo$ciclos[[i]]$CostoHIV)
    
     escribirOutput(i,
                   ciclosPorAño,
                   modelo$ciclos[[i]]$SanoSinPrEP$Total,
                   modelo$ciclos[[i]]$SanosOffPrEPTotal,
                   modelo$ciclos[[i]]$SanosOnPrEPTotal,
                   modelo$ciclos[[i]]$InfectadoNoDx$Total,
                   modelo$ciclos[[i]]$InfectadoPreDx$Total,
                   modelo$ciclos[[i]]$InfectadoDx$Total,
                   (modelo$ciclos[[i]]$MuerteGeneral$Total + finalizados),
                   modelo$ciclos[[i]]$MuerteHIV$Total,
                   personasRiesgo,
                   modelo$ciclos[[i]]$InfectadosTotal,
                   modelo$ciclos[[i]]$nuevosCasos,
                   modelo$ciclos[[i]]$CasosAcumulados,
                   modelo$ciclos[[i]]$nuevosCasosDx,
                   modelo$ciclos[[i]]$qalyNOHIV,
                   modelo$ciclos[[i]]$qalyHIV,
                   modelo$ciclos[[i]]$LyPerdidosHIVMP,
                   modelo$ciclos[[i]]$QalyPerdidoHIVMP,
                   modelo$ciclos[[i]]$Disutilidad,
                   modelo$ciclos[[i]]$CostoSano,
                   modelo$ciclos[[i]]$costoPrEP,
                   modelo$ciclos[[i]]$CostoHIV)
  }
  
  
  # Calcular los años vividos y descontados
  
  modelo$anosVividos <-
    modelo$anosVividos + (
      modelo$ciclos[[i]]$SanoSinPrEP$Total +
        modelo$ciclos[[i]]$SanosOffPrEPTotal + modelo$ciclos[[i]]$SanosOnPrEPTotal +
        modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoPreDx$Total +
        modelo$ciclos[[i]]$InfectadoDx$Total
    ) / ciclosPorAño
  modelo$anosVividosD <-
    modelo$anosVividosD + (((
      modelo$ciclos[[i]]$SanoSinPrEP$Total +
        modelo$ciclos[[i]]$SanosOffPrEPTotal + modelo$ciclos[[i]]$SanosOnPrEPTotal +
        modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoPreDx$Total +
        modelo$ciclos[[i]]$InfectadoDx$Total
    ) / ciclosPorAño
    ) / (1 + tasaDescuento) ^ i)
  
  # Calcular QALYs vividos y descontados
  modelo$qalysVividos <-
    modelo$qalysVividos + modelo$ciclos[[i]]$qalyHIV + modelo$ciclos[[i]]$qalyNOHIV
  modelo$qalysVividosD <-
    modelo$qalysVividosD + ((modelo$ciclos[[i]]$qalyHIV + modelo$ciclos[[i]]$qalyNOHIV) / ((1 + tasaDescuento) ^ i))
  
  modelo$casosHIVDx <-
    modelo$casosHIVDx + modelo$ciclos[[i]]$nuevosCasosDx
  modelo$casosHIV <-
    modelo$casosHIV + modelo$ciclos[[i]]$nuevosCasos
  
  # Calcular los años vividos y descontados
  
  modelo$anosVividos <-
    modelo$anosVividos + (
      modelo$ciclos[[i]]$SanoSinPrEP$Total +
        modelo$ciclos[[i]]$SanosOffPrEPTotal + modelo$ciclos[[i]]$SanosOnPrEPTotal +
        modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoPreDx$Total +
        modelo$ciclos[[i]]$InfectadoDx$Total
    ) / ciclosPorAño
  
  #print(paste("anos_vividos",modelo$anosVividos))
  
  modelo$anosVividosD <-
    modelo$anosVividosD + (((
      modelo$ciclos[[i]]$SanoSinPrEP$Total +
        modelo$ciclos[[i]]$SanosOffPrEPTotal + modelo$ciclos[[i]]$SanosOnPrEPTotal +
        modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoPreDx$Total +
        modelo$ciclos[[i]]$InfectadoDx$Total
    ) / ciclosPorAño
    ) / (1 + tasaDescuento) ^ i)
  
  # Calcular QALYs vividos y descontados
  modelo$qalysVividos <-
    modelo$qalysVividos + modelo$ciclos[[i]]$qalyHIV + modelo$ciclos[[i]]$qalyNOHIV
  modelo$qalysVividosD <-
    modelo$qalysVividosD + ((modelo$ciclos[[i]]$qalyHIV + modelo$ciclos[[i]]$qalyNOHIV) / ((1 + tasaDescuento) ^ i))
  
  
  # Actualizar los años de vida perdidos debido al VIH y sus años descontados
  modelo$anosPerdidosMPHIV <-
    modelo$anosPerdidosMPHIV + modelo$ciclos[[i]]$LyPerdidosHIVMP
  modelo$anosPerdidosMPHIVD <-
    modelo$anosPerdidosMPHIVD + (modelo$ciclos[[i]]$LyPerdidosHIVMP / (1 + tasaDescuento) ^ i)
  
  # Actualizar QALYs perdidos debido al VIH y sus QALYs descontados
  modelo$qalysPerdidosMPHIV <-
    modelo$qalysPerdidosMPHIV + modelo$ciclos[[i]]$QalyPerdidoHIVMP
  modelo$qalysPerdidosMPHIVD <-
    modelo$qalysPerdidosMPHIVD + (modelo$ciclos[[i]]$QalyPerdidoHIVMP / (1 + tasaDescuento) ^ i)
  
  # Actualizar QALYs perdidos debido a la discapacidad relacionada con el VIH y sus QALYs descontados
  modelo$qalysPerdidosDiscHIV <-
    modelo$qalysPerdidosDiscHIV + modelo$ciclos[[i]]$Disutilidad
  modelo$qalysPerdidosDiscHIVD <-
    modelo$qalysPerdidosDiscHIVD + (modelo$ciclos[[i]]$Disutilidad / (1 + tasaDescuento) ^ i)
  
  # Actualizar QALYs perdidos totales y descontados
  modelo$qalysPerdidos <-
    modelo$qalysPerdidos + modelo$ciclos[[i]]$QalyPerdidoHIVMP + modelo$ciclos[[i]]$Disutilidad
  modelo$qalysPerdidosD <-
    modelo$qalysPerdidosD + (modelo$ciclos[[i]]$QalyPerdidoHIVMP + modelo$ciclos[[i]]$Disutilidad) / (1 + tasaDescuento) ^ i
  
  # Actualizar costos relacionados con el VIH, sanos y PrEP, y sus costos descontados
  modelo$CostoHIV <- modelo$CostoHIV + modelo$ciclos[[i]]$CostoHIV
  modelo$costoHIVD <-
    modelo$costoHIVD + (modelo$ciclos[[i]]$CostoHIV / (1 + tasaDescuento) ^ i)
  modelo$CostoSano <-
    modelo$CostoSano + modelo$ciclos[[i-1]]$CostoSano
  modelo$costoSanoD <-
    modelo$costoSanoD + (modelo$ciclos[[i-1]]$CostoSano / (1 + tasaDescuento) ^ i)
  
  
  modelo$CostoPrEP <-
    modelo$CostoPrEP + modelo$ciclos[[i]]$CostoPrEP
  modelo$CostoPrEPD <-
    modelo$CostoPrEPD + (modelo$ciclos[[i]]$CostoPrEP / (1 + tasaDescuento) ^ i)
  
  # Actualizar tiempo sin diagnóstico
  modelo$TiempoSinDx <-
    modelo$TiempoSinDx + (modelo$ciclos[[i]]$InfectadoPreDx$Total * (12 / ciclosPorAño))
  
  
  
}#termina loop i



###final fuera de loop i

# Actualizar años vividos por la cohorte
modelo$anosVividos <- modelo$anosVividos + (cohorteSize / ciclosPorAño)

# Actualizar el total de muertes por VIH
modelo$muertesHIV <- modelo$ciclos[[numeroCiclos-1]]$MuerteHIV$Total

# Actualizar el total de muertes (incluyendo VIH, muertes generales y otras finalizaciones)
modelo$muertesTotales <- modelo$ciclos[[numeroCiclos]]$MuerteHIV$Total + modelo$ciclos[[numeroCiclos]]$MuerteGeneral$Total + finalizados

escenario <- 2
#### fn comapra resultado
resultados <- escribirResultadosComparacion(
  escenario, 
  modelo$anosVividos, 
  modelo$qalysVividos, 
  modelo$anosPerdidosMPHIV, 
  modelo$qalysPerdidosMPHIV, 
  modelo$qalysPerdidosDiscHIV, 
  modelo$qalysPerdidos, 
  modelo$muertesHIV, 
  modelo$muertesTotales, 
  modelo$casosHIV, 
  modelo$casosHIVDx, 
  modelo$TiempoSinDx, 
  modelo$CostoSano, 
  modelo$CostoPrEP, 
  modelo$CostoHIV, 
  modelo$casosHIV + modelo$ciclos[[1]]$InfectadosTotal, 
  modelo$anosVividosD, 
  modelo$qalysVividosD, 
  modelo$anosPerdidosMPHIVD, 
  modelo$qalysPerdidosMPHIVD, 
  modelo$qalysPerdidosDiscHIVD, 
  modelo$qalysPerdidosD, 
  modelo$costoSanoD, 
  modelo$costoPrEPD, 
  modelo$costoHIVD, 
  cohorteSizeFinal
)







