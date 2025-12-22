options(scipen=999)
source("models/prep/FUNCIONES.R")
input_prep <- readxl::read_excel("models/prep/data/inputs_prep.xlsx", col_types = c("text","text","text","numeric","skip","skip","skip","skip"))

funcionPrincipal <- function(linea,paisCol, parametro){
  #[LEAN2]
  #Parametros Hardcodeados de configuración.
  cohorteSize = 100000
  tipoCohorte = 1
  tipoDuracion = 1
  calculoPorRiesgo <- 0
  riesgoHIV <- 0
  anosMaximosPrEP <- 5
  anosHastaClinica <- 10
  anosHastaComplicacion <- 10
  cohorteDinamicaIngresanEnfermos <- 1
  prevalenciaAnoMinimo = 0.0105
  edadFinal = 100
  limiteEdadRiesgo = 55
  limiteEdadContagiosos = 55
# Usando list2env para crear variables en el entorno global
list2env(parametro, envir = .GlobalEnv)

input_prep <- readxl::read_excel("models/prep/data/inputs_prep.xlsx", col_types = c("text","text","text","numeric","skip","skip","skip","skip"))
# datos_paises <- readxl::read_excel("prep/data/Datos_paises.xlsx")
# utilidades <- readxl::read_excel("prep/data/utilidades.xlsx")
# utilidades2 <- readxl::read_excel("prep/data/utilidades.xlsx", sheet = "Hoja2")
probabilidades <- readxl::read_excel("models/prep/data/probabilidades.xlsx")
#parametros <- readxl::read_excel("prep/data/parametros.xlsx")
#costos <- readxl::read_excel("prep/data/Costos.xlsx")
#probabilidad_muerte <- readxl::read_excel("prep/data/PROBABILIDAD_MUERTE.xlsx")
probabilidad_muerte <-input_prep %>% dplyr::filter(PAIS==paisCol & tipo=="PROBABILIDAD_MUERTE") %>% dplyr::select(VALOR) %>% as.data.frame()
distribucion_cohortes <- readxl::read_excel("models/prep/data/DISTRIBUCION_COHORTES.xlsx")
utilidades2 <- input_prep %>% dplyr::filter(PAIS==paisCol & tipo=="UTILIDAD") %>% dplyr::select(VALOR) %>% as.data.frame()
#[LEAN 17/1]

#casosIncidentes = input_prep %>%  dplyr::filter(PAIS==paisCol & PARAMETRO=="Nuevos casos HIV en poblacion") %>% dplyr::select(VALOR) %>% as.numeric()
#casosPrevalentes = input_prep %>% dplyr::filter(PAIS==paisCol & PARAMETRO=="Casos Prevalentes HIV en poblacion") %>% dplyr::select(VALOR) %>% as.numeric()


esperanzaVida = input_prep %>% dplyr::filter(PAIS==paisCol & PARAMETRO=="Esperanza de Vida") %>% dplyr::select(VALOR) %>% as.numeric()


## PARAMETRO QUE NO ESTA EN LOS INPUTS
rrHIVDiagnosticadoNoTratados = 0.43 # ver luego


#'Seteos basales
testPorAno = 1 #definodo en hoja 6
escribir = "True"
ciclosPorAno = 4 #El modelo hace ciclos trimestrales
tasaDescuento = descuento / ciclosPorAno #Calculamos la tasa de descuento trimestral.


uHIV = input_prep %>% 
  dplyr::filter(PAIS=="GLOBAL" & tipo=="UTILIDAD" & PARAMETRO=="Early HIV") %>%
  dplyr::select(VALOR) %>% as.numeric()
uHIVTTO = input_prep %>% dplyr::filter(PAIS=="GLOBAL" & tipo=="UTILIDAD" & PARAMETRO=="TTO") %>%  dplyr::select(VALOR) %>% as.numeric()
  

cuHIV = uHIV / ciclosPorAno 
cuHIVTTO = uHIVTTO / ciclosPorAno 

##1

#[Modificado LEAN]
uPoblacion <- numeric(101)  # Crear un vector para almacenar los datos
cuPoblacion <- numeric(101)


for (z in 0:100) {
  uPoblacion[z + 1] <- utilidades2[z + 1,1]
  cuPoblacion[z + 1] <- uPoblacion[[z + 1]] / ciclosPorAno
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

cDiagnostico =input_prep %>% 
  dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="DIAGNOSTICO") %>%
  dplyr::select(VALOR) %>% as.numeric()



cComplicacionesHIV = cComplicacionesHIV /ciclosPorAno
cSeguimientoHIV <- cSeguimientoHIV_anual/ciclosPorAno
cTratamientoHIV <- cTratamientoHIV_anual/ciclosPorAno
cPrEPTratamiento <- cPrEPTratamiento_anual/ciclosPorAno
cPrEPSeguimiento <- cPrEPSeguimiento_anual/ciclosPorAno
cPrEPTest <-  cPrEPTest_anual/ciclosPorAno




# 'Cargamos la array de utilidad restante.
utilidadRestante = calcularUtilidadRestante(ciclosPorAno, esperanzaVida, cuPoblacion)


#'Calculamos cada cuantos ciclos se testea.
testeoNoPrEP = ciclosPorAno / testPorAno

#'Asignamos el tamaño final de la cohorte al tamaño inicial.

cohorteSizeFinal = cohorteSize

if (calculoPorRiesgo == 0)
{
  

  casosPrevalentes <- 100000
  casosIncidentes <- casosPrevalentes * ratio
  
  #Ajustamos por los diagnosticos de ese año.
  casosDiagnosticados = casosPrevalentes * (pHIVDiagnosticado - ratio)
  
  casosNoDiagnosticados = casosPrevalentes - casosDiagnosticados
  casosTratados = casosDiagnosticados * pHIVTratado
  # 'Estos son los casos no tratados dentro de los diagnosticados...
  casosNoTratados = casosDiagnosticados - casosTratados
  # 'Casos no controlados dentro de los tratados.
  casosNoControlados = casosTratados * (1 - pHIVControlado)
  
  #'Calculamos la tasaTransmision para pacientes infectados no diagnosticados.
  
  # sacamos el ajuste por pHIVdiagnosticado pq en realidad eso impacta en ambos lados.
  #tasaTransmision <- (casosIncidentes / pHIVDiagnosticado) / (casosNoDiagnosticados + (rrHIVDiagnosticadoNoTratados * (casosNoTratados + casosNoControlados)))
  tasaTransmision <- (casosIncidentes) / (casosNoDiagnosticados + (rrHIVDiagnosticadoNoTratados * (casosNoTratados + casosNoControlados)))
  
  #'Calculamos la tasa de transmision para pacientes diagnosticados pero no tratados y para aquellos tratados no controlados.
  tasaTransmisionNoControlados = tasaTransmision * rrHIVDiagnosticadoNoTratados

}
else
{
  
  casosDiagnosticados = (cohorteSize * prevalenciaHIV) * pHIVDiagnosticado
  casosNoDiagnosticados = (cohorteSize * prevalenciaHIV) - casosDiagnosticados
  casosTratados = casosDiagnosticados * pHIVTratado
  casosNoTratados = casosDiagnosticados - casosTratados #Estos son los casos no tratados dentro de los diagnosticados...
  casosNoControlados = casosTratados * (1 - pHIVControlado) #Casos no controlados dentro de los tratados.
  
  #Calculamos la tasaTransmision para pacientes infectados no diagnosticados.
  tasaTransmision = (((cohorteSize - (cohorteSize * prevalenciaHIV)) * riesgoHIV)) / (casosNoDiagnosticados + (rrHIVDiagnosticadoNoTratados * (casosNoTratados + casosNoControlados)))
  #Calculamos la tasa de transmision para pacientes diagnosticados pero no tratados y para aquellos tratados no controlados.
  tasaTransmisionNoControlados = tasaTransmision * rrHIVDiagnosticadoNoTratados
  
  
}

#'Definimos el numero de años que durara el modelo, el mismo corre hasta que el menor de la cohorte cumpla 100 años.
anosRango = edadFinal - edadMinima
#Definimos el numero de ciclos.
numeroCiclos = anosRango * ciclosPorAno


if (duracionPrEP == 0)#[LEAN2]
{
  ciclosPrEPON <- 5
  ciclosPrEPOFF <- 5
# Definimos la configuración
} else if (tipoDuracion == 0) {
  # Un usuario recibe PrEP por la duración que hayan asignado
  if (duracionPrEP == 99) {
    # La intervención dura hasta la edad de fin de indicación de PrEP
    # El estadio onPrep tiene 5 años de túneles para tener probabilidades distintas
    # Al llegar al último año se asume que es la probabilidad final de abandono de PrEP
    ciclosPrEPON <- 5 * ciclosPorAno
  } else {
    # Si duración PrEP es distinta a 99 se le da un túnel por cada año
    ciclosPrEPON <- duracionPrEP * ciclosPorAno
  }
} else if (tipoDuracion == 1) {
  # La medida dura lo que hayan asignado en duración
  # Es decir, todos reciben PrEP durante el mismo tiempo
  if (anosMaximosPrEP > duracionPrEP | anosMaximosPrEP == 0)
  {
    anosMaximosPrEP = duracionPrEP
  }
  if (anosMaximosPrEP < 5)
  {
    ciclosPrEPON <- anosMaximosPrEP * ciclosPorAno
  } else {
    ciclosPrEPON <- 5 * ciclosPorAno
  }
}

#ciclosPrEPoff siempre tiene 5 años d tuneles con distintas probabilidades.
ciclosPrEPOFF = 5 * ciclosPorAno

pAbandonoPrEP <- list()
for (z in 1:ciclosPrEPON) {
  if (z <= 5 * ciclosPorAno)
  {
    rTemp <- -log(1 - as.numeric(aAbandonoPrEP[((z - 1) %/% ciclosPorAno) + 1]))
    pAbandonoPrEP[z] <- 1 - exp(-rTemp * (1 / ciclosPorAno))
  }
  else
    
  {
    pAbandonoPrEP[z] <- pAbandonoPrEP[5 * ciclosPorAno]
  }
}

pReiniciarPrEP <- list()
for (z in 1:ciclosPrEPOFF) {
  if (z <= 5 * ciclosPorAno)
  {
    rTemp <- -log(1 - as.numeric(aReiniciarPrEP[((z - 1) %/% ciclosPorAno) + 1]))
    pReiniciarPrEP[z] <- 1 - exp(-rTemp * (1 / ciclosPorAno))
  }
  else
  {
    pReiniciarPrEP[z] <- pReiniciarPrEP[5 * ciclosPorAno]
    
  }
}

pMuerteGeneral <- NULL
pMuerteGeneral <- numeric(101)  
for (z in 1:86) {
  pMuerteGeneral[14+z] <- 1 - exp(-probabilidad_muerte[z , 1] * (1 / ciclosPorAno))
  print(pMuerteGeneral[14+z])
}

pMuerteGeneral

pMuerteHIV <- numeric(101) 
for (z in 1:86) {
  tasaMortalidad <- probabilidades[z, 6]  
  pMuerteHIV[14 + z] <- 1 - exp(-(-log(1 - tasaMortalidad)) * (1 / ciclosPorAno))
  print(pMuerteHIV[14 + z])
}

##

distribucionCohorte <- cargarDistribucion(edadMinima, edadMaximaInicial, distribucion_cohortes)


##creo modelo #######
modelo <- list()
modelo$ciclos <- vector("list", numeroCiclos + 1)  # +1 porque R indexa desde 1



# Inicializar estructuras para el primer ciclo
modelo$ciclos[[1]]$SanoOnPrEP <- vector("list", ciclosPrEPON)
modelo$ciclos[[1]]$SanoOffPrEP <- vector("list", ciclosPrEPOFF)
modelo$ciclos[[1]]$SanoSinPrEP <- list(Personas = numeric(edadFinal))

modelo$ciclos[[1]]$InfectadoDx <- vector("list", ciclosPorAno * anosHastaComplicacion)
modelo$ciclos[[1]]$InfectadoNoDx <- vector("list", ciclosPorAno * anosHastaClinica)
modelo$ciclos[[1]]$InfectadoPreDx <- list(Personas = numeric(edadFinal))
modelo$ciclos[[1]]$MuerteGeneral <- list(Personas = numeric(edadFinal))
modelo$ciclos[[1]]$MuerteHIV <- list(Personas = numeric(edadFinal))

modelo$ciclos[[1]]$nuevosCasos <- 0
modelo$ciclos[[1]]$nuevosCasosDx <- 0
modelo$ciclos[[1]]$CostoPrEP <- 0
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
modelo$ciclos[[i]]$InfectadoNoDxTotal=0
modelo$ciclos[[1]]$InfectadoPreDx$Total=0
modelo$ciclos[[1]]$InfectadoDxTotal=0
modelo$ciclos[[1]]$MuerteGeneral$Total=0
modelo$ciclos[[1]]$MuerteHIV$Total=0

for (z in 1:ciclosPrEPON) {
  modelo$ciclos[[1]]$SanoOnPrEP[[z]]$Personas <- numeric(edadFinal)
}


for (z in 1:ciclosPrEPOFF) {
  modelo$ciclos[[1]]$SanoOffPrEP[[z]]$Personas <- numeric(edadFinal)
}

for (z in 1:(ciclosPorAno * anosHastaComplicacion)) {
  modelo$ciclos[[1]]$InfectadoDx[[z]]$Personas <- numeric(edadFinal)
  
}
for (z in 1:(ciclosPorAno * anosHastaClinica)) {
  modelo$ciclos[[1]]$InfectadoNoDx[[z]]$Personas <- numeric(edadFinal)
  print(modelo$ciclos[[1]]$InfectadoNoDx[[z]]$Personas[18])
}

prevalencia <- list()

for (z in 1:100)
{
  if (z >= 15)
  {
    prevalencia[z] = as.numeric(distribucion_cohortes[5 + (z - 14), 10])
  }
  else
  {
    prevalencia[z] = 0
  }
}

prevInfectados <- 0
for (z in edadMinima:edadMaximaInicial)
{
  prevInfectados = prevInfectados + (cohorteSize * distribucionCohorte[z] * prevalencia[[z]])
}

# Asignar valores iniciales

#modelo$ciclos[[1]]$SanosTotal <- cohorteSize * (1 - prevalenciaHIV)
modelo$ciclos[[1]]$SanosTotal <- cohorteSize - prevInfectados
modelo$ciclos[[1]]$SanosOnPrEPTotal <- modelo$ciclos[[1]]$SanosTotal * PrEPuptake
modelo$ciclos[[1]]$SanosOffPrEPTotal<- 0
modelo$ciclos[[1]]$SanoSinPrEP$Total <- modelo$ciclos[[1]]$SanosTotal * (1 - PrEPuptake)
#modelo$ciclos[[1]]$InfectadosTotal <- cohorteSize * prevalenciaHIV
modelo$ciclos[[1]]$InfectadosTotal <- prevInfectados

#modelo$ciclos[[1]]$InfectadoDx$Total <- modelo$ciclos[[1]]$InfectadosTotal * pHIVDiagnosticado
modelo$ciclos[[1]]$InfectadoDxTotal <- modelo$ciclos[[1]]$InfectadosTotal * pHIVDiagnosticado
modelo$ciclos[[1]]$InfectadoNoDxTotal <- modelo$ciclos[[1]]$InfectadosTotal * (1 - pHIVDiagnosticado)

modelo$ciclos[[1]]$MuerteGeneral$Total <-0
#modelo$ciclos[[2]]$MuerteGeneral$Total <-0
modelo$ciclos[[1]]$InfectadoPreDx$Total <- 0
# Distribuir la población inicial en los estados
totalCheck <- 0

#Esto es innecesario correrlo desde 1, deberia correrse desde edad minima. Pero si lo corres asi en cada ciclo son 18 iteraciones innecesarias por ciclo.
for (i in 1 : edadMaximaInicial) {
  
  modelo$ciclos[[1]]$SanoOnPrEP[[1]]$Personas[i] <- modelo$ciclos[[1]]$SanosOnPrEPTotal * distribucionCohorte[i]
  modelo$ciclos[[1]]$SanoSinPrEP$Personas[i] <- modelo$ciclos[[1]]$SanoSinPrEP$Total * distribucionCohorte[i]
  
  modelo$ciclos[[1]]$InfectadoDx[[1]]$Personas[i] <- (cohorteSize * distribucionCohorte[i] * prevalencia[[i]]) * pHIVDiagnosticado   #modelo$ciclos[[1]]$InfectadoDx$Total * distribucionCohorte[i]
  modelo$ciclos[[1]]$InfectadoNoDx[[1]]$Personas[i] <- (cohorteSize * distribucionCohorte[i] * prevalencia[[i]]) * (1 - pHIVDiagnosticado)  #modelo$ciclos[[1]]$InfectadoNoDxTotal * distribucionCohorte[i]
  totalCheck <- sum(totalCheck , modelo$ciclos[[1]]$SanoOnPrEP[[1]]$Personas[i] , 
    modelo$ciclos[[1]]$SanoSinPrEP$Personas[i],
    modelo$ciclos[[1]]$InfectadoDx[[1]]$Personas[i] , 
    modelo$ciclos[[1]]$InfectadoNoDx[[1]]$Personas[i], na.rm = T)
  
  
}


# Manejar la dinámica de la cohorte

if (tipoCohorte == 1)
{
  genteEntrante <- cohorteSize  * distribucionCohorte[edadMinima]
  
  if (cohorteDinamicaIngresanEnfermos == 1)
  {
    enfermoEntrante <- genteEntrante * prevalenciaAnoMinimo
    sanoEntrante <- genteEntrante - enfermoEntrante
  }
  else
  {
    sanoEntrante <- genteEntrante
  }
}
else
{
  genteEntrante <- 0
  sanoEntrante <- 0
  enfermoEntrante <- 0
}






# Calcular total de infectados
modelo$ciclos[[1]]$InfectadosTotal <- modelo$ciclos[[1]]$InfectadoDxTotal + modelo$ciclos[[1]]$InfectadoNoDxTotal


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
DiagnosticadosContagiosos <- modelo$ciclos[[1]]$InfectadoDxTotal
NoDiagnosticadosContagiosos <- modelo$ciclos[[1]]$InfectadoNoDxTotal + modelo$ciclos[[1]]$InfectadoPreDx$Total

escribir <- TRUE
i <- 1


modelo$CostoPrEP <-0
if (escribir) {
  # Llamada a la función 'escribirOutput' con los argumentos correspondientes
  # Aquí asumimos que todas las propiedades (.Total, etc.) están definidas adecuadamente
  result <- escribirOutput(0, ciclosPorAno, modelo$ciclos[[i]]$SanoSinPrEP$Total, modelo$ciclos[[i]]$SanosOffPrEPTotal, 
                 modelo$ciclos[[i]]$SanosOnPrEPTotal, modelo$ciclos[[i]]$InfectadoNoDxTotal, 
                 modelo$ciclos[[i]]$InfectadoPreDx$Total, modelo$ciclos[[i]]$InfectadoDxTotal, 
                 modelo$ciclos[[i]]$MuerteGeneral$Total, modelo$ciclos[[i]]$MuerteHIV$Total, 
                 personasRiesgo, modelo$ciclos[[i]]$InfectadosTotal, 0, 0, modelo$ciclos[[i]]$nuevosCasosDx, 
                 0, 0, 0, 0, 0, 0, 0, 0)
}


# Establecer la edad inicial para el loop

edadLoopInicial <- edadMinima
# Si la cohorte es dinámica, la edad inicial siempre será edadMinima.
# Si no es dinámica, la lógica para ajustar edadLoopInicial deberá ser añadida.

cicloCounter <- 0
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
modelo$CostoHIVD <-0

modelo$CostoPrEPD <-costoProgramatico / ciclosPorAno
modelo$CostoPrEP <- costoProgramatico / ciclosPorAno

modelo$CostoSano <- 0
modelo$CostoSanoD <- 0
modelo$qalysPerdidosD <-0
modelo$anosPerdidosMPHIVD <-0


#### inicia loop i #####
anosPasados <- 0
#numeroCiclos

for (i in 2:(numeroCiclos+1)) {

  #browser()
  # # Inicializar las estructuras para el i-ésimo ciclo

  

  modelo$ciclos[[i]]$SanoSinPrEP <- list(Personas = numeric(edadFinal), Total = 0)
  modelo$ciclos[[i]]$SanoOnPrEP <- vector("list", ciclosPrEPON)
  modelo$ciclos[[i]]$SanoOffPrEP <- vector("list", ciclosPrEPOFF)
  modelo$ciclos[[i]]$InfectadoDx <- vector("list", ciclosPorAno * anosHastaComplicacion) #list(Personas = numeric(edadFinal), Total = 0)#[LEAN2]
  modelo$ciclos[[i]]$InfectadoPreDx <-list(Personas = numeric(edadFinal),  Total = 0)
  modelo$ciclos[[i]]$InfectadoNoDx <- vector("list", ciclosPorAno * anosHastaClinica)  # list(Personas = numeric(edadFinal)) #[LEAN2]
  modelo$ciclos[[i]]$MuerteGeneral <-list(Personas = numeric(edadFinal))
  modelo$ciclos[[i]]$MuerteHIV <- list(Personas = numeric(edadFinal))
  modelo$ciclos[[i]]$nuevosCasos<- 0
  modelo$ciclos[[i]]$nuevosCasosDx<- 0
  modelo$ciclos[[i]]$InfectadoDxTotal <-0 
  modelo$ciclos[[i]]$InfectadoPreDx$Total <-0
  modelo$ciclos[[i]]$InfectadoNoDxTotal <-0
  modelo$ciclos[[i]]$SanoSinPrEP$Total <-0
  modelo$ciclos[[i]]$SanosOffPrEPTotal <-0
  modelo$ciclos[[i]]$SanosOnPrEPTotal <-0
  modelo$ciclos[[i]]$qalyHIV <-0
  modelo$ciclos[[i]]$qalyNOHIV <-0
  modelo$ciclos[[i]]$LyPerdidosHIVMP <- 0
  modelo$ciclos[[i]]$QalyPerdidoHIVMP <- 0
  modelo$ciclos[[i]]$Disutilidad <- 0
  modelo$ciclos[[i]]$CostoHIV <- 0
  modelo$ciclos[[i]]$CostoPrEP <- 0
  modelo$ciclos[[i]]$CostoSano <- 0
  for (z in 1:(ciclosPrEPON)) {
    modelo$ciclos[[i]]$SanoOnPrEP[[z]]$Personas <-numeric(edadFinal)
  }
  for (z in 1:(ciclosPrEPOFF)) {
    modelo$ciclos[[i]]$SanoOffPrEP[[z]]$Personas <- numeric(edadFinal)
  }
  #[LEAN2]
  for (z in 1:(ciclosPorAno * anosHastaClinica)) {
    modelo$ciclos[[i]]$InfectadoNoDx[[z]]$Personas <- numeric(edadFinal)
  }
  for (z in 1:(ciclosPorAno * anosHastaComplicacion)) {
    modelo$ciclos[[i]]$InfectadoDx[[z]]$Personas <- numeric(edadFinal)
  }
  #[/LEAN2]
  
  # Lógica para el contador de ciclos
  if (cicloCounter == 4) {
    cicloCounter <- 0
  }
  cicloCounter <- cicloCounter + 1
  

  #anoOffset <- 1
  # Si estamos en un ciclo múltiplo de 4, significa que ha pasado 1 año
  if (i %in% seq(5, numeroCiclos + 1, by = ciclosPorAno)) {
    anoOffset <- 1
    # Establecemos anoOffset en 1 para poder envejecer la población
    anosPasados <- anosPasados + 1
    # Sumamos un año
    # Si la cohorte es dinámica, la edad inicial del bucle sigue siendo la edad mínima
    if (tipoCohorte == 1) {
      edadLoopInicial <- edadMinima
      # Ha pasado 1 año, necesitamos ingresar nuevas personas a los estados
      if (tipoDuracion == 0 || (tipoDuracion == 1 && i < ciclosPorAno * duracionPrEP)) {
        modelo$ciclos[[i]]$SanoOnPrEP[[1]]$Personas[edadMinima] <- sanoEntrante * PrEPuptake 
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[edadMinima] <- sanoEntrante * (1 - PrEPuptake)
      } else {
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[edadMinima] <- sanoEntrante
      }
      # Ajustamos el tamaño de la cohorte final
      cohorteSizeFinal <- cohorteSizeFinal + genteEntrante
    } else if (tipoCohorte == 0) {
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
  
  if (personasRiesgo > 0) {
    
    
    if (calculoPorRiesgo == 2)
    {
      pAnualHIV <- riesgoHIV
    }
    else
    {
      # Calculamos la probabilidad anual de tener HIV
      pAnualHIV <- ((tasaTransmision * NoDiagnosticadosContagiosos) +
                      (tasaTransmisionNoControlados * ((DiagnosticadosContagiosos * (1 - pHIVTratado)) +
                                                         (DiagnosticadosContagiosos * pHIVTratado * (1 - pHIVControlado))))) / personasRiesgo
    }#[/LEAN2]

    # Limitamos la probabilidad anual a un máximo de 0.99
    if (pAnualHIV >= 1) {
      pAnualHIV <- 0.99
    }
    
    # Convertimos la probabilidad en tasa
    rAnualHIV <- -log(1 - pAnualHIV) # dividido por 1, que es equivalente a no cambiarlo
    
    # Recalculamos la probabilidad en el ciclo
    pCicloHIV <- 1 - exp(-rAnualHIV * (1 / ciclosPorAno))
  }
  
  

  # Inicializar variables para el siguiente ciclo
  DiagnosticadosContagiosos <- 0
  NoDiagnosticadosContagiosos <- 0
  personasRiesgo <- 0
  
 
  # Actualizar muertes totales y casos acumulados

    modelo$ciclos[[i]]$MuerteGeneral$Total <- modelo$ciclos[[i - 1]]$MuerteGeneral$Total
    modelo$ciclos[[i]]$MuerteHIV$Total <-modelo$ciclos[[i - 1]]$MuerteHIV$Total
    modelo$ciclos[[i]]$CasosAcumulados <-modelo$ciclos[[i - 1]]$CasosAcumulados
  
    #(edadMaximaInicial + anosPasados)
#inicia loop j --------------    
    #((edadMaximaInicial+ anosPasados)- 10)
  for (j in edadLoopInicial:(edadMaximaInicial+ anosPasados)) {

    
    if (j > (edadFinal)) {
      #if (i == 205)
      #{
      #  browser()
      #}
      # Si la edad es mayor que el límite (ej: 100 años), se finaliza el ciclo de esa edad
      if (anoOffset == 1 ) {#[LEAN2]
        finalizados <- finalizados + modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[100] + modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[100]
        for (z in 1:(ciclosPorAno*anosHastaComplicacion))
        {
          finalizados = finalizados + modelo$ciclos[[i - 1]]$InfectadoDx[[z]]$Personas[100]
        }
        for (z in 1:(ciclosPorAno*anosHastaClinica))
        {
          finalizados = finalizados + modelo$ciclos[[i - 1]]$InfectadoNoDx[[z]]$Personas[100]
        }
      }#[/LEAN2]
      break  # Salir del bucle de edad
    }
   
    
      
    # Inicializar contadores para este ciclo
    nuevosInfectadosSinPrEP <- 0
    nuevosInfectadosConPrEP <- 0
    conteoParcialSOFP <- 0
    conteoParcialSOP <- 0
    #[LEAN2]
    InfectadosNoDxParcial <- 0
    InfectadosDxParcial <- 0
    NoDxPasanADx <- 0
    
    if (tipoCohorte == 1 && cohorteDinamicaIngresanEnfermos == 1 && j == edadMinima && anoOffset == 1)
    {
      nuevosInfectadosSinPrEP = enfermoEntrante
    }
    #[/LEAN2]
    
    # Establecer la probabilidad de contagio de HIV a 0 si se supera el límite de edad de riesgo
    if (j > limiteEdadRiesgo) {
      pCicloHIV <- 0
    }
    
    # Manejo de cohortes dinámicas
    if (!(tipoCohorte == 1 && j == edadMinima && anoOffset == 1)) {
      
      # Cálculos para los sanos sin PrEP y ajustes por muerte general y contagio de HIV
  
      
      
      modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
        modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] * pCicloHIV +
            modelo$ciclos[[i - 1]]$SanoSinPrEP$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
        )
      
      # Continuar con más cálculos y lógica dentro del bucle de edad
    
    # borro el i >= duracion porque i aca parte de 2 en este bucle.
    if (j <= edadFinPrEP &&
        !(tipoDuracion == 1 && i > duracionPrEP * ciclosPorAno)) {
      # Si todavía puede reiniciar PrEP
      
      
      #aca ya tiene asignado una cantidad de gente y suma gente a este mismo año [LEAN]
      modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] +
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
    
    #[LEAN] SACO I >= porque I ARRANCA EN 2
    if (j <= edadFinPrEP &&
        !(tipoDuracion == 1 && i > duracionPrEP * ciclosPorAno)) {
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
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[z - 1])
        
        # Agrega los nuevos abandonadores de PrEP al primer estado de SanoOffPrEP
        modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] <-
          modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] +
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pAbandonoPrEP[z - 1])
        
        # Actualiza muertes generales y nuevos infectados
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
          modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
          modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
        
        nuevosInfectadosConPrEP <-
          nuevosInfectadosConPrEP + modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * pCicloHIV * rrPrEP
        
        
        # Contadores para los que entran y salen de PrEP
        conteoParcialSOP <-conteoParcialSOP + modelo$ciclos[[i]]$SanoOnPrEP[[z]]$Personas[j]
        conteoParcialSOFP <-conteoParcialSOFP + modelo$ciclos[[i - 1]]$SanoOnPrEP[[z - 1]]$Personas[j - anoOffset] * as.numeric(as.numeric(pAbandonoPrEP[z - 1]))
      }
    
    
    if (duracionPrEP < 99 && (tipoDuracion == 0 || (anosMaximosPrEP < duracionPrEP))) {
      # Maneja el último estadio de OnPrEP
      modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] +
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
      
      
      modelo$ciclos[[i]]$SanoOffPrEP[[1]]$Personas[j] <-
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
    #ESTO ERA UN ELSE IF del ANTERIOR
    }
    else if (j == edadFinPrEP + 1 &&
        anoOffset == 1 ||
        (tipoDuracion == 1 && i == (duracionPrEP * ciclosPorAno)+1)){
      for (z in 1:ciclosPrEPON) {
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
          modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] +#aca no mira el ciclo anterior sino q lo suma al q esta
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
    
    
    
    #[LEAN] aca es i > porque I arranca en 2 y el bucle es z in 2:(ciclosPrEPOFF) porque lo que hace es ir pasando desde el 
    #ciclo previo al siguiente entonces tiene q llegar al ultimo para poder traer los del anteultimo
    if (j <= edadFinPrEP &&
        !(tipoDuracion == 1 && i > duracionPrEP * ciclosPorAno)) {
      for (z in 2:(ciclosPrEPOFF)) {
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
    #}[LEAN] mismo que antes, aca no cerraba el if.
    }
    else if (j == edadFinPrEP + 1 &&
        anoOffset == 1 ||
        (tipoDuracion == 1 && i == (duracionPrEP * ciclosPorAno)+1)) {#aca es +1 porque i arranca en 2 [LEAN]
      for (z in 1:(ciclosPrEPOFF - 1)) {
        modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] <-
          modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] + #[LEAN] aca es [i] no [i-1] sino perdes a todos los sanos que tenias hasta ese momento.
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
    
    
    for (z in 2:(ciclosPorAno * anosHastaClinica))
    {
      modelo$ciclos[[i]]$InfectadoNoDx[[z]]$Personas[j] <-
        modelo$ciclos[[i - 1]]$InfectadoNoDx[[z - 1]]$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$InfectadoNoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
            modelo$ciclos[[i - 1]]$InfectadoNoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
        )
      InfectadosNoDxParcial = InfectadosNoDxParcial + modelo$ciclos[[i]]$InfectadoNoDx[[z]]$Personas[j]
      
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
        modelo$ciclos[[i - 1]]$InfectadoNoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
        modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
        modelo$ciclos[[i - 1]]$InfectadoNoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    }

    NoDxPasanADx = modelo$ciclos[[i - 1]]$InfectadoNoDx[[ciclosPorAno * anosHastaClinica]]$Personas[j - anoOffset] - (modelo$ciclos[[i - 1]]$InfectadoNoDx[[ciclosPorAno * anosHastaClinica]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) + modelo$ciclos[[i - 1]]$InfectadoNoDx[[ciclosPorAno * anosHastaClinica]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset]))
    
    modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
      modelo$ciclos[[i - 1]]$InfectadoNoDx[[ciclosPorAno * anosHastaClinica]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
      modelo$ciclos[[i - 1]]$InfectadoNoDx[[ciclosPorAno * anosHastaClinica]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    
    modelo$ciclos[[i]]$InfectadoNoDx[[1]]$Personas[j] <- ((nuevosInfectadosSinPrEP) * (1 - pHIVDiagnosticado))
    
    InfectadosNoDxParcial = InfectadosNoDxParcial + modelo$ciclos[[i]]$InfectadoNoDx[[1]]$Personas[j]
    
    for (z in 2:(ciclosPorAno * anosHastaComplicacion))
    {
      # Actualizamos los infectados diagnosticados
      modelo$ciclos[[i]]$InfectadoDx[[z]]$Personas[j] <-
        modelo$ciclos[[i - 1]]$InfectadoDx[[z - 1]]$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$InfectadoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
            modelo$ciclos[[i - 1]]$InfectadoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
        )
      InfectadosDxParcial = InfectadosDxParcial + modelo$ciclos[[i]]$InfectadoDx[[z]]$Personas[j]
      # Agregamos las muertes por causas generales y por HIV dentro de los Infectados dx
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
        modelo$ciclos[[i - 1]]$InfectadoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
        modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
        modelo$ciclos[[i - 1]]$InfectadoDx[[z - 1]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    }
    
    modelo$ciclos[[i]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j] <- modelo$ciclos[[i]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j] +
      modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] -
      (
        modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
          modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
      )
    
    modelo$ciclos[[i]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j] <- modelo$ciclos[[i]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j] + NoDxPasanADx
    
    InfectadosDxParcial = InfectadosDxParcial + NoDxPasanADx + modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] - (modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) + modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset]))
    
    # Agregamos las muertes por causas generales y por HIV dentro de los Infectados dx
    modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
      modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
      modelo$ciclos[[i - 1]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    
    
    
    # Actualizamos los infectados no diagnosticados
    #modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] <-
    #  modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] -
    #  (
    #    modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
    #      modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    #  )
    
    # Los que no están en PrEP tienen un porcentaje de ser diagnosticados
    #modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] <-
    #  modelo$ciclos[[i]]$InfectadoNoDx$Personas[j] +
    #  (nuevosInfectadosSinPrEP * (1 - pHIVDiagnosticado))
    
    # Agregamos las muertes por causas generales y por HIV dentro de los Infectados no dx
    #modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
    #  modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
    #  modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    #modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
    #  modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
    #  modelo$ciclos[[i - 1]]$InfectadoNoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    
    # Actualizamos los infectados diagnosticados
    #modelo$ciclos[[i]]$InfectadoDx$Personas[j] <-
    #  modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] -
    #  (
    #    modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
    #      modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    #  )
    
    # Agregamos las muertes por causas generales y por HIV dentro de los Infectados dx
    #modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
    #  modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
    #  modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
    
    #modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
    #  modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
    #  modelo$ciclos[[i - 1]]$InfectadoDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    
    #[LEAN] aca es i-1 porque arranca en 2 y vos queres que testee en el ciclo 5.
    
    if ((i-1) %% testeoNoPrEP == 0) {
      # Ciclo diagnóstico
      modelo$ciclos[[i]]$InfectadoDx[[1]]$Personas[j] <-
        modelo$ciclos[[i]]$InfectadoDx[[1]]$Personas[j] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
            modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
        )
      
      # Sin nuevos casos en este grupo para este ciclo
      modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] <- 0
      
      # Agregamos los nuevos casos diagnosticados
      modelo$ciclos[[i]]$InfectadoDx[[1]]$Personas[j] <-
        modelo$ciclos[[i]]$InfectadoDx[[1]]$Personas[j] + (nuevosInfectadosSinPrEP * pHIVDiagnosticado) + nuevosInfectadosConPrEP
      
      InfectadosDxParcial = InfectadosDxParcial + ((nuevosInfectadosSinPrEP * pHIVDiagnosticado) + nuevosInfectadosConPrEP) +  modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
            modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
        )
      
      # Agregamos las muertes por causas generales y por HIV dentro de los PreDx
      modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] <-
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
      
      modelo$ciclos[[i]]$MuerteHIV$Personas[j] <-
        modelo$ciclos[[i]]$MuerteHIV$Personas[j] +
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
    } else {
      # No es un ciclo de diagnóstico
      modelo$ciclos[[i]]$InfectadoDx[[1]]$Personas[j] <-
        modelo$ciclos[[i]]$InfectadoDx[[1]]$Personas[j] + nuevosInfectadosConPrEP
      
      InfectadosDxParcial = InfectadosDxParcial + nuevosInfectadosConPrEP
      
      # Actualizamos los infectados PreDx
      modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] <-
        modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] -
        (
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])+
            modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset])
        )
      
      # Le sumamos los casos nuevos que vienen de la rama sin PrEP
      modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] <-
        modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] +
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
      modelo$ciclos[[i]]$LyPerdidosHIVMP <- modelo$ciclos[[i]]$LyPerdidosHIVMP +
        (modelo$ciclos[[i]]$MuerteHIV$Personas[j] * (esperanzaVida - (j - anoOffset)))
      modelo$ciclos[[i]]$QalyPerdidoHIVMP <- modelo$ciclos[[i]]$QalyPerdidoHIVMP + (modelo$ciclos[[i]]$MuerteHIV$Personas[j] * utilidadRestante[(((j - anoOffset ) * ciclosPorAno ) + cicloCounter + 1)])
    }

    # Actualizar total de muertes generales
    modelo$ciclos[[i]]$MuerteGeneral$Total <-
      modelo$ciclos[[i]]$MuerteGeneral$Total +#[LEAN] aca es I porque sino, al analizar lagente de un año mas lo pisas.
      (
        modelo$ciclos[[i]]$MuerteGeneral$Personas[j - anoOffset] -
          modelo$ciclos[[i - 1]]$MuerteGeneral$Personas[j - anoOffset]
      )
    
    # Si es ciclo de testeo, agregar costos
    if ((i-1) %% testeoNoPrEP == 0) { 
      modelo$ciclos[[i]]$CostoSano <- modelo$ciclos[[i]]$CostoSano + #[LEAN] estas en un bucle J si pones I-1 lo pisa en la siguiente edad
        (cDiagnostico * (
          modelo$ciclos[[i]]$SanoSinPrEP$Personas[j] + conteoParcialSOFP
        ))
      
      modelo$ciclos[[i]]$CostoHIV <- modelo$ciclos[[i]]$CostoHIV + #[LEAN] igual que arriba.
        ((
          modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] -
            (
              modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteGeneral[j - anoOffset]) +
                modelo$ciclos[[i - 1]]$InfectadoPreDx$Personas[j - anoOffset] * as.numeric(pMuerteHIV[j - anoOffset])
            )
        ) *
          (cDiagnostico + cConsulta))
    } 
  } else {
      
      conteoParcialSOP <-
        modelo$ciclos[[i]]$SanoOnPrEP[[1]]$Personas[edadMinima]
      
      modelo$ciclos[[i]]$InfectadoNoDx[[1]]$Personas[j] = ((nuevosInfectadosSinPrEP) * (1 - pHIVDiagnosticado))
      InfectadosNoDxParcial = InfectadosNoDxParcial + modelo$ciclos[[i]]$InfectadoNoDx[[1]]$Personas[j]
      
      if (((i-1) %% testeoNoPrEP == 0))
      {
        modelo$ciclos[[i]]$InfectadoDx[[1]]$Personas[j] = ((nuevosInfectadosSinPrEP) * (pHIVDiagnosticado))
        InfectadosDxParcial = InfectadosDxParcial + ((nuevosInfectadosSinPrEP) * (pHIVDiagnosticado))
        
        modelo$ciclos[[i]]$CostoSano = modelo$ciclos[[i]]$CostoSano + (cDiagnostico * modelo$ciclos[[i]]$SanoSinPrEP$Personas[j])
        modelo$ciclos[[i]]$CostoHIV = modelo$ciclos[[i]]$CostoHIV + (nuevosInfectadosSinPrEP * (cDiagnostico + cConsulta))
      }
      else
      {
        modelo$ciclos[[i]]$InfectadoPreDx[1]$Personas[j] = modelo$ciclos[[i]]$InfectadoPreDx[1]$Personas[j] + nuevosInfectadosSinPrEP * pHIVDiagnosticado
      }
      
      
    }
    
    
    # Actualizar costos asociados con el tratamiento y seguimiento de VIH
    
    
    modelo$ciclos[[i]]$CostoHIV <- modelo$ciclos[[i]]$CostoHIV + (InfectadosDxParcial * pHIVTratado * (cTratamientoHIV + cSeguimientoHIV)) + ((modelo$ciclos[[i]]$InfectadoDx[[ciclosPorAno * anosHastaComplicacion]]$Personas[j] * (1 - (pHIVTratado * pHIVControlado))) * cComplicacionesHIV)#[LEAN2]
    modelo$ciclos[[i]]$CostoHIV <- modelo$ciclos[[i]]$CostoHIV + (NoDxPasanADx * (cDiagnostico + cConsulta))#[LEAN2]
    
    # Actualizar costos de PrEP
    modelo$ciclos[[i]]$CostoPrEP <- modelo$ciclos[[i]]$CostoPrEP + (conteoParcialSOP * (cPrEPSeguimiento + cPrEPTratamiento + cPrEPTest))
    
    # Agregar costo de consulta para nuevos infectados con PrEP
    modelo$ciclos[[i]]$CostoHIV <- modelo$ciclos[[i]]$CostoHIV + nuevosInfectadosConPrEP * cConsulta
    
    # Actualizar la disutilidad[LEAN] Asignamos el del ciclo.
    #
    modelo$ciclos[[i]]$Disutilidad <-
      modelo$ciclos[[i]]$Disutilidad +
      ((
        modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] + InfectadosNoDxParcial +
          (InfectadosDxParcial * (1 - pHIVTratado))
      ) * cuHIV
      ) + InfectadosDxParcial * pHIVTratado * cuHIVTTO
    
    # Actualizar totales y acumulados
    modelo$ciclos[[i]]$MuerteHIV$Total <-modelo$ciclos[[i]]$MuerteHIV$Total + modelo$ciclos[[i]]$MuerteHIV$Personas[j]
    #[Lean] estas asignaciones son intraciclo para juntar toda la info de las edades.
    modelo$ciclos[[i]]$InfectadoDxTotal <-modelo$ciclos[[i]]$InfectadoDxTotal + InfectadosDxParcial#[LEAN2]
    modelo$ciclos[[i]]$InfectadoPreDx$Total <-modelo$ciclos[[i]]$InfectadoPreDx$Total + modelo$ciclos[[i]]$InfectadoPreDx$Personas[j]
    modelo$ciclos[[i]]$InfectadoNoDxTotal <-modelo$ciclos[[i]]$InfectadoNoDxTotal + InfectadosNoDxParcial#[LEAN2]
    modelo$ciclos[[i]]$SanoSinPrEP$Total <-modelo$ciclos[[i]]$SanoSinPrEP$Total + modelo$ciclos[[i]]$SanoSinPrEP$Personas[j]
    modelo$ciclos[[i]]$SanosOffPrEPTotal <-modelo$ciclos[[i]]$SanosOffPrEPTotal + conteoParcialSOFP
    modelo$ciclos[[i]]$SanosOnPrEPTotal <-modelo$ciclos[[i]]$SanosOnPrEPTotal + conteoParcialSOP
    
    modelo$ciclos[[i]]$SanosTotal <-modelo$ciclos[[i]]$SanosOnPrEPTotal + modelo$ciclos[[i]]$SanosOffPrEPTotal + modelo$ciclos[[i]]$SanoSinPrEP$Total
    modelo$ciclos[[i]]$InfectadosTotal <-modelo$ciclos[[i]]$InfectadoPreDx$Total + modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoDxTotal
    modelo$ciclos[[i]]$nuevosCasos <-modelo$ciclos[[i]]$nuevosCasos + nuevosInfectadosSinPrEP + nuevosInfectadosConPrEP
    modelo$ciclos[[i]]$nuevosCasosDx <-modelo$ciclos[[i]]$nuevosCasosDx +  (nuevosInfectadosSinPrEP * pHIVDiagnosticado)  + nuevosInfectadosConPrEP
    
    modelo$ciclos[[i]]$CasosAcumulados <-modelo$ciclos[[i]]$CasosAcumulados + nuevosInfectadosSinPrEP + nuevosInfectadosConPrEP
    # Calcular QALY para personas con VIH
    modelo$ciclos[[i]]$qalyHIV <- modelo$ciclos[[i]]$qalyHIV + 
      (modelo$ciclos[[i]]$InfectadoPreDx$Personas[j] + InfectadosNoDxParcial + (InfectadosDxParcial * (1 - pHIVTratado))) * (cuPoblacion[j + 1] - cuHIV) +
      (InfectadosDxParcial * pHIVTratado * (cuPoblacion[j + 1] - cuHIVTTO))#[LEAN2]
   
     # Calcular QALY para personas sin VIH
    modelo$ciclos[[i]]$qalyNOHIV <- modelo$ciclos[[i]]$qalyNOHIV + (
      (
        conteoParcialSOFP + conteoParcialSOP + modelo$ciclos[[i]]$SanoSinPrEP$Personas[j]
      ) * cuPoblacion[j + 1]
    )
    

    
    # Establecer la población en riesgo
    if (j <= limiteEdadRiesgo) {
      
      personasRiesgo <- modelo$ciclos[[i]]$SanosTotal
    }
    
    # Si está por debajo del límite de edad contagiosa, agregar a aquellos que pueden contagiar
    if (j <= limiteEdadContagiosos) {
      DiagnosticadosContagiosos <-DiagnosticadosContagiosos + InfectadosDxParcial#[LEAN2]
      NoDiagnosticadosContagiosos <-NoDiagnosticadosContagiosos + InfectadosNoDxParcial  + modelo$ciclos[[i]]$InfectadoPreDx$Personas[j]#[LEAN2]
    }
    
    
  }##aca cierro loop j
  
  # Actualizar el total de casos de VIH diagnosticados y casos de VIH 
  modelo$casosHIVDx <-modelo$casosHIVDx + modelo$ciclos[[i]]$nuevosCasosDx
  modelo$casosHIV <-modelo$casosHIV + modelo$ciclos[[i]]$nuevosCasos
  
# Calcular los años vividos y descontados
  
  modelo$anosVividos <-
    modelo$anosVividos + (
      modelo$ciclos[[i]]$SanoSinPrEP$Total +
        modelo$ciclos[[i]]$SanosOffPrEPTotal + modelo$ciclos[[i]]$SanosOnPrEPTotal +
        modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoPreDx$Total +
        modelo$ciclos[[i]]$InfectadoDxTotal
    ) / ciclosPorAno#[LEAN2]
  modelo$anosVividosD <-
    modelo$anosVividosD + (((
      modelo$ciclos[[i]]$SanoSinPrEP$Total +
        modelo$ciclos[[i]]$SanosOffPrEPTotal + modelo$ciclos[[i]]$SanosOnPrEPTotal +
        modelo$ciclos[[i]]$InfectadoNoDxTotal + modelo$ciclos[[i]]$InfectadoPreDx$Total +
        modelo$ciclos[[i]]$InfectadoDxTotal
    ) / ciclosPorAno
    ) / (1 + tasaDescuento) ^ (i-1))#[LEAN2]

  # Calcular QALYs vividos y descontados
  modelo$qalysVividos <-
    modelo$qalysVividos + modelo$ciclos[[i]]$qalyHIV + modelo$ciclos[[i]]$qalyNOHIV
  modelo$qalysVividosD <-
    modelo$qalysVividosD + ((modelo$ciclos[[i]]$qalyHIV + modelo$ciclos[[i]]$qalyNOHIV) / ((1 + tasaDescuento) ^ (i-1)))
  
  # Actualizar los años de vida perdidos debido al VIH y sus años descontados
  modelo$anosPerdidosMPHIV <-
    modelo$anosPerdidosMPHIV + modelo$ciclos[[i]]$LyPerdidosHIVMP
  modelo$anosPerdidosMPHIVD <-
    modelo$anosPerdidosMPHIVD + (modelo$ciclos[[i]]$LyPerdidosHIVMP / (1 + tasaDescuento) ^ (i-1))# i - 1
  
  # Actualizar QALYs perdidos debido al VIH y sus QALYs descontados
  modelo$qalysPerdidosMPHIV <-
    modelo$qalysPerdidosMPHIV + modelo$ciclos[[i]]$QalyPerdidoHIVMP
  modelo$qalysPerdidosMPHIVD <-
    modelo$qalysPerdidosMPHIVD + (modelo$ciclos[[i]]$QalyPerdidoHIVMP / (1 + tasaDescuento) ^ (i-1))
  
  # Actualizar QALYs perdidos debido a la discapacidad relacionada con el VIH y sus QALYs descontados
  modelo$qalysPerdidosDiscHIV <-
    modelo$qalysPerdidosDiscHIV + modelo$ciclos[[i]]$Disutilidad
  modelo$qalysPerdidosDiscHIVD <-
    modelo$qalysPerdidosDiscHIVD + (modelo$ciclos[[i]]$Disutilidad / (1 + tasaDescuento) ^ (i-1))
  
  # Actualizar QALYs perdidos totales y descontados
  modelo$qalysPerdidos <-
    modelo$qalysPerdidos + modelo$ciclos[[i]]$QalyPerdidoHIVMP + modelo$ciclos[[i]]$Disutilidad
  modelo$qalysPerdidosD <-
    modelo$qalysPerdidosD + (modelo$ciclos[[i]]$QalyPerdidoHIVMP + modelo$ciclos[[i]]$Disutilidad) / (1 + tasaDescuento) ^ (i-1)#[LEAN2]
  
  # Actualizar costos relacionados con el VIH, sanos y PrEP, y sus costos descontados
  modelo$CostoHIV <- modelo$CostoHIV + modelo$ciclos[[i]]$CostoHIV
  modelo$CostoHIVD <-
    modelo$CostoHIVD + (modelo$ciclos[[i]]$CostoHIV / (1 + tasaDescuento) ^ (i-1))
  modelo$CostoSano <-
    modelo$CostoSano + modelo$ciclos[[i]]$CostoSano
  modelo$CostoSanoD <-
    modelo$CostoSanoD + (modelo$ciclos[[i]]$CostoSano / (1 + tasaDescuento) ^ (i-1))
  
  #[LEAN 19/1]
  if (modelo$ciclos[[i]]$CostoPrEP > 0)
  {
  modelo$CostoPrEP <-
    modelo$CostoPrEP + modelo$ciclos[[i]]$CostoPrEP + (costoProgramatico / ciclosPorAno)
  modelo$CostoPrEPD <-
    modelo$CostoPrEPD + ((modelo$ciclos[[i]]$CostoPrEP + (costoProgramatico / ciclosPorAno)) / (1 + tasaDescuento) ^ (i-1))
  }
  #[LEAN 19/1]
  # Actualizar tiempo sin diagnóstico
  modelo$TiempoSinDx <-
    modelo$TiempoSinDx + (modelo$ciclos[[i]]$InfectadoPreDx$Total * (12 / ciclosPorAno))
  
  if (escribir) {
    
    newres <- escribirOutput((i-1), 
                             ciclosPorAno,
                             modelo$ciclos[[i]]$SanoSinPrEP$Total,
                             modelo$ciclos[[i]]$SanosOffPrEPTotal,
                             modelo$ciclos[[i]]$SanosOnPrEPTotal,
                             modelo$ciclos[[i]]$InfectadoNoDxTotal,
                             modelo$ciclos[[i]]$InfectadoPreDx$Total,
                             modelo$ciclos[[i]]$InfectadoDxTotal,
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
                             modelo$ciclos[[i]]$CostoPrEP,
                             modelo$ciclos[[i]]$CostoHIV)


    result <- rbind(result, newres)
  } #if escribir
  
}#termina loop i



###final fuera de loop i

# Actualizar años vividos por la cohorte
#modelo$anosVividos <- modelo$anosVividos + (cohorteSize / ciclosPorAno)



# Actualizar el total de muertes por VIH
modelo$muertesHIV <- modelo$ciclos[[numeroCiclos + 1]]$MuerteHIV$Total

# Actualizar el total de muertes (incluyendo VIH, muertes generales y otras finalizaciones)
modelo$muertesTotales <- modelo$ciclos[[numeroCiclos + 1]]$MuerteHIV$Total + modelo$ciclos[[numeroCiclos + 1]]$MuerteGeneral$Total + finalizados


casosTotales <- modelo$casosHIV + modelo$ciclos[[1]]$InfectadosTotal

resultados <<- escribirResultadosComparacion(
  linea, 
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
  casosTotales, 
  modelo$anosVividosD, 
  modelo$qalysVividosD, 
  modelo$anosPerdidosMPHIVD, 
  modelo$qalysPerdidosMPHIVD, 
  modelo$qalysPerdidosDiscHIVD, 
  modelo$qalysPerdidosD, 
  modelo$CostoSanoD, 
  modelo$CostoPrEPD, 
  modelo$CostoHIVD, 
  cohorteSizeFinal
)

parametro <<-parametro
#View(result)
#return(resultados)
print("fin")

}

#linea <- "Baseline" # o "nuevo"
#parametro1 <- crearParametros(linea)

###Leo los datos
#debug(funcionPrincipal)
#funcionPrincipal("Baseline", "ARGENTINA", parametro)

#### aplico la fn para los dos escenarios-----

# linea <- "Baseline" # o "nuevo"
# parametro1 <- crearParametros(linea)
# 
# linea <- "Nuevo" # o "nuevo"
# parametro2 <- crearParametros(linea)

# parametros_prep <- list(cohorteSize = 100000,
#                    descuento = 0.03,
#                    edadMinima = 18, ## parametro basico
#                    edadFinal = 100,
#                    #tipoDuracion = 1,##hardcodeada
#                    duracionPrEP = 99,## parametro basico
#                    edadMaximaInicial = 50,##parametro basico
#                    PrEPuptake = 0,##parametro basico
#                    edadFinPrEP = 50,
#                    limiteEdadRiesgo = 60,
#                    eficaciaPrEP = 0,
#                    adherenciaPrEP = 0,
#                    limiteEdadContagiosos = 65,
#                    #tipoCohorte = 1,#hardcordeada
#                    cohorteSize_nuevo = 100000 ,
#                    tasaDescuento_nuevo = 0.03, 
#                    edadMinima_nuevo = 18 ,## parametro basico
#                    edadFinal_nuevo = 100 ,
#                    #tipoDuracion_nuevo = 0 ,#hardcodeada
#                    duracionPrEP_nuevo = 99 ,## parametro basico
#                    edadMaximaInicial_nuevo = 50 ,## parametro basico
#                    PrEPuptake_nuevo = 0.5, 
#                    edadFinPrEP_nuevo = 50, ## parametro basico
#                    limiteEdadRiesgo_nuevo = 60 ,
#                    eficaciaPrEP_nuevo = 0.86, 
#                    adherenciaPrEP_nuevo = 0.8 ,
#                    limiteEdadContagiosos_nuevo = 65 
#                    #tipoCohorte_nuevo = 1 #harcoddeada
#                    )

get_prep_params = function (paisCol) {
  #if (paisCol == "URUGUAY") {browser()}
  lista = list(
    
    ###basicos
    duracionPrEP = 0,## parametro basico
    cPrEPTratamiento_anual = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="PREP_TTO") %>%
      dplyr::select(VALOR) %>% 
      as.numeric(),## parametro basico
    
    duracionPrEP_nuevo = 5,## parametro basico
    cPrEPTratamiento_anual_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="PREP_TTO") %>%
      dplyr::select(VALOR) %>% 
      as.numeric(),## parametro basico
    
    ####
    edadMinima = 18, 
    edadMaximaInicial = 50,
    PrEPuptake = 0,
    adherenciaPrEP = 0,
    eficaciaPrEP = 0,
    edadFinPrEP = 50,
    prevalenciaHIV = input_prep %>%
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de HIV en la poblacion") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    ratio = input_prep %>%
      dplyr::filter(PAIS==paisCol & PARAMETRO=="Razon Incidencia/Prevalencia") %>%
      dplyr::select(VALOR) %>% as.numeric(),
   
     pHIVDiagnosticado = input_prep %>% 
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de hiv diagnosticados") %>%
      dplyr::select(VALOR) %>% 
      as.numeric(),
    pHIVTratado =input_prep %>% 
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de hiv tratados") %>%
      dplyr::select(VALOR) %>% 
      as.numeric(),
    
    pHIVControlado = input_prep %>%
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de hiv controlados") %>% 
      dplyr::select(VALOR) %>% 
      as.numeric(),
    
    #CoSTOS
    cPrEPSeguimiento_anual = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="PREP_SEGUIMIENTO") %>%
      dplyr::select(VALOR) %>%
      as.numeric(),
    
    cPrEPTest_anual = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="PREP_TEST") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cTratamientoHIV_anual =input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="TRATAMIENTO") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cSeguimientoHIV_anual = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="SEGUIMIENTO") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cConsulta = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="CONSULTA") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cComplicacionesHIV = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="COMPLICACIONES") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
   
    descuento = 0.05,
    
    costoProgramatico = 0,#[LEAN 19/1],
    
    
    #NUEVOS
    
    edadMinima_nuevo = 18, 
    edadMaximaInicial_nuevo = 50,
    PrEPuptake_nuevo = 0.5,
    adherenciaPrEP_nuevo = 0.6,
    eficaciaPrEP_nuevo = 0.86,
    edadFinPrEP_nuevo = 50,
    
    prevalenciaHIV_nuevo = input_prep %>%
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de HIV en la poblacion") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    ratio_nuevo = input_prep %>%
      dplyr::filter(PAIS==paisCol & PARAMETRO=="Razon Incidencia/Prevalencia") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    pHIVDiagnosticado_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de hiv diagnosticados") %>%
      dplyr::select(VALOR) %>% 
      as.numeric(),
    
    pHIVTratado_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de hiv tratados") %>%
      dplyr::select(VALOR) %>% 
      as.numeric(),
    
    pHIVControlado_nuevo = input_prep %>%
      dplyr::filter(PAIS==paisCol & PARAMETRO=="% de hiv controlados") %>% 
      dplyr::select(VALOR) %>% 
      as.numeric(),
    
    #CoSTOS
    
    cPrEPSeguimiento_anual_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="PREP_SEGUIMIENTO") %>%
      dplyr::select(VALOR) %>%
      as.numeric(),
    
    cPrEPTest_anual_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="PREP_TEST") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cTratamientoHIV_anual_nuevo =input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="TRATAMIENTO") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cSeguimientoHIV_anual_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="SEGUIMIENTO") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cConsulta_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="CONSULTA") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    cComplicacionesHIV_nuevo = input_prep %>% 
      dplyr::filter(PAIS==paisCol & tipo=="COSTOS" & PARAMETRO=="COMPLICACIONES") %>%
      dplyr::select(VALOR) %>% as.numeric(),
    
    
    descuento_nuevo = 0.05,
    
    costoProgramatico_nuevo = 0
    )
  return(lista)
}

get_prep_params_labels = function () {
  c(
    #basicos
    "Duración de la intervención de uso de PrEP (años) (Baseline)",
    "Costo anual del uso de PrEP (USD) (Baseline)",
    "Duración de la intervención de uso de PrEP (años)",
    "Costo anual del uso de PrEP (USD)",
    #avanzados
    "Edad mínima inicial(Baseline)",
    "Edad máxima inicial (Baseline)", 
    "Aceptabilidad del tratamiento con PrEP en la población (%) (Baseline)",
    "Adherencia al uso de PrEP en la población (%) (Baseline)",
    "Eficacia del uso de PrEP en la población (%) (Baseline)",
    "Edad de fin de indicación de PrEP (Baseline)",
    "Prevalencia de VIH en la población en riesgo (%) (Baseline)",
    "Razón incidencia/prevalencia (Baseline)",
    "Personas que viven con VIH y que conocen su diagnóstico (%) (Baseline)",
    "Personas que viven con VIH que reciben Terapia Antirretroviral (TARV) (%) (Baseline)",
    "Personas que viven con VIH que tiene carga viral suprimida (%) (Baseline)",
    "Costo de seguimiento anual de PrEP (USD) (Baseline)",
    "Costo anual de testeo PrEP (USD) (Baseline)",
    "Costo anual de tratamiento de VIH (USD) (Baseline)",
    "Costo anual de seguimiento de VIH (USD) (Baseline)",
    "Costo de consulta con infectología (USD) (Baseline)",
    "Costo anual de complicaciones asociados al VIH (USD) (Baseline)",
    "Tasa de descuento (%) (Baseline)",
    "Costo programático anual de PrEP (USD) (Baseline)",
    #nuevos
   
    "Edad mínima inicial",
    "Edad máxima inicial", 
    "Aceptabilidad del tratamiento con PrEP en la población (%)",
    "Adherencia al uso de PrEP en la población (%)", 
    "Eficacia del uso de PrEP en la población (%)", 
    "Edad de fin de indicación de PrEP", 
    "Prevalencia de VIH en la población en riesgo (%)",
    "Razón incidencia/prevalencia",
    "Personas que viven con VIH y que conocen su diagnóstico (%)",
    "Personas que viven con VIH que reciben Terapia Antirretroviral (TARV) (%)",
    "Personas que viven con VIH que tiene carga viral suprimida (%)",
    "Costo de seguimiento anual de PrEP (USD)",
    "Costo anual de testeo PrEP (USD)",
    "Costo anual de tratamiento de VIH (USD)",
    "Costo anual de seguimiento de VIH (USD)",
    "Costo de consulta con infectología (USD)",
    "Costo anual de complicaciones asociados al VIH (USD)",
    "Tasa de descuento (%)",
    "Costo programático anual de PrEP (USD)"
    
  )
}


get_prep_hover = function () {
  
  c( #basicos
    "Período de tiempo en años durante el cual se implementará la intervención de uso de PrEP a nivel de la población.",
     "Costo de recibir PrEP diario por un año. Incluye Emtricitabine/Tenofovir.",
     "Período de tiempo en años durante el cual se implementará la intervención de uso de PrEP a nivel de la población.",
     "Costo de recibir PrEP diario por un año. Incluye Emtricitabine/Tenofovir.",
     #avanzados
     "Edad mínima de los participantes en la cohorte inicial del estudio de distribución etaria.",
     "Edad máxima de los integrantes de la cohorte inicial en el estudio de distribución por edades.",
     "Porcentaje de personas sanas en la cohorte que recibirán PrEP. Puede utilizarse para reflejar la disposición de la población hacia el uso de PrEP o la cobertura de la intervención objetivo.",
     "Porcentaje de personas con indicación a PrEP para la prevención de infección por VIH que siguen el régimen de tratamiento, incluyendo la consistencia y regularidad en la toma del medicamento. La adherencia impacta directamente en la efectividad final del uso de PrEP.",
     "Porcentaje de reducción del riesgo de contagio de HIV en la población a analizar que recibe PrEP, asumiendo una adherencia mayor a 80%.",
     "Edad máxima en años que el modelo asume que una persona buscará recibir PrEP o tendrá indicación del mismo.",
     "Porcentaje de individuos en la población en estudio que viven con VIH",
     "Cociente entre los nuevos casos de VIH diagnosticados en la población en riesgo y los casos prevalentes.",
     "Porcentaje de personas que conocen su diagnóstico entre la población de personas que viven con VIH.",
     "Porcentaje de personas que viven con VIH y reciben TARV entre la población que conoce su diagnóstico de VIH.",
     "Porcentaje de individuos que viven con VIH con carga viral negativa en la población entre la población que conoce su diagnóstico y recibe tratamiento. Es el porcentaje de individuos que han reducido a niveles muy bajos o indetectables la presencia de VIH en sangre gracias al tratamiento, indicando un control efectivo del virus.",
     "Costo total de seguimiento médico anual de una persona bajo la estrategia PrEP.",
     "Costo total de realizar los exámenes complementarios pertinentes a la estrategia PrEP durante un año. Incluye tests VIH y creatinina trimestral.",
     "Costo anual de recibir tratamiento antirretroviral para VIH. Incluye Dolutegravir, Lamivudine, Tenofovir, Efavirenz, Emtricitabine, Darunavir, Ritonavir, Emtricitabina y Abacavir.",
     "Costo anual de seguimiento de una persona que vive con VIH. Incluye consultas médicas, pruebas de laboratorio, estudios de imagen y consejería y educación sexual.",
     "Costo de una consulta con infectología.",
     "Costo ponderado anual de complicaciones en pacientes infectados con VIH. Se estimó utilizando la probabilidad de internación por complicaciones asociadas a la enfermedad de estos pacientes, la mediana de días de internación y el costo promedio de estadía hospitalaria.",
     "Se utiliza para traer al presente los costos y beneficios en salud futuros.",
     "Costo de implementar y mantener PrEP en un año.",
     #nuevos
     
     "Edad mínima de los participantes en la cohorte inicial del estudio de distribución etaria.",
     "Edad máxima de los integrantes de la cohorte inicial en el estudio de distribución por edades.",
     "Porcentaje de personas sanas en la cohorte que recibirán PrEP. Puede utilizarse para reflejar la disposición de la población hacia el uso de PrEP o la cobertura de la intervención objetivo.",
     "Porcentaje de personas con indicación a PrEP para la prevención de infección por VIH que siguen el régimen de tratamiento, incluyendo la consistencia y regularidad en la toma del medicamento. La adherencia impacta directamente en la efectividad final del uso de PrEP.",
     "Porcentaje de reducción del riesgo de contagio de HIV en la población a analizar que recibe PrEP, asumiendo una adherencia mayor a 80%.",
     "Edad máxima en años que el modelo asume que una persona buscará recibir PrEP o tendrá indicación del mismo.",
     "Porcentaje de individuos en la población en estudio que viven con VIH.",
     "Cociente entre los nuevos casos de VIH diagnosticados en la población en riesgo y los casos prevalentes.",
     "Porcentaje de personas que conocen su diagnóstico entre la población de personas que viven con VIH.",
     "Porcentaje de personas que viven con VIH y reciben TARV entre la población que conoce su diagnóstico de VIH.",
     "Porcentaje de individuos que viven con VIH con carga viral negativa en la población entre la población que conoce su diagnóstico y recibe tratamiento. Es el porcentaje de individuos que han reducido a niveles muy bajos o indetectables la presencia de VIH en sangre gracias al tratamiento, indicando un control efectivo del virus.",
     "Costo total de seguimiento médico anual de una persona bajo la estrategia PrEP.",
     "Costo total de realizar los exámenes complementarios pertinentes a la estrategia PrEP durante un año. Incluye tests VIH y creatinina trimestral.",
     "Costo anual de recibir tratamiento antirretroviral para VIH. Incluye Dolutegravir, Lamivudine, Tenofovir, Efavirenz, Emtricitabine, Darunavir, Ritonavir, Emtricitabina y Abacavir.",
     "Costo anual de seguimiento de una persona que vive con VIH. Incluye consultas médicas, pruebas de laboratorio, estudios de imagen y consejería y educación sexual.",
     "Costo de una consulta con infectología.",
     "Costo ponderado anual de complicaciones en pacientes infectados con VIH. Se estimó utilizando la probabilidad de internación por complicaciones asociadas a la enfermedad de estos pacientes, la mediana de días de internación y el costo promedio de estadía hospitalaria.",
     "Se utiliza para traer al presente los costos y beneficios en salud futuros.",
     "Costo de implementar y mantener PrEP en un año."
     )
}

prep_outcomes_labels = function() {
  c("Casos de VIH evitados (n)",
    "Muertes evitadas (n)",
    "Años de vida salvados",
    "Años de vida salvados (descontado)",
    "Años de vida ajustados por discapacidad evitados",
    "Años de vida ajustados por discapacidad evitados (descontado)",
    "Costo total de la intervención (USD)",
    "Costo total de la intervención (USD) (descontado)",
    "Costos evitados atribuibles a la intervención (USD)",
    "Costos evitados atribuibles a la intervención (USD) (descontado)",
    "Diferencia de costos respecto al escenario basal (USD)",
    "Diferencia de costos respecto al escenario basal (USD) (descontado)",
    "Razón de costo-efectividad incremental (RCEI) por vida salvada (VS) (USD)",
    "Razón de costo-efectividad incremental (RCEI) por vida salvada (VS) (USD) (descontado)",
    "Razón de costo-efectividad incremental (RCEI) por Año de vida salvado (AVS)",
    "Razón de costo-efectividad incremental (RCEI) por Año de vida salvado (AVS) (descontado)",
    "Razón de costo-efectividad incremental (RCEI) por Años de Vida Ajustados por Discapacidad (AVAD) Evitados",
    "Razón de costo-efectividad incremental (RCEI) por Años de Vida Ajustados por Discapacidad (AVAD) Evitados (descontado)",
    "Retorno de Inversión (ROI) (%)",
    "Retorno de Inversión (ROI) (%) (descontado)"
  )
}

# lista_resultados <- funcionCalculos(parametros_prep, "ARGENTINA")

#linea <- "Baseline"

#funcionCalculos(get_prep_params(), "ARGENTINA")
