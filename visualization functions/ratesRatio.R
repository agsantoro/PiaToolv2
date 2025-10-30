#Cociente de tasas
ratesRatio <- function(datosFiltrados) {
  calculocociente <- datosFiltrados$tmape [datosFiltrados$sexo == "2"] / datosFiltrados$tmape [datosFiltrados$sexo == "1"]
  
  return(calculocociente)
}
