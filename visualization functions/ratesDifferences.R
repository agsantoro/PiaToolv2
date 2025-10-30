ratesDifferences <-function(datosFiltrados) {
  calculodiferencia <- datosFiltrados$tmape [datosFiltrados$sexo == "1"] - datosFiltrados$tmape [datosFiltrados$sexo == "2"]
  
  return(calculodiferencia)
}