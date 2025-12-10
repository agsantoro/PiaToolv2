library(dplyr)
library(tidyr)
library(stringr)

source("models/hpp/funciones/funciones.R")

#Outcomes

input = list(
  pais = input$pais,
  usoOxitocina_base = 0.71818,
  usoOxitocina_target = 0.80339,
  descuento = 0.05
)

reactive = list(
  "Diferencia de costos" = hpp(input$pais,input$usoOxitocina_target*.99,descuento)[[12]]-hpp(input$pais,input$usoOxitocina_base,descuento)[[12]],
  "Hemorragias Post Parto Evitadas" = hpp(input$pais,input$usoOxitocina_base,descuento)[[3]]-hpp(input$pais,input$usoOxitocina_target,descuento)[[3]],
  "Muertes por Hemorragias Post Parto Evitadas" = (hpp(input$pais,input$usoOxitocina_base,descuento)[[3]]-hpp(input$pais,input$usoOxitocina_target,descuento)[[3]]) * hpp(input$pais,input$usoOxitocina_target,descuento)[[5]],
  "Histerectomias por Hemorragias Post Parto Evitadas:" = (hpp(input$pais,input$usoOxitocina_target,descuento)[[3]]-hpp(input$pais,input$usoOxitocina_base,descuento)[[3]]) * hpp(input$pais,input$usoOxitocina_target,descuento)[[9]] * hpp(input$pais,input$usoOxitocina_target,descuento)[[10]],
  "Años de vida por muerte prematura salvados" = hpp(input$pais,input$usoOxitocina_base,descuento)[[11]] * ((hpp(input$pais,input$usoOxitocina_target,descuento)[[3]] - hpp(input$pais,input$usoOxitocina_base,descuento)[[3]]) * hpp(input$pais,input$usoOxitocina_base,descuento)[[5]]),
  "Años de vida por discapacidad salvados" = hpp(input$pais,input$usoOxitocina_base,descuento)[[1]] * (hpp(input$pais,input$usoOxitocina_target,descuento)[[3]] - hpp(input$pais,input$usoOxitocina_base,descuento)[[3]]) * hpp(input$pais,input$usoOxitocina_base,descuento)[[9]] * hpp(input$pais,input$usoOxitocina_base,descuento)[[10]]
)


