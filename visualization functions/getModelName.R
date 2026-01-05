getModelName = function(model) {
  switch(model,
         "hearts" = "Iniciativa HEARTS",
         "hpv"    = "Vacunación contra el VPH",
         "tbc"    = "VDOT para Tuberculosis",
         "hepC"   = "Tratamiento para la hepatitis C crónica",
         "hpp"    = "Uso de oxitocina para la prevención de la hemorragia post parto",
         "prep"   = "Profilaxis pre exposición (PrEP) para VIH",
         "sifilis"= "Tests rápidos en punto de cuidado para sífilis gestacional",
         "naat"   = "Pruebas de amplificación de ácidos nucleicos (NAAT) para TBC",
         x)
}

getModelName("hpp")
