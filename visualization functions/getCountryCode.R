getCountryCode <- function(country) {
  switch(country,
         "ARGENTINA" = "ar",
         "URUGUAY" = "uy",
         "COSTA RICA" = "cr",
         "REPÚBLICA DOMINICANA" = "do",
         "PERU" = "pe",
         "BRAZIL" = "br",
         "JAMAICA" = "jm",
         "COLOMBIA" = "co",
         "CHILE" = "cl",
         "MEXICO" = "mx"
  )
}


