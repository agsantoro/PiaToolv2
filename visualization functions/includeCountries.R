includeCountries = function(
    xlsxPath) {
  countriesIncluded = readxl::read_xlsx(xlsxPath, sheet = "countries")
  
  setNames(
    lapply(seq_along(countriesIncluded$Name), function(i) {
      ctr = toupper(countriesIncluded$Name[i])
      alpha = countriesIncluded$Alpha2[i]
      ctr = tags$div(
        style = "display: flex; align-items: center;",
        tags$img(
          src = glue("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/{alpha}.svg"),
          alt = "Bandera {ctr}", width = 20, height = 15, style = "margin-right: 8px;"
        ),
        tags$span(countriesIncluded$Name[i])
      )
    }), toupper(countriesIncluded$Name)
  )
}