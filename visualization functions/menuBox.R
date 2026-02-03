menuBox = function (title, text, iconType, iconColor, linkTo) {
  
  htmlId = linkTo
  uniqueId = paste0("text_", htmlId, "_", sample(1000:9999, 1))
  
  textLength = nchar(text)
  shouldCollapse = textLength > 100
  
  if (shouldCollapse) {
    shortText = substr(text, 1, 100)
  } else {
    shortText = text
  }
  
  tags$a(
    id = glue("menuBox_{htmlId}"),
    href = route_link(linkTo),
    style = "text-decoration: none; color: inherit;", # Evita que el <a> cambie el color del texto
    div(
      class = "grid-item",
      style = "background: white;
               padding: 30px; 
               border-radius: 10px;
               backdrop-filter: blur(10px); 
               transition: transform 0.3s ease, box-shadow 0.3s ease;
               cursor: pointer;
               display: flex;
               flex-direction: column;
               height: 100%;",
               
      
      icon(iconType, style = glue("font-size: 3em; margin-bottom: 15px; opacity: 0.8; color: {iconColor};")),
      
      div(h4(title, style = "color: #1c98d6;")), # Asegurar título negro
      
      div(
        style = "text-align: center; font-size: 0.9em; line-height: 1.6; padding: 0; flex-grow: 1;",
        
        if (shouldCollapse) {
          p(
            style = "margin: 0; color: black !important;",
            # 1. TEXTO CORTO
            span(id = paste0(uniqueId, "_short"), style = "color: black !important", shortText),
            # 2. PUNTOS
            span(id = paste0(uniqueId, "_dots"), style = "color: black !important", "... "),
            # 3. TEXTO COMPLETO (Ahora va antes que el botón)
            span(id = paste0(uniqueId, "_full"), style = "display: none; color: black !important;", text),
            # 4. BOTÓN (Siempre al final)
            tags$span(
              id = paste0(uniqueId, "_toggle"),
              style = "color: #4a90e2 !important; cursor: pointer; text-decoration: underline; font-weight: 500; margin-left: 5px;",
              onclick = glue("
                event.preventDefault();
                event.stopPropagation();
                var short = document.getElementById('{uniqueId}_short');
                var full = document.getElementById('{uniqueId}_full');
                var toggle = document.getElementById('{uniqueId}_toggle');
                var dots = document.getElementById('{uniqueId}_dots');
                if (full.style.display === 'none') {{
                  short.style.display = 'none';
                  dots.style.display = 'none';
                  full.style.display = 'inline';
                  toggle.textContent = ' Leer menos';
                }} else {{
                  short.style.display = 'inline';
                  dots.style.display = 'inline';
                  full.style.display = 'none';
                  toggle.textContent = 'Leer más';
                }}
              "),
              "Leer más"
            )
          )
        } else {
          p(style = "margin: 0; color: black !important;", text)
        }
      )
    )
  )
}