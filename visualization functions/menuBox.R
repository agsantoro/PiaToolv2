menuBox = function (title, text, iconType, iconColor, linkTo) {
  
  htmlId = linkTo
  
  tags$a(
    id = glue("menuBox_{htmlId}"),
    div(
      class = "grid-item",
      style = "background: rgba(170, 202, 228, 0.3);
                   padding: 30px; 
                   border-radius: 10px;
                   backdrop-filter: blur(10px); 
                   transition: transform 0.3s ease, box-shadow 0.3s ease;
                   cursor: pointer;
                   display: flex;
                   flex-direction: column;
                   height: 100%;",
      
      icon(iconType, style = glue("font-size: 3em; margin-bottom: 15px; opacity: 0.8; color: {iconColor};")),
      
      div(
        h4(title)
      ),
      
      div(
        style = "text-align: center; font-size: 0.9em; line-height: 1.6; padding: 0; flex-grow: 1; display: flex; align-items: top;",
        p(text)
      )
      
    ),
    href = route_link(linkTo)
  )
  
}

