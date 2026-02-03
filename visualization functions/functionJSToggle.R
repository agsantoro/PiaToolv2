functionJSToggle = function(i) {
  tags$script(HTML(
    glue("$(document).ready(function() {{
      $('#toggle_floating_buttons_<<i>>').on('click', function(e) {{
        e.preventDefault();
        $('#floatingButtonsContainer_<<i>>').toggleClass('collapsed');
      }});
    }});", 
         .open = "<<", 
         .close = ">>")
  ))
}

