# Función para crear el input con drag and drop
dragDropInput <- function(inputId, label, n_cuadros, choices, width = "100%", 
                          height_cuadros = "200px", height_final = "150px") {
  
  # Validar que choices sea una lista con nombres
  if (!is.list(choices) || is.null(names(choices))) {
    stop("choices debe ser una lista nombrada")
  }
  
  # Validar que el número de cuadros coincida con la lista
  if (length(choices) != n_cuadros) {
    stop("El número de cuadros debe coincidir con la longitud de choices")
  }
  
  # Generar IDs únicos para cada cuadro
  cuadro_ids <- paste0(inputId, "_cuadro_", seq_len(n_cuadros))
  final_id <- paste0(inputId, "_final")
  
  # Crear HTML para cada cuadro fuente
  cuadros_html <- lapply(seq_len(n_cuadros), function(i) {
    cuadro_name <- names(choices)[i]
    cuadro_choices <- choices[[i]]
    
    tags$div(
      class = "drag-source-container",
      tags$h4(cuadro_name, style = "margin-top: 0;"),
      tags$div(
        id = cuadro_ids[i],
        class = "drag-source-box",
        style = sprintf("height: %s; overflow-y: auto;", height_cuadros),
        lapply(cuadro_choices, function(choice) {
          tags$div(
            class = "drag-item",
            draggable = "true",
            `data-value` = choice,
            choice
          )
        })
      )
    )
  })
  
  # HTML del cuadro final con botón de limpiar
  final_box <- tags$div(
    class = "drag-final-container",
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      tags$h4("Área de destino", style = "margin: 0;"),
      tags$button(
        id = paste0(inputId, "_clear"),
        class = "btn btn-sm btn-warning clear-button",
        type = "button",
        "Limpiar selección"
      )
    ),
    tags$div(
      id = final_id,
      class = "drag-final-box",
      style = sprintf("height: %s; margin-top: 10px;", height_final)
    )
  )
  
  # CSS
  css <- tags$head(tags$style(HTML(sprintf("
    #%s-container {
      width: %s;
      font-family: Arial, sans-serif;
    }
    
    .drag-sources-wrapper {
      display: flex;
      gap: 15px;
      margin-bottom: 20px;
      flex-wrap: wrap;
    }
    
    .drag-source-container {
      flex: 1;
      min-width: 200px;
    }
    
    .drag-source-box {
      border: 2px solid #ddd;
      border-radius: 8px;
      padding: 10px;
      background-color: #f9f9f9;
    }
    
    .drag-final-container {
      margin-top: 10px;
    }
    
    .drag-final-box {
      border: 2px dashed #4CAF50;
      border-radius: 8px;
      padding: 10px;
      background-color: #f0f8f0;
      min-height: 100px;
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
      align-content: flex-start;
    }
    
    .drag-item {
      background-color: #2196F3;
      color: white;
      padding: 8px 12px;
      border-radius: 20px;
      cursor: move;
      display: inline-block;
      margin: 4px;
      user-select: none;
      transition: all 0.2s;
    }
    
    .drag-item:hover {
      background-color: #1976D2;
      transform: scale(1.05);
    }
    
    .drag-item.dragging {
      opacity: 0.5;
    }
    
    .drag-final-box .drag-item {
      background-color: #4CAF50;
    }
    
    .drag-final-box .drag-item:hover {
      background-color: #45a049;
    }
    
    .drag-over {
      background-color: #e8f5e9 !important;
      border-color: #2196F3 !important;
    }
    
    .clear-button {
      font-size: 12px;
      padding: 4px 12px;
    }
  ", inputId, width))))
  
  # JavaScript
  js <- tags$script(HTML(sprintf("
    $(document).ready(function() {
      var finalId = '%s';
      var inputId = '%s';
      var clearBtnId = '%s';
      var initialState = %s;
      
      // Función para actualizar el valor en Shiny
      function updateShinyValue() {
        var items = $('#' + finalId + ' .drag-item');
        var values = [];
        items.each(function() {
          values.push($(this).attr('data-value'));
        });
        Shiny.setInputValue(inputId, values);
      }
      
      // Función para reiniciar al estado inicial
      function resetToInitial() {
        // Limpiar el cuadro final
        $('#' + finalId).empty();
        
        // Restaurar todos los cuadros fuente a su estado inicial
        initialState.forEach(function(cuadro) {
          var boxId = cuadro.id;
          var choices = cuadro.choices;
          
          $('#' + boxId).empty();
          
          choices.forEach(function(choice) {
            var item = $('<div>')
              .addClass('drag-item')
              .attr('draggable', 'true')
              .attr('data-value', choice)
              .text(choice);
            $('#' + boxId).append(item);
          });
        });
        
        updateShinyValue();
      }
      
      // Botón de limpiar
      $('#' + clearBtnId).on('click', function() {
        resetToInitial();
      });
      
      // Manejar el inicio del drag
      $(document).on('dragstart', '.drag-item', function(e) {
        $(this).addClass('dragging');
        e.originalEvent.dataTransfer.effectAllowed = 'move';
        e.originalEvent.dataTransfer.setData('text/html', $(this).attr('data-value'));
        
        // Guardar el ID del cuadro de origen
        var sourceBox = $(this).closest('.drag-source-box, .drag-final-box');
        $(this).attr('data-source-id', sourceBox.attr('id'));
      });
      
      // Manejar el fin del drag
      $(document).on('dragend', '.drag-item', function(e) {
        $(this).removeClass('dragging');
      });
      
      // Permitir drop en el cuadro final
      $('#' + finalId).on('dragover', function(e) {
        e.preventDefault();
        e.originalEvent.dataTransfer.dropEffect = 'move';
        $(this).addClass('drag-over');
      });
      
      $('#' + finalId).on('dragleave', function(e) {
        $(this).removeClass('drag-over');
      });
      
      $('#' + finalId).on('drop', function(e) {
        e.preventDefault();
        $(this).removeClass('drag-over');
        
        var draggedItem = $('.drag-item.dragging');
        if (draggedItem.length && draggedItem.closest('.drag-source-box').length) {
          // Solo permitir desde cuadros fuente
          var clone = draggedItem.clone();
          clone.removeClass('dragging');
          $(this).append(clone);
          
          // Eliminar el original de su cuadro fuente
          draggedItem.remove();
          
          updateShinyValue();
        }
      });
      
      // Permitir drag de vuelta SOLO al cuadro de origen
      $('.drag-source-box').on('dragover', function(e) {
        var draggedItem = $('.drag-item.dragging');
        var targetBoxId = $(this).attr('id');
        var sourceBoxId = draggedItem.attr('data-source-id');
        
        // Solo permitir si el elemento viene del cuadro final Y este es su cuadro de origen
        if (draggedItem.length && 
            draggedItem.closest('.drag-final-box').length && 
            sourceBoxId === targetBoxId) {
          e.preventDefault();
          e.originalEvent.dataTransfer.dropEffect = 'move';
          $(this).addClass('drag-over');
        }
      });
      
      $('.drag-source-box').on('dragleave', function(e) {
        $(this).removeClass('drag-over');
      });
      
      $('.drag-source-box').on('drop', function(e) {
        var draggedItem = $('.drag-item.dragging');
        var targetBoxId = $(this).attr('id');
        var sourceBoxId = draggedItem.attr('data-source-id');
        
        // Solo permitir si es el cuadro de origen
        if (draggedItem.length && 
            draggedItem.closest('.drag-final-box').length && 
            sourceBoxId === targetBoxId) {
          e.preventDefault();
          $(this).removeClass('drag-over');
          
          var clone = draggedItem.clone();
          clone.removeClass('dragging');
          $(this).append(clone);
          draggedItem.remove();
          
          updateShinyValue();
        }
      });
      
      // Inicializar valor vacío
      updateShinyValue();
    });
  ", final_id, inputId, paste0(inputId, "_clear"), 
                                 jsonlite::toJSON(lapply(seq_len(n_cuadros), function(i) {
                                   list(id = cuadro_ids[i], choices = unname(choices[[i]]))
                                 }), auto_unbox = FALSE))))
  
  # Estructura completa
  tags$div(
    id = paste0(inputId, "-container"),
    css,
    if (!is.null(label)) tags$label(label, `for` = inputId),
    tags$div(
      class = "drag-sources-wrapper",
      cuadros_html
    ),
    final_box,
    js
  )
}

# Ejemplo de uso en una app Shiny
# library(shiny)
# 
# ui <- fluidPage(
#   titlePanel("Ejemplo de Drag and Drop"),
#   
#   dragDropInput(
#     inputId = "mi_seleccion",
#     label = "Arrastra las opciones al cuadro de destino:",
#     n_cuadros = 3,
#     choices = list(
#       "Frutas" = c("Manzana", "Banana", "Naranja", "Fresa"),
#       "Verduras" = c("Zanahoria", "Lechuga", "Tomate"),
#       "Colores" = c("Rojo", "Azul", "Verde", "Amarillo", "Morado")
#     ),
#     height_cuadros = "180px",
#     height_final = "120px"
#   ),
#   
#   br(),
#   
#   verbatimTextOutput("resultado")
# )
# 
# server <- function(input, output, session) {
#   output$resultado <- renderPrint({
#     cat("Elementos seleccionados:\n")
#     print(input$mi_seleccion)
#     cat("\nClase:", class(input$mi_seleccion), "\n")
#     cat("Longitud:", length(input$mi_seleccion))
#   })
# }
# 
# shinyApp(ui, server)