# Función para crear el input con drag and drop (CON SOPORTE PARA HTML)
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
        lapply(names(cuadro_choices), function(label_html) {
          value <- cuadro_choices[[label_html]]
          tags$div(
            class = "drag-item",
            draggable = "true",
            `data-value` = value,
            HTML(label_html)  # CAMBIO CLAVE: Usar HTML() para el label
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
  
  # CSS (AGREGADO: estilos para imágenes)
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
      display: inline-flex;
      align-items: center;
      margin: 4px;
      user-select: none;
      transition: all 0.2s;
    }
    
    .drag-item img {
      display: inline-block !important;
      vertical-align: middle !important;
      margin-right: 8px;
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
  
  # JavaScript (MODIFICADO para soportar HTML)
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
        $('#' + finalId).empty();
        
        initialState.forEach(function(cuadro) {
          var boxId = cuadro.id;
          var items = cuadro.items;
          
          $('#' + boxId).empty();
          
          items.forEach(function(item) {
            var div = $('<div>')
              .addClass('drag-item')
              .attr('draggable', 'true')
              .attr('data-value', item.value)
              .html(item.label);  // CAMBIO: usar .html() en lugar de .text()
            $('#' + boxId).append(div);
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
        
        var sourceBox = $(this).closest('.drag-source-box, .drag-final-box');
        $(this).attr('data-source-id', sourceBox.attr('id'));
        $(this).attr('data-html-content', $(this).html());  // NUEVO: guardar HTML
      });
      
      $(document).on('dragend', '.drag-item', function(e) {
        $(this).removeClass('dragging');
      });
      
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
          var clone = draggedItem.clone();
          clone.removeClass('dragging');
          clone.html(draggedItem.attr('data-html-content'));  // CAMBIO: restaurar HTML
          $(this).append(clone);
          draggedItem.remove();
          updateShinyValue();
        }
      });
      
      $('.drag-source-box').on('dragover', function(e) {
        var draggedItem = $('.drag-item.dragging');
        var targetBoxId = $(this).attr('id');
        var sourceBoxId = draggedItem.attr('data-source-id');
        
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
        
        if (draggedItem.length && 
            draggedItem.closest('.drag-final-box').length && 
            sourceBoxId === targetBoxId) {
          e.preventDefault();
          $(this).removeClass('drag-over');
          
          var clone = draggedItem.clone();
          clone.removeClass('dragging');
          clone.html(draggedItem.attr('data-html-content'));  // CAMBIO: restaurar HTML
          $(this).append(clone);
          draggedItem.remove();
          updateShinyValue();
        }
      });
      
      updateShinyValue();
    });
  ", final_id, inputId, paste0(inputId, "_clear"), 
                                 jsonlite::toJSON(lapply(seq_len(n_cuadros), function(i) {
                                   list(
                                     id = cuadro_ids[i], 
                                     items = lapply(names(choices[[i]]), function(label_html) {
                                       list(
                                         value = choices[[i]][[label_html]],
                                         label = label_html  # Guardar el HTML como label
                                       )
                                     })
                                   )
                                 }), auto_unbox = TRUE))))
  
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