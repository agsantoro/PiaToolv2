dragDropInput <- function(inputId, label, n_cuadros, choices, width = "100%", 
                          height_cuadros = "100px", height_final = "auto") {
  
  if (!is.list(choices) || is.null(names(choices))) stop("choices debe ser una lista nombrada")
  if (length(choices) != n_cuadros) stop("El número de cuadros debe coincidir con la longitud de choices")
  
  cuadro_ids <- paste0(inputId, "_cuadro_", seq_len(n_cuadros))
  final_id <- paste0(inputId, "_final")
  
  # 1. HTML de cuadros fuente
  cuadros_html <- lapply(seq_len(n_cuadros), function(i) {
    cuadro_name <- names(choices)[i]
    cuadro_choices <- choices[[i]]
    
    tags$div(
      class = "drag-source-container",
      tags$h4(cuadro_name, style = "margin-top: 0; font-size: 14px; font-weight: bold;"),
      tags$div(
        id = cuadro_ids[i],
        class = "drag-source-box",
        style = sprintf("height: %s; overflow-y: auto; margin-bottom: 15px;", height_cuadros),
        lapply(names(cuadro_choices), function(label_html) {
          value <- cuadro_choices[[label_html]]
          tags$div(
            class = "drag-item",
            draggable = "true",
            `data-value` = value,
            HTML(label_html)
          )
        })
      )
    )
  })
  
  # 2. HTML del cuadro final
  final_box <- tags$div(
    class = "drag-final-main-container",
    style = "display: flex; flex-direction: column; height: 100%;", # Ocupa todo el alto
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
      tags$h4("Escenarios seleccionados para comparación", style = "margin: 0; font-size: 16px;"),
      tags$button(
        id = paste0(inputId, "_clear"),
        class = "btn btn-sm btn-warning clear-button",
        type = "button",
        "Limpiar"
      )
    ),
    tags$div(
      id = final_id,
      class = "drag-final-box",
      style = "flex-grow: 1; min-height: 200px;" # Se expande hasta el final
    )
  )
  
  # 3. CSS ACTUALIZADO
  css <- tags$head(tags$style(HTML(sprintf("
    #%s-container {
      width: %s;
      
    }
    
    .drag-main-layout {
      display: flex;
      gap: 20px;
      align-items: stretch; /* Importante: estira ambas columnas por igual */
    }
    
    .drag-column-left {
      flex: 1;
      display: flex;
      flex-direction: column;
    }
    
    .drag-column-right {
      flex: 1;
    }

    .drag-source-box {
      border: 2px solid #ddd;
      border-radius: 8px;
      padding: 8px;
      background-color: #f9f9f9;
    }
    
    .drag-final-box {
      border: 2px dashed #4CAF50;
      border-radius: 8px;
      padding: 10px;
      background-color: #f0f8f0;
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
      align-content: flex-start;
      overflow-y: auto;
    }
    
    .drag-item {
      background-color: #2196F3;
      color: white;
      padding: 5px 10px;
      border-radius: 15px;
      cursor: move;
      display: inline-flex;
      align-items: center;
      margin: 3px;
      font-size: 12px;
      transition: all 0.2s;
    }
    
    .drag-item:hover { background-color: #1976D2; }
    .drag-final-box .drag-item { background-color: #4CAF50; }
    .drag-over { background-color: #e8f5e9 !important; border-color: #2196F3 !important; }
  ", inputId, width))))
  
  # 4. JavaScript (Sin cambios requeridos en la lógica)
  js <- tags$script(HTML(sprintf("
    $(document).ready(function() {
      var finalId = '%s';
      var inputId = '%s';
      var clearBtnId = '%s';
      var initialState = %s;
      
      function updateShinyValue() {
        var values = [];
        $('#' + finalId + ' .drag-item').each(function() {
          values.push($(this).attr('data-value'));
        });
        Shiny.setInputValue(inputId, values);
      }
      
      $('#' + clearBtnId).on('click', function() {
        $('#' + finalId).empty();
        initialState.forEach(function(cuadro) {
          var box = $('#' + cuadro.id).empty();
          cuadro.items.forEach(function(item) {
            $('<div>').addClass('drag-item').attr('draggable', 'true')
              .attr('data-value', item.value).html(item.label).appendTo(box);
          });
        });
        updateShinyValue();
      });

      $(document).on('dragstart', '.drag-item', function(e) {
        $(this).addClass('dragging');
        var sourceBox = $(this).closest('.drag-source-box, .drag-final-box');
        $(this).attr('data-source-id', sourceBox.attr('id'));
        $(this).attr('data-html-content', $(this).html());
      });

      $(document).on('dragend', '.drag-item', function() { $(this).removeClass('dragging'); });

      $('#' + finalId + ', .drag-source-box').on('dragover', function(e) { 
        e.preventDefault(); $(this).addClass('drag-over'); 
      }).on('dragleave', function() { $(this).removeClass('drag-over'); });

      $('#' + finalId).on('drop', function(e) {
        e.preventDefault(); $(this).removeClass('drag-over');
        var draggedItem = $('.drag-item.dragging');
        if (draggedItem.length && draggedItem.closest('.drag-source-box').length) {
          draggedItem.clone().removeClass('dragging').html(draggedItem.attr('data-html-content')).appendTo($(this));
          draggedItem.remove();
          updateShinyValue();
        }
      });

      $('.drag-source-box').on('drop', function(e) {
        e.preventDefault(); $(this).removeClass('drag-over');
        var draggedItem = $('.drag-item.dragging');
        if (draggedItem.length && draggedItem.closest('.drag-final-box').length && draggedItem.attr('data-source-id') === $(this).attr('id')) {
          draggedItem.clone().removeClass('dragging').html(draggedItem.attr('data-html-content')).appendTo($(this));
          draggedItem.remove();
          updateShinyValue();
        }
      });
    });
  ", final_id, inputId, paste0(inputId, "_clear"), 
                                 jsonlite::toJSON(lapply(seq_len(n_cuadros), function(i) {
                                   list(id = cuadro_ids[i], items = lapply(names(choices[[i]]), function(h) list(value=choices[[i]][[h]], label=h)))
                                 }), auto_unbox = TRUE))))
  
  # 5. ESTRUCTURA FINAL
  tags$div(
    id = paste0(inputId, "-container"),
    css,
    if (!is.null(label)) tags$label(label, style="font-weight: bold; margin-bottom: 10px; display: block;"),
    tags$div(
      class = "drag-main-layout",
      tags$div(class = "drag-column-left", cuadros_html),
      tags$div(class = "drag-column-right", final_box)
    ),
    js
  )
}