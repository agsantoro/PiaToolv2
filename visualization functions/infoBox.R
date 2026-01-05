infoBox = function(nombre_scn, hito, valor, intervencion) {
  
  # Generar un ID único para cada infoBox
  box_id <- paste0("infobox_", sample(1:10000, 1))
  
  HTML(
    glue(
      "<style>
        #{box_id} {{
          padding: 30px;
          border-radius: 10px;
          backdrop-filter: blur(10px);
          transition: transform 0.3s ease, box-shadow 0.3s ease;
          cursor: pointer;
          display: flex;
          flex-direction: column;
          box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
          background-color: #f9f9f9;
          margin: 15px 0;
        }}
        #{box_id}:hover {{
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
        }}
      </style>
      <div id='{box_id}'>
        <div style='display: flex; align-items: center; margin-bottom: 15px;'>
          <div style='flex: 1;'>
            <div style='
              background-color: #2196F3;
              color: white;
              padding: 8px 16px;
              border-radius: 20px;
              display: inline-block;
              font-size: 16px;
              font-weight: 500;
            '>{nombre_scn}</div>
          </div>
          <div style='margin-left: 10px;'>
            <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='#2196F3' style='width: 32px; height: 32px;'>
              <path fill-rule='evenodd' d='M18.97 3.659a2.25 2.25 0 0 0-3.182 0l-10.94 10.94a3.75 3.75 0 1 0 5.304 5.303l7.693-7.693a.75.75 0 0 1 1.06 1.06l-7.693 7.693a5.25 5.25 0 1 1-7.424-7.424l10.939-10.94a3.75 3.75 0 1 1 5.303 5.304L9.097 18.835l-.008.008-.007.007-.002.002-.003.002A2.25 2.25 0 0 1 5.91 15.66l7.81-7.81a.75.75 0 0 1 1.061 1.06l-7.81 7.81a.75.75 0 0 0 1.054 1.068L18.97 6.84a2.25 2.25 0 0 0 0-3.182Z' clip-rule='evenodd' />
            </svg>
          </div>
        </div>
        
        <hr style='border: none; border-top: 1px solid #ddd; margin: 15px 0;'>
        
        <div style='
          background-color: #4CAF50;
          color: white;
          padding: 12px 16px;
          border-radius: 8px;
          margin-bottom: 10px;
          display: flex;          /* Añadido */
          flex-direction: column; /* Añadido */
          align-items: center;    /* Centra horizontalmente los hijos */
          text-align: center;     /* Asegura el centro del texto */
        '>
          <div style='font-size: 14px; margin-bottom: 5px; opacity: 0.9;'>{hito}</div>
          <div style='font-size: 24px; font-weight: bold;'>{valor}</div>
        </div>
        
        <div style='
          padding: 10px 16px;
          background-color: #f0f8f0;
          border-radius: 8px;
          border-left: 4px solid #4CAF50;
        '>
          <span style='color: #555; font-size: 14px;'>Intervención: </span>
          <span style='font-weight: 600; color: #2c3e50;'>{intervencion}</span>
        </div>
      </div>"
    )
  )
}