getHelpModalText = function(intervention) {
  glue(
    "<div style='width: 100%;'>
  
  <p style='margin: 0 0 20px 0;
            color: #103362;
            font-size: 0.95em;
            line-height: 1.6;
            text-align: left;'>
    En el panel de la izquierda puede definir los valores para los parámetros básicos y avanzados del modelo y correrlo. En el panel de la derecha observará los resultados. En el panel de botones podrá realizar las siguientes acciones:
  </p>
  
  <div style='display: grid;
              grid-template-columns: 60% auto 1fr; 
              gap: 0 15px;
              align-items: center;
              padding: 15px;
              background: #f8f9fa;
              border-radius: 12px;'>
    
    <div style='grid-column: 1; grid-row: 1 / 5; display: flex; align-items: center; justify-content: center; padding-right: 15px;'>
      <img src='screenshot_{intervention}.jpg' 
           alt='Screenshot' 
           style='width: 100%;
                  max-width: 100%;
                  height: auto;
                  border-radius: 8px;
                  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);'>
    </div>

    <div style='grid-column: 2; display: flex; align-items: center; justify-content: center; margin-left: 10px;'>
      <button type='button' 
              style='background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
                     color: white; border: none; border-radius: 50%; width: 50px; height: 50px;
                     font-size: 1.2em; display: flex; align-items: center; justify-content: center;
                     cursor: pointer; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);'>
        <i class='fa fa-table'></i>
      </button>
    </div>
    <div style='grid-column: 3; display: flex; align-items: center;'>
      <p style='margin: 0; color: #103362; font-size: 0.95em; line-height: 1.4;'>
        Compare los resultados de los escenarios guardados.
      </p>
    </div>
    
    <div style='grid-column: 2; display: flex; align-items: center; justify-content: center; margin-left: 10px; padding-top: 10px;'>
      <button type='button' 
              style='background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
                     color: white; border: none; border-radius: 50%; width: 50px; height: 50px;
                     font-size: 1.2em; display: flex; align-items: center; justify-content: center;
                     cursor: pointer; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);'>
        <i class='fa fa-save'></i>
      </button>
    </div>
    <div style='grid-column: 3; display: flex; align-items: center; padding-top: 10px;'>
      <p style='margin: 0; color: #103362; font-size: 0.95em; line-height: 1.4;'>
        Guarde los resultados para hacer comparaciones.
      </p>
    </div>
    
    <div style='grid-column: 2; display: flex; align-items: center; justify-content: center; margin-left: 10px; padding-top: 10px;'>
      <button type='button' 
              style='background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
                     color: white; border: none; border-radius: 50%; width: 50px; height: 50px;
                     font-size: 1.2em; display: flex; align-items: center; justify-content: center;
                     cursor: pointer; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);'>
        <i class='fa fa-rocket'></i>
      </button>
    </div>
    <div style='grid-column: 3; display: flex; align-items: center; padding-top: 10px;'>
      <p style='margin: 0; color: #103362; font-size: 0.95em; line-height: 1.4;'>
        Borre los resultados en pantalla para generar un escenario nuevo.
      </p>
    </div>
    
    <div style='grid-column: 2; display: flex; align-items: center; justify-content: center; margin-left: 10px; padding-top: 10px;'>
      <button type='button' 
              style='background: linear-gradient(135deg, #2C5F8B 0%, #4A90A4 100%);
                     color: white; border: none; border-radius: 50%; width: 50px; height: 50px;
                     font-size: 1.2em; display: flex; align-items: center; justify-content: center;
                     cursor: pointer; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);'>
        <i class='fa fa-download'></i>
      </button>
    </div>
    <div style='grid-column: 3; display: flex; align-items: center; padding-top: 10px;'>
      <p style='margin: 0; color: #103362; font-size: 0.95em; line-height: 1.4;'>
        Descargue los resultados del escenario en pantalla.
      </p>
    </div>
    
  </div>
</div>"
  )
  
}

