modalText = function(page) {
if (page == "map") {
  list(
    title = HTML(paste(icon("globe", style = "
    font-size: 1em; 
    color: #766bd3;
    filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1));"),"Mapas Interactivos" )),
    content = HTML(
      '
    

    <p>Este mapa utiliza <strong>colores</strong> (un <a href="#" title="Mapa de Coropletas: Un tipo de mapa temático en el que las áreas geográficas están sombreadas con colores o patrones que representan valores estadísticos de una variable en esa área.">Mapa de Coropletas</a>) para mostrar la <strong>Razón de Mortalidad Estandarizada (RME)</strong> en los distintos departamentos o partidos de Argentina, clasificada en <strong>cuatro grupos (cuartiles)</strong>.</p>

    <h4 style="margin-top: 20px; color: #4682b4;">Definiciones Clave:</h4>
    <ul style="list-style-type: disc; padding-left: 20px; font-size: 14px;">
        <li><strong>Mapa de Coropletas:</strong> Un tipo de mapa en el que las áreas geográficas (departamentos/partidos) se colorean o sombrean para representar la magnitud de una variable estadística.</li>
        <li><strong>Razón de Mortalidad Estandarizada (RME):</strong> Es una medida que compara el número de muertes observadas en una población con el número de muertes que se esperarían, ajustando por diferencias en la estructura de edad de la población. Permite comparar de forma justa la mortalidad entre distintas regiones o períodos.</li>
    </ul>

    <hr style="border-top: 1px solid #ccc; margin: 15px 0;">

    <h3>¿Qué te permite ver?</h3>

    <ol style="padding-left: 20px;">
        <li>
            <p><strong>Comparación Geográfica:</strong> Al mirar el mapa en un momento dado, podés ver fácilmente qué áreas tienen una RME <strong>significativamente más alta o más baja</strong> que el promedio nacional (representado por los cuartiles más oscuros y más claros, respectivamente).</p>
        </li>
        <li>
            <p><strong>Evolución en el Tiempo:</strong> Al <strong>cambiar el año</strong> o período de tiempo, podés observar <strong>cómo ha cambiado la mortalidad</strong> en cada región. Esto es crucial para identificar:</p>
            <ul style="list-style-type: circle; margin-top: 5px; padding-left: 20px;">
                <li><strong>Tendencias:</strong> ¿La mortalidad está empeorando o mejorando en ciertas zonas?</li>
                <li><strong>Puntos Críticos:</strong> ¿Dónde persisten las altas tasas de mortalidad?</li>
                <li><strong>Impacto de Políticas:</strong> ¿Las intervenciones o programas de salud han tenido un efecto visible en la reducción de la RME en áreas específicas?</li>
            </ul>
        </li>
    </ol>

    <p style="margin-top: 15px; background-color: #f0f8ff; padding: 10px; border-radius: 5px;">Con esta herramienta podés <strong>identificar rápidamente las áreas más vulnerables</strong> y a <strong>monitorear el progreso</strong> o el deterioro de la situación de salud a nivel territorial.</p>
    <hr>
    <p style="margin-top: 15px; background-color: #e5f5e0; padding: 10px; border-radius: 5px;">En el panel de la izquierda <strong>podés seleccionar el trienio y el grupo de causas de muerte para visualizar</strong>.</p>
'
    ) 
  )
  
  
} else if (page == "gaps") {
  list(
    title = HTML(paste(icon("not-equal", style = "
    font-size: 1em; 
    color: #766bd3;
    filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1));"),"Mapas Interactivos" )),
    content = HTML(
      '
    

    <p>Estos gráficos muestran la evolución de un indicador de mortalidad (como la <strong>Tasa de Mortalidad</strong> o la <strong>Cantidad de Defunciones Prematuras</strong>) a lo largo del tiempo para las jurisdicciones seleccionadas (provincia o CABA).</p>

    <h4 style="margin-top: 20px; color: #4682b4;">Componentes Clave:</h4>
    <ul style="list-style-type: disc; padding-left: 20px; font-size: 14px;">
        <li><strong>Indicador:</strong> Puedes seleccionar diferentes métricas, desde la <strong>Cantidad de defunciones</strong> hasta la <strong>Tasa de mortalidad ajustada por edad</strong> o las muertes <strong>prematuras</strong> (prevenibles y tratables).</li>
        <li><strong>Series:</strong> Se presentan dos series separadas para identificar las diferencias en la mortalidad según el sexo: <strong>Varones</strong> y <strong>Mujeres</strong>.</li>
    </ul>

    <hr style="border-top: 1px solid #ccc; margin: 15px 0;">

    <h3>¿Qué te permite ver?</h3>

    <ol style="padding-left: 20px;">
        <li>
            <p><strong>Tendencias Temporales:</strong> Observar si la mortalidad en la jurisdicción seleccionada está <strong>aumentando, disminuyendo o se mantiene estable</strong> a lo largo de los años para el indicador elegido.</p>
        </li>
        <li>
            <p><strong>Brecha de Sexo:</strong> Identificar la <strong>magnitud de la diferencia</strong> en la mortalidad entre varones y mujeres. </p>
        </li>
        <li>
            <p><strong>Análisis de Desigualdad (Tasas Ajustadas):</strong> Si se seleccionan <strong>tasas ajustadas por edad</strong>, el gráfico puede desplegar el <strong>Cociente de Tasas</strong>. Este cociente es una métrica de desigualdad que facilita ver cuánto mayor es la tasa de un sexo respecto al otro (por ejemplo, si el cociente es 1.5, la mortalidad en varones es 50% superior a la de mujeres).</p>
        </li>
        <li>
            <p><strong>Impacto Específico:</strong> Al seleccionar indicadores específicos (como tasas ajustadas o defunciones prevenibles/tratables), se puede evaluar la <strong>efectividad de los sistemas de salud</strong> y las políticas de prevención en la jurisdicción.</p>
        </li>
    </ol>

    <p style="margin-top: 15px; background-color: #f0f8ff; padding: 10px; border-radius: 5px;">Estos gráficos son esenciales para el <strong>análisis de la situación de salud a nivel provincial en términos comparativos</strong>, permitiendo monitorear la evolución y dirigir intervenciones específicas basadas en el género y la desigualdad.</p>
     <hr>
    <p style="margin-top: 15px; background-color: #e5f5e0; padding: 10px; border-radius: 5px;">En el panel de la izquierda se puede seleccionar el indicador a mostrar, las jurisdicciones seleccionadas y el grupo de causas de muerte.</p>
</div>

    
'
    ) 
  )
  
} else if (page == "/") {

  list(
    title = HTML('<h2 style="color: #004a99; font-size: 1em; margin-top: 0; padding-bottom: 8px;">
      Guía de Navegación del tablero 
    </h2>'),
    content = HTML('
  <div style="font-family: Arial, sans-serif; line-height: 1.6; color: #333; max-width: 100%; padding: 15px; border: 1px solid #ddd; border-radius: 8px; background-color: #ffffff;">
    
    
      
      <p style="margin-bottom: 15px;">
        <strong>El Observatorio está diseñado para ofrecer una exploración de datos intuitiva y profunda.</strong> El acceso a la información se realiza mediante un sistema de <strong>tarjetas (cards) interactivas</strong> en esta página de inicio. Simplemente <strong style="color: #cc0000;">haga clic en cualquiera de las secciones</strong> ("Gráficos Interactivos," "Mapas Interactivos," etc.) para dirigirse a la página específica de su interés.
        </p>
          
          <h3 style="color: #4a4a4a; font-size: 1.2em; margin-top: 20px;">Elementos Clave en Cada Página</h3>
            <ul style="list-style: none; padding-left: 0;">
              <li style="margin-bottom: 15px;">
                <strong style="color: #007bff;">1. Panel de Filtros:</strong> En el lateral, encontrará un panel de filtros detallado que le permitirá <strong>segmentar y personalizar los datos</strong> por jurisdicción, rango de años, causa de mortalidad u otras variables relevantes, ajustando instantáneamente las figuras mostradas.
                </li>
                  <li style="margin-bottom: 15px;">
                    <strong style="color: #007bff;">2. Figuras y Visualizaciones:</strong> El cuerpo principal de cada página contendrá las <strong>visualizaciones clave</strong> (gráficos, mapas, tablas) específicas de la sección que seleccionó.
                    </li>
                      </ul>
                      
                      <div style="background-color: #f0f8ff; border-left: 5px solid #007bff; padding: 15px; margin-top: 20px; border-radius: 4px;">
                        <h3 style="color: #007bff; font-size: 1.2em; margin-top: 0; margin-bottom: 10px;">Soporte a la Interpretación y Consultas (IA) </h3>
                          <p style="margin-bottom: 10px;">
                            Para potenciar su análisis, hemos integrado un <strong>Asistente de Inteligencia Artificial (IA).</strong> Este asistente está disponible para:
                            </p>
                            <ul style="list-style-type: circle; padding-left: 20px; margin-top: 0;">
                              <li style="margin-bottom: 5px;"><strong>Apoyar la interpretación</strong> de las figuras y tendencias que esté visualizando.</li>
                                <li style="margin-bottom: 5px;"><strong>Responder a sus consultas específicas</strong> sobre la información, metodologías o fuentes de datos.</li>
                                  </ul>
                                  </div>
                                  
                                  <h3 style="color: #4a4a4a; font-size: 1.2em; margin-top: 20px;">Descarga de Información ⬇️</h3>
                                    <p style="margin-bottom: 10px;">
                                      El Observatorio promueve el uso y la difusión de sus datos. En cada sección, tendrá la capacidad de <strong style="color: #cc0000;">descargar</strong> la información analizada:
                                        </p>
                                        <ul style="list-style-type: square; padding-left: 20px; margin-top: 0;">
                                          <li style="margin-bottom: 5px;"><strong>Imágenes (.png/.svg):</strong> Descargue las visualizaciones para presentaciones o publicaciones.</li>
                                            <li style="margin-bottom: 5px;"><strong>Tablas de Datos:</strong> Acceda a los datos crudos subyacentes en formato tabla para sus propios procesamientos.</li>
                                              <li style="margin-bottom: 5px;"><strong>Reportes:</strong> Genere y descargue reportes resumidos con las cifras y análisis principales de la sección.</li>
                                                </ul>
                                                
                                                </div>'))
}
  
  
  
  else {
  list(
    title = "Titulo",
    content = "Texto"
  )
  
  }
  
}