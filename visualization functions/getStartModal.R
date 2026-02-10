getStartModal = function () {
  "<div class='pia-modal-wrapper'>
    <style>
        @import url('https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600;700&display=swap');

        /* Contenedor principal con imagen de fondo simple */
        .pia-modal-wrapper {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
            font-family: 'Open Sans', sans-serif;
            
            position: relative;
            min-height: 70vh;
            display: flex;
            flex-direction: column;
            overflow: hidden;
            background: url('landing-op2.jpg');
            background-size: cover;
            background-position: center;
            background-repeat: no-repeat;
            border-radius: 0;
        }

        header.pia-header {
            position: relative;
            z-index: 10;
            padding: 12px 40px;
            display: flex;
            align-items: center;
            background: rgba(255, 255, 255, 0.98);
            box-shadow: 0 4px 15px rgba(0,0,0,0.3);
            justify-content: space-between;
        }

        .logo-paho {
            height: 50px;
            width: auto;
        }

        .page-title {
            font-size: 22px;
            font-weight: 700;
            color: #1a1a1a;
            margin: 0;
        }

        .content-main {
            position: relative;
            z-index: 5;
            flex: 1;
            display: flex;
            flex-direction: column;
            align-items: flex-start;
            justify-content: center;
            padding: 50px 60px;
            gap: 30px;
            animation: fadeIn 1.2s ease-out;
        }

        .info-box {
            background: transparent;
            padding: 0;
            border-radius: 0;
            box-shadow: none;
            max-width: 500px;
            text-align: left;
            border: none;
        }

        .info-box p {
            font-size: 19px;
            line-height: 1.6;
            color: #ffffff;
            margin: 0;
        }

        .cta-button {
            background: #f0682a;
            color: #ffffff;
            padding: 14px 40px;
            border-radius: 8px;
            font-size: 16px;
            font-weight: 700;
            text-transform: uppercase;
            border: none;
            cursor: pointer;
            box-shadow: 0 6px 20px rgba(240, 104, 42, 0.4);
            transition: all 0.5s ease;
            opacity: 0;
            animation: fadeInButton 1s ease-out 4s forwards, pulse 2s infinite 4s;
        }

        .cta-button:hover {
            transform: translateY(-2px);
            box-shadow: 0 8px 25px rgba(240, 104, 42, 0.6);
        }

        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(20px); }
            to { opacity: 1; transform: translateY(0); }
        }

        @keyframes fadeInButton {
            from { 
                opacity: 0; 
                transform: translateY(20px); 
            }
            to { 
                opacity: 1; 
                transform: translateY(0); 
            }
        }

        @keyframes pulse {
            0%, 100% { transform: scale(1); }
            50% { transform: scale(1.03); }
        }
        
        .pulse-highlight { 
            animation: pulse 1s ease-out; 
        }
        
    </style>
    
    <main class='content-main'>
        <div class='info-box'>
            <p>
                La Herramienta <strong>PIA Tool</strong> es una solución interactiva diseñada para guiar a los usuarios en la evaluación del impacto epidemiológico y económico de intervenciones prioritarias identificadas por la <strong>Organización Panamericana de la Salud</strong> y la <strong>Organización Mundial de la Salud</strong>.
            </p>
        </div>

        <button class='cta-button' id='boton_ingresar'>
            Ingresar a la Aplicación
        </button>
    </main>
</div>"
}