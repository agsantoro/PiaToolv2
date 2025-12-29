getStartModal = function () {
  "<div class='pia-modal-wrapper'>
    <style>
        @import url('https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600;700&display=swap');

        /* Contenedor principal sin bordes ni footer */
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
            background: linear-gradient(rgba(0, 0, 0, 0.55), rgba(0, 0, 0, 0.55)), 
                        url('https://www.paho.org/sites/default/files/better-care-ninos_0.jpg');
            background-size: cover;
            background-position: center;
            border-radius: 15px;
        }

        .bg-shapes {
            position: absolute;
            width: 100%;
            height: 100%;
            overflow: hidden;
            z-index: 1;
        }

        .shape-orange-top {
            position: absolute;
            opacity: 0.7;
            width: 450px;
            height: 450px;
            background: linear-gradient(135deg, #ff6f00, #ff8f00);
            border-radius: 50%;
            top: -225px;
            left: -150px;
            clip-path: polygon(0 0, 100% 0, 50% 100%);
            animation: scaleIn 0.8s ease-out;
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
            align-items: center;
            justify-content: center;
            padding: 30px;
            gap: 25px;
            animation: fadeIn 1.2s ease-out;
        }

        .info-box {
            background: rgba(255, 255, 255, 0.94);
            opacity: 60%;
            padding: 35px 45px;
            border-radius: 12px;
            box-shadow: 0 12px 40px rgba(0,0,0,0.4);
            max-width: 650px;
            text-align: center;
            border-bottom: 5px solid #ff6f00;
        }

        .info-box p {
            font-size: 19px;
            line-height: 1.5;
            color: #222;
            margin: 0;
        }

        /* Botón más pequeño */
        .cta-button {
            background: linear-gradient(135deg, #ffb74d, #ffa726);
            color: #1a1a1a;
            padding: 14px 40px;
            border-radius: 50px;
            font-size: 16px;
            font-weight: 700;
            text-transform: uppercase;
            border: none;
            cursor: pointer;
            box-shadow: 0 6px 20px rgba(255, 152, 0, 0.4);
            transition: all 0.3s ease;
            animation: pulse 2s infinite;
        }

        .cta-button:hover {
            transform: translateY(-2px);
            box-shadow: 0 8px 25px rgba(255, 152, 0, 0.6);
        }

        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(20px); }
            to { opacity: 1; transform: translateY(0); }
        }

        @keyframes scaleIn {
            from { opacity: 0; transform: scale(0.8); }
            to { opacity: 1; transform: scale(1); }
        }

        @keyframes pulse {
            0%, 100% { transform: scale(1); }
            50% { transform: scale(1.03); }
        }
    </style>

    <div class='bg-shapes'>
        <div class='shape-orange-top'></div>
    </div>

    
    <main class='content-main'>
        <div class='info-box'>
            <p>
                La Herramienta <strong>PIA Tool</strong> es una solución interactiva diseñada para guiar a los usuarios en la evaluación del impacto epidemiológico y económico de intervenciones prioritarias identificadas por la <strong>Organización Mundial de la Salud (OMS)</strong>.
            </p>
        </div>

        <button class='cta-button' id='boton_ingresar'>
            Ingresar a la Aplicación
        </button>
    </main>
</div>"
}
