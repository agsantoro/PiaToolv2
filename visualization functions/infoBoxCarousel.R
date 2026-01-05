infoBoxCarousel <- function(infoBoxes_list, carousel_id = "infoCarousel") {
  
  n_boxes <- length(infoBoxes_list)
  
  # Generar los dots indicadores
  dots_html <- paste(
    lapply(1:n_boxes, function(i) {
      active_class <- if(i == 1) "active" else ""
      sprintf('<span class="carousel-dot %s" data-slide="%d"></span>', active_class, i)
    }),
    collapse = ""
  )
  
  # Generar los slides
  slides_html <- paste(
    lapply(1:n_boxes, function(i) {
      active_class <- if(i == 1) "active" else ""
      sprintf('<div class="carousel-slide %s">%s</div>', active_class, infoBoxes_list[[i]])
    }),
    collapse = ""
  )
  
  HTML(
    glue(
      "<style>
        .carousel-container {{
          position: relative;
          width: 100%;
          overflow: hidden;
          padding: 20px 60px;
        }}
        
        .carousel-wrapper {{
          display: flex;
          transition: transform 0.5s ease-in-out;
        }}
        
        .carousel-slide {{
          min-width: 100%;
          opacity: 0;
          transition: opacity 0.5s ease-in-out;
          display: none;
        }}
        
        .carousel-slide.active {{
          opacity: 1;
          display: block;
        }}
        
        .carousel-btn {{
          position: absolute;
          top: 50%;
          transform: translateY(-50%);
          background-color: rgba(33, 150, 243, 0.8);
          color: white;
          border: none;
          width: 50px;
          height: 50px;
          border-radius: 50%;
          cursor: pointer;
          font-size: 24px;
          display: flex;
          align-items: center;
          justify-content: center;
          transition: all 0.3s ease;
          z-index: 10;
          box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
        }}
        
        .carousel-btn:hover {{
          background-color: rgba(33, 150, 243, 1);
          transform: translateY(-50%) scale(1.1);
        }}
        
        .carousel-btn-prev {{
          left: 10px;
        }}
        
        .carousel-btn-next {{
          right: 10px;
        }}
        
        .carousel-dots {{
          text-align: center;
          margin-top: 20px;
        }}
        
        .carousel-dot {{
          display: inline-block;
          width: 12px;
          height: 12px;
          border-radius: 50%;
          background-color: #ddd;
          margin: 0 5px;
          cursor: pointer;
          transition: all 0.3s ease;
        }}
        
        .carousel-dot.active {{
          background-color: #2196F3;
          width: 30px;
          border-radius: 6px;
        }}
        
        .carousel-dot:hover {{
          background-color: #64B5F6;
        }}
      </style>
      
      <div id='{carousel_id}' class='carousel-container'>
        <button class='carousel-btn carousel-btn-prev' onclick='moveSlide(\"{carousel_id}\", -1)'>‹</button>
        
        <div class='carousel-wrapper'>
          {slides_html}
        </div>
        
        <button class='carousel-btn carousel-btn-next' onclick='moveSlide(\"{carousel_id}\", 1)'>›</button>
        
        <div class='carousel-dots'>
          {dots_html}
        </div>
      </div>
      
      <script>
        (function() {{
          var currentSlide_{carousel_id} = 1;
          var totalSlides_{carousel_id} = {n_boxes};
          
          window.moveSlide = function(carouselId, direction) {{
            if (carouselId !== '{carousel_id}') return;
            
            currentSlide_{carousel_id} += direction;
            
            if (currentSlide_{carousel_id} > totalSlides_{carousel_id}) {{
              currentSlide_{carousel_id} = 1;
            }}
            if (currentSlide_{carousel_id} < 1) {{
              currentSlide_{carousel_id} = totalSlides_{carousel_id};
            }}
            
            showSlide_{carousel_id}(currentSlide_{carousel_id});
          }};
          
          function showSlide_{carousel_id}(n) {{
            var slides = document.querySelectorAll('#{carousel_id} .carousel-slide');
            var dots = document.querySelectorAll('#{carousel_id} .carousel-dot');
            
            slides.forEach(function(slide) {{
              slide.classList.remove('active');
            }});
            
            dots.forEach(function(dot) {{
              dot.classList.remove('active');
            }});
            
            if (slides[n-1]) {{
              slides[n-1].classList.add('active');
            }}
            if (dots[n-1]) {{
              dots[n-1].classList.add('active');
            }}
          }}
          
          // Click en los dots
          var dots = document.querySelectorAll('#{carousel_id} .carousel-dot');
          dots.forEach(function(dot, index) {{
            dot.addEventListener('click', function() {{
              currentSlide_{carousel_id} = index + 1;
              showSlide_{carousel_id}(currentSlide_{carousel_id});
            }});
          }});
          
          // Auto-play opcional (descomenta para activar)
          // setInterval(function() {{
          //   moveSlide('{carousel_id}', 1);
          // }}, 5000);
        }})();
      </script>
      "
    )
  )
}