plotBox = function (title, text, iconType, iconColor, outputIdName) {
  div(
    div(
      
      style = "background: rgba(170, 202, 228, 0.3);
                   padding-top: 10px; 
                   padding-bottom: 10px;
                   padding-left: 30px;
                   padding-right: 30px;
                   border-radius: 10px;
                   backdrop-filter: blur(10px); 
                   transition: transform 0.3s ease, box-shadow 0.3s ease;
                   cursor: pointer;
                   display: flex;
                   flex-direction: column;
                   height: 100%;
      box-shadow: 0 10px 25px rgba(0,0,0,0.2);",
      
      
      uiOutput(outputIdName)
    )
  )
  
}

