getPalette = function(n) {
  # Define the base colors for interpolation
  base_colors <- c("#D3D3D3", "#87CEEB", "#98FB98", "#F5F5DC")
  
  # Create a color interpolation function
  discreet_color_function <- colorRampPalette(base_colors)
  
  # Generate the 25-color palette
  discrete_palette_custom <- discreet_color_function(n)
  
  # View the palette
  sample(discrete_palette_custom)
  
}

