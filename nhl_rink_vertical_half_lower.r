#load in packages
library(tidyverse)
library(ggplot2)

# Setting up color values
NHL_red <- "#C8102E" # Use #C8102E for original red in the rules, #FFE6EB for lighter hue
NHL_blue <- "#0033A0" # Use #0033A0 for original blue in the rules, #E6EFFF for lighter hue
NHL_light_blue <- "#41B6E6" # Use #41B6E6 for original crease blue in the rules, #E6F9FF for lighter hue

nhl_rink_vertical_half_lower <- function () {
  
  # Plotting an NHL rink completely following the NHL rule book:
  # https://cms.nhl.bamgrid.com/images/assets/binary/308893668/binary-file/file.pdf
  # Line widths, lengths, colors, all followed as closely as possible
  
  ggplot() +
    
    # Faceoff circles
    geom_arc(aes(x0 = 0, y0 = 0, r = 15, start = pi / 2, end = 3 * pi / 2), color = NHL_blue) + #Half of Center Circle
    geom_circle(aes(x0 = 22, y0 = -69, r = 15), color = NHL_red, linewidth = 2 / 12) + # Bottom-Right
    geom_circle(aes(x0 = -22, y0 = -69, r = 15), color = NHL_red, linewidth = 2 / 12) + # Bottom-Left
    
    # Hash marks in T-R/B-R/T-L/B-R order, groups of four
    geom_tile(aes(x = 37.77, y = -66.125, width = 2, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 6.23, y = -66.125, width = 2, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 37.77, y = -71.875, width = 2, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 6.23, y = -71.875, width = 2, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -37.77, y = -66.125, width = 2, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -6.23, y = -66.125, width = 2, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -37.77, y = -71.875, width = 2, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -6.23, y = -71.875, width = 2, height = 2 / 12), fill = NHL_red) +
    
    # Center line
    geom_tile(aes(x = 0, y = 0, width = 85, height = 1), fill = NHL_red) + # Center line
    
    # Faceoff dots - Plot AFTER center lines for center ice circle to show up above
    geom_circle(aes(x0 = 0, y0 = 0, r = 6 / 12), color = "#FF99B4", fill = "#FF99B4", linewidth = 0) + # Center dot with unique red
    geom_circle(aes(x0 = 22, y0 = -69, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Bottom-Right
    geom_circle(aes(x0 = -22, y0 = -69, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Bottom-Left
 
    geom_circle(aes(x0 = 22, y0 = -20.5, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Bottom-Right
    geom_circle(aes(x0 = -22, y0 = -20.5, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Bottom-Left
    
    # Ells surrounding faceoff dots
    geom_tile(aes(y = -65, x = 22.83, width = 2 / 12, height = 4), fill = NHL_red) + # Bottom-Right
    geom_tile(aes(y = -73, x = 22.83, width = 2 / 12, height = 4), fill = NHL_red) +
    geom_tile(aes(y = -65, x = 21.17, width = 2 / 12, height = 4), fill = NHL_red) +
    geom_tile(aes(y = -73, x = 21.17, width = 2 / 12, height = 4), fill = NHL_red) +
    geom_tile(aes(y = -66.92, x = 24.25, width = 3, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(y = -71.08, x = 24.25, width = 3, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(y = -66.92, x = 19.75, width = 3, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(y = -71.08, x = 19.75, width = 3, height = 2 / 12), fill = NHL_red) +
    
    geom_tile(aes(y = -65, x = -22.83, width = 2 / 12, height = 4), fill = NHL_red) + # Bottom-Left
    geom_tile(aes(y = -73, x = -22.83, width = 2 / 12, height = 4), fill = NHL_red) +
    geom_tile(aes(y = -65, x = -21.17, width = 2 / 12, height = 4), fill = NHL_red) +
    geom_tile(aes(y = -73, x = -21.17, width = 2 / 12, height = 4), fill = NHL_red) +
    geom_tile(aes(y = -66.92, x = -24.25, width = 3, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(y = -71.08, x = -24.25, width = 3, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(y = -66.92, x = -19.75, width = 3, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(y = -71.08, x = -19.75, width = 3, height = 2 / 12), fill = NHL_red) +
    
    # Bottom goalie crease
    geom_tile(aes(y = -86.75, x = 0, width = 8, height = 4.5), fill = NHL_light_blue) +
    geom_arc_bar(aes(y0 = -89, x0 = 0, start = (pi / 2) - atan(4.5/4) - 0.01, end = (-pi / 2) + atan(4.5 / 4) + 0.01, r0 = 4, r = 6), 
                 fill = NHL_light_blue, color = NHL_light_blue, linewidth = 1 / 12) + # manually adjusted arc
    geom_tile(aes(y = -86.75, x = -4, width = 2 / 12, height = 4.5), fill = NHL_red) +
    geom_tile(aes(y = -86.75, x = 4, width = 2 / 12, height = 4.5), fill = NHL_red) +
    geom_arc(aes(y0 = -89, x0 = 0, start = (pi / 2) - atan(4.5/4) - 0.01, end = (-pi / 2) + atan(4.5 / 4) + 0.01, r = 6), 
             color = NHL_red, linewidth = 2 / 12) + # manually adjusted arc
    geom_tile(aes(y = -85, x = 3.75, width = 0.42, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(y = -85, x = -3.75, width = 0.42, height = 2 / 12), fill = NHL_red) +
    
    # Goalie nets placed as rectangles
    geom_tile(aes(y = -90.67, x = 0, width = 6, height = 3.33), fill = "#E5E5E3") + # Bottom # with grey fills
    
    # Trapezoids
    geom_polygon(aes(y = c(-100, -100, -89, -89), x = c(10.92, 11.08, 7.08, 6.92)), fill = NHL_red) + # Bottom
    geom_polygon(aes(y = c(-100, -100, -89, -89), x = c(-10.92, -11.08, -7.08, -6.92)), fill = NHL_red) + # Bottom 
    
    # Lines
    geom_tile(aes(y = -25.5, x = 0, width = 85, height = 1), fill = NHL_blue) + # Bottom Blue line
    geom_tile(aes(y = -89, x = 0, width = 73.50, height = 2 / 12), fill = NHL_red) + # Bottom goal line (73.5 value is rounded from finding intersect of goal line and board radius)
    
    # Borders as line segments - plotted last to cover up line ends, etc.
    geom_line(aes(y = c(-72, 0.5), x = c(42.5, 42.5))) + # Right
    geom_line(aes(y = c(-72, 0.5), x = c(-42.5, -42.5))) + # Left
    geom_line(aes(y = c(-100, -100), x = c(-14.5, 14.5))) + # Bottom
    geom_arc(aes(y0 = -72, x0 = 14.5, start = pi, end = pi / 2, r = 28)) + # Bottom-Right
    geom_arc(aes(y0 = -72, x0 = -14.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
    
    # Fixed scale for the coordinate system  
    coord_fixed() + 
    
    #add a white background for the plot
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA)
    ) +
    
    #remove any labels and leave only the rink plot
    theme_void()
}