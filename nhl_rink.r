#load in packages
library(tidyverse)
library(ggplot2)
library(ggforce)

# Setting up color values
NHL_red <- "#C8102E" # Use #C8102E for original red in the rules, #FFE6EB for lighter hue
NHL_blue <- "#0033A0" # Use #0033A0 for original blue in the rules, #E6EFFF for lighter hue
NHL_light_blue <- "#41B6E6" # Use #41B6E6 for original crease blue in the rules, #E6F9FF for lighter hue

nhl_rink <- function () {
  
  # Plotting an NHL rink completely following the NHL rule book:
  # https://cms.nhl.bamgrid.com/images/assets/binary/308893668/binary-file/file.pdf
  # Line widths, lengths, colors, all followed as closely as possible
  
  ggplot() +
    
    # Faceoff circles
    geom_circle(aes(x0 = 0, y0 = 0, r = 15), color = NHL_blue, linewidth = 2 / 12) + # Center
    geom_circle(aes(x0 = 69, y0 = 22, r = 15), color = NHL_red, linewidth = 2 / 12) + # Top-Right
    geom_circle(aes(x0 = 69, y0 = -22, r = 15), color = NHL_red, linewidth = 2 / 12) + # Bottom-Right
    geom_circle(aes(x0 = -69, y0 = 22, r = 15), color = NHL_red, linewidth = 2 / 12) + # Top-Left
    geom_circle(aes(x0 = -69, y0 = -22, r = 15), color = NHL_red, linewidth = 2 / 12) + # Bottom-Left
    
    # Hash marks in T-R/B-R/T-L/B-R order, groups of four
    geom_tile(aes(x = 66.125, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    
    # Center line
    geom_tile(aes(x = 0, y = 0, width = 1, height = 85), fill = NHL_red) + # Center line
    
    # Faceoff dots - Plot AFTER center lines for centre ice circle to show up above
    geom_circle(aes(x0 = 0, y0 = 0, r = 6 / 12), color = "#FF99B4", fill = "#FF99B4", linewidth = 0) + # Center dot with unique red
    geom_circle(aes(x0 = 69, y0 = 22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Top-Right
    geom_circle(aes(x0 = 69, y0 = -22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Bottom-Right
    geom_circle(aes(x0 = -69, y0 = 22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Top-Left
    geom_circle(aes(x0 = -69, y0 = -22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Bottom-Left
    
    geom_circle(aes(x0 = 20.5, y0 = 22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Top-Right
    geom_circle(aes(x0 = 20.5, y0 = -22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Bottom-Right
    geom_circle(aes(x0 = -20.5, y0 = 22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Top-Left
    geom_circle(aes(x0 = -20.5, y0 = -22, r = 1), color = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Bottom-Left
    
    # Ells surrounding faceoff dots
    geom_tile(aes(x = 65, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Right
    geom_tile(aes(x = 73, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 65, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 73, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = 65, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Right
    geom_tile(aes(x = 73, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 65, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 73, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = -65, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Left
    geom_tile(aes(x = -73, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -65, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -73, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = -65, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Left
    geom_tile(aes(x = -73, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -65, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -73, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    # Referee crease
    geom_arc(aes(x0 = 0, y0 = -42.5, start = -pi / 2, end = pi / 2, r = 10), color = NHL_red) +
    
    # Left goalie crease
    geom_tile(aes(x = -86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
    geom_arc_bar(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), 
                 fill = NHL_light_blue, color = NHL_light_blue, linewidth = 1 / 12) + # manually adjusted arc
    geom_tile(aes(x = -86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_arc(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r = 6), 
             color = NHL_red, linewidth = 2 / 12) + # manually adjusted arc
    geom_tile(aes(x = -85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    geom_tile(aes(x = -85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    
    # Right goalie crease
    geom_tile(aes(x = 86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
    geom_arc_bar(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r0 = 4, r = 6), 
                 fill = NHL_light_blue, color = NHL_light_blue, linewidth = 1 / 12) + # manually adjusted arc
    geom_tile(aes(x = 86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_arc(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r = 6), 
             color = NHL_red, linewidth = 2 / 12) + # manually adjusted arc
    geom_tile(aes(x = 85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    geom_tile(aes(x = 85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    
    # Goalie nets placed as rectangles
    geom_tile(aes(x = -90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Left # with grey fills
    geom_tile(aes(x = 90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Right
    
    # Trapezoids
    geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(10.92, 11.08, 7.08, 6.92)), fill = NHL_red) + # Left
    geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = NHL_red) + # Left 
    geom_polygon(aes(x = c(100, 100, 89, 89), y = c(10.92, 11.08, 7.08, 6.92)), fill = NHL_red) + # Right
    geom_polygon(aes(x = c(100, 100, 89, 89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = NHL_red) + # Right
    
    # Lines
    geom_tile(aes(x = -25.5, y = 0, width = 1, height = 85), fill = NHL_blue) + # Left Blue line
    geom_tile(aes(x = 25.5, y = 0, width = 1, height = 85),  fill = NHL_blue) + # Right Blue line
    geom_tile(aes(x = -89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Left goal line (73.5 value is rounded from finding intersect of goal line and board radius)
    geom_tile(aes(x = 89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Right goal line
    
    # Borders as line segments - plotted last to cover up line ends, etc.
    geom_line(aes(x = c(-72, 72), y = c(42.5, 42.5))) + # Top
    geom_line(aes(x = c(-72, 72), y = c(-42.5, -42.5))) + # Bottom
    geom_line(aes(x = c(-100, -100), y = c(-14.5, 14.5))) + # Left
    geom_line(aes(x = c(100, 100), y = c(-14.5, 14.5))) + # Right
    geom_arc(aes(x0 = 72, y0 = 14.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
    geom_arc(aes(x0 = 72, y0 = -14.5, start = pi, end =  pi / 2, r = 28)) + # Bottom-Right
    geom_arc(aes(x0 = -72, y0 = 14.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
    geom_arc(aes(x0 = -72, y0 = -14.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
    
    
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