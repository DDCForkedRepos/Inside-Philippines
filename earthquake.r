#Call the Packages
library(maptools)
library(ggplot2)

#Import the Data
edat <- read.csv("~/ShinyApps/Earthquake/EDataSept13.csv", header = TRUE)

#Import the Shape File
Ph <- readShapePoly("~/ShinyApps/Earthquake/Provinces.shp")

#Plot the Shape File overlayed with Data Points from edat
p <- ggplot() + 
  geom_polygon(data = Ph, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "#03C03C") + 
  coord_fixed() + geom_point(aes(x = Longitude, 
                                 y = Latitude,
                                 colour = Depth,
                                 size = Magnitude),
                             data = edat,
                             alpha = 0.80) + 
  scale_colour_gradient(low = "orange", 
                        high = "#C23B22") + 
  labs(title = "Earthquake: January 2013") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(
    size = 3, 
    colour = "black",
    fill = "white"),
        axis.ticks = element_line(
          size = 2),
        panel.grid.major = element_line(
          colour = "gray80", 
          linetype = "dotted"),
        panel.grid.minor = element_line(
          colour = "gray90", 
          linetype = "dashed"),
        axis.title.x = element_text(
          size = rel(1.2), 
          face = "bold"),
        axis.title.y = element_text(
          size = rel(1.2), 
          face = "bold"),
        plot.title = element_text(
          size = 20,
          face = "bold", 
          vjust = 1.5)) + 
  scale_x_continuous(
    limits = c(114, 130)) + 
  scale_y_continuous(
    limits = c(3, 22))

#Plot the Shape File overlayed with Data Points from edat, to include the
#two dimensional density of the edat points

p <- ggplot() + 
  geom_polygon(data = Ph, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "#03C03C") + 
  coord_fixed() + geom_point(aes(x = Longitude, 
                                 y = Latitude,
                                 colour = Depth,
                                 size = Magnitude),
                             data = edat,
                             alpha = 0.80) + 
  scale_colour_gradient(low = "orange", 
                        high = "#C23B22") + 
  stat_density2d(aes(x = Longitude, 
                     y = Latitude, 
                     fill = ..level..),
                 alpha = 0.4,
                 size = 2,
                 bins = 4,
                 data = edat,
                 geom = "polygon") + 
  scale_fill_continuous(name = "Density Level") + 
  labs(title = "Earthquake: January 2013") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(
    size = 3, 
    colour = "black",
    fill = "white"),
        axis.ticks = element_line(
          size = 2),
        panel.grid.major = element_line(
          colour = "gray80", 
          linetype = "dotted"),
        panel.grid.minor = element_line(
          colour = "gray90", 
          linetype = "dashed"),
        axis.title.x = element_text(
          size = rel(1.2), 
          face = "bold"),
        axis.title.y = element_text(
          size = rel(1.2), 
          face = "bold"),
        plot.title = element_text(
          size = 20,
          face = "bold", 
          vjust = 1.5)) + 
  scale_x_continuous(
    limits = c(114, 130)) + 
  scale_y_continuous(
    limits = c(3, 22))

