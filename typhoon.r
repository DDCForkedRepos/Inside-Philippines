#Call the Packages
library(ggplot2)
library(maptools)
library(OpenStreetMap)

#Function for label of Grade Variable
category <- function(x){
  a <- character()
  for(i in 1:length(x)){
    if(x[i] == 2){
      a[i] <- "TD" #Tropical Depression
    }
    if(x[i] == 3){
      a[i] <- "TS" #Tropical Storm
    }
    if(x[i] == 4){
      a[i] <- "STS" #Severe Tropical Storm
    }
    if(x[i] == 5){
      a[i] <- "TY" #Typhoon
    }
    if(x[i] == 6){
      a[i] <- "L" #Extra-Tropical Cyclone
    }
  }
  return(a)
}

#Import the Typhoon Track Data. Source: RSMC Best Track
tydat <- read.csv("~/ShinyApps/TyphoonTrack/TyDatNando2013.csv", header = TRUE)

#Transform the Longitude to Unit 0.1
tydat$CLongitude <- tydat$CLongitude * 0.1

#Transform the Latitude to Unit 0.1
tydat$CLatitude <- tydat$CLatitude * 0.1

#Label the Grade
tydat$Category <-  category(tydat$Grade)

#Create the Map
map <- openmap(c(27.605671,103.256834),
               c(1.757537,150.4541),
               minNumTiles=4,type="mapquest-aerial",zoom = 6)

#Apply openproj to use the map in ggplot2 package
map <- openproj(map)

#Plot the Map overlayed with the Typhoon Track
p <- autoplot(map) +
  geom_point(data = tydat, 
             aes(x = CLongitude, 
                 y = CLatitude),
             alpha = 0.5,
             colour = "orange",
             size = 2) + 
  geom_point(data = tydat, 
             aes(x = CLongitude, 
                 y = CLatitude),
             alpha = 0.3,
             colour = "white",
             size = tydat$MaxSpeed*0.4) + 
  geom_path(data = tydat, 
            aes(x = CLongitude, 
                y = CLatitude),
            alpha = 0.7,
            colour = "orange") + 
  geom_point(data = tydat, 
             aes(x = CLongitude, 
                 y = CLatitude),
             alpha = 0.5,
             colour = "yellow",
             size = tydat$MaxSpeed*0.25) +
  geom_point(data = tydat, 
             aes(x = CLongitude, 
                 y = CLatitude,
                 colour = MaxSpeed),
             alpha = 0.6,
             size = tydat$MaxSpeed*0.2) +
  scale_colour_gradient(
    name = "Wind Speed (Knot)",
    low = "yellow", 
    high = "red") +
  geom_text(data = tydat, 
            aes(x = CLongitude, 
                y = CLatitude,
                label = Category,
                family = "serif",
                fontface = "bold"),
            colour = "white",
            size = rel(3.5)) +
  labs(title = "Severe Tropical Storm | Nando (Kong-rey) | August 25 - August 30, 2013") +
  xlab(expression(bold("Longitude"))) + 
  ylab(expression(bold("Latitude")))  +
  theme(panel.background = element_rect(
    size = 3, 
    colour = "black",
    fill = "white"),
        axis.ticks = element_line(
          size = 2),
        axis.title.x = element_text(
          size = rel(1.2), 
          face = "bold"),
        axis.title.y = element_text(
          size = rel(1.2), 
          face = "bold"),
        plot.title = element_text(
          size = 20,
          face = "bold", 
          vjust = 1.5),
        legend.position = "bottom",
        legend.title = element_text(
          size=rel(1.2), 
          face="bold")) + 
  coord_fixed()
p

