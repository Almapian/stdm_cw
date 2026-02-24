# Load the required libraries
#library(maptools)
library(lattice)
library(spdep)
#library(sp)
library(sf)
library(sftime)
#library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)
library(stars)
library(reshape)

japan <- st_read("japan_earthquakes_2000_2025.gpkg")

str(japan)

japan$time <- ymd_hms(japan$time)
class(japan$time)
jpLagged <- data.frame(t=japan[2:(length(japan))], t_minus_1=japan[1:(length(japan)-1)])


p1 <- ggplot(japan, aes(x=japan$time, y=japan$mag)) + geom_line()
p2 <- ggplot(ChLagged, aes(x=t, y=t_minus_1)) +
  geom_point() +
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  ggplot2::annotate("text", 8.5, 10, label=paste("r =", round(cor(ChLagged$t, ChLagged$t_minus_1), 3))) # Calculate PMCC

grid.arrange(p1,p2, nrow=1)
