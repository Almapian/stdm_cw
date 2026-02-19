# Import libraries
library(sp)
library(rworldmap)
library(tidyverse)
library(sf)
library(stars)
library(lubridate)
library(tmap)
library(leaflet)
library(conflicted)
source("./utils/mapper.R")


e_data <- read.csv("./usgs_earthquake_data_2000_2025.csv")


# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{
  countriesSP <- getMap(resolution='low')

  # convert our list of points to a SpatialPoints object

  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))

  # use 'over' to get indices of the Polygons object containing each point
  indices = over(pointsSP, countriesSP)

  # return the ADMIN names of each country
  indices$ADMIN
}

# Give the entries country names
coords2country(e_data[,c("longitude","latitude")])

# Add the country names to the csv
e_data$country <- coords2country(e_data[,c("longitude","latitude")])

# Filter for Japan and convert to sf object
japan_data <- e_data %>%
  filter(country == "Japan") %>%
  select(time, longitude, latitude, mag, depth) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%
  mutate(t_year = year(as.POSIXct(time)))

# Plot the data
tmap_mode("plot")
j_plot <- tm_shape(japan_data) +
  tm_dots(col="t_year",
          title.col="Year",
          palette="viridis",
          style="cont") +
  add_map_decorations()

j_plot

# Save the plot
tmap_save(j_plot, "./images/japan_earthquakes.png", width=15, height=15, units="cm")
# ================================================================
# KDE plot
library(spatstat)
library(raster)

japan_coords <- st_coordinates(japan_data[,c("geometry")])
#pts <- as.matrix(japan_coords)
window <- owin(xrange = range(japan_coords[,1]), yrange = range(japan_coords[,2]))
p <- ppp(japan_coords[,1], japan_coords[,2], window = window)
kde_1 <- density(p, sigma=bw.ppl)
plot(kde_1)
