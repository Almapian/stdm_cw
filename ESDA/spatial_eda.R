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


# e_data <- read.csv("./usgs_earthquake_data_2000_2025.csv")
#
#
# # The single argument to this function, points, is a data.frame in which:
# #   - column 1 contains the longitude in degrees
# #   - column 2 contains the latitude in degrees
# coords2country = function(points)
# {
#   countriesSP <- getMap(resolution='low')
#
#   # convert our list of points to a SpatialPoints object
#
#   #setting CRS directly to that from rworldmap
#   pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))
#
#   # use 'over' to get indices of the Polygons object containing each point
#   indices = over(pointsSP, countriesSP)
#
#   # return the ADMIN names of each country
#   indices$ADMIN
# }
#
# # Give the entries country names
# coords2country(e_data[,c("longitude","latitude")])
#
# # Add the country names to the csv
# e_data$country <- coords2country(e_data[,c("longitude","latitude")])
#
# # Filter for Japan and convert to sf object
# japan_data <- e_data %>%
#   filter(country == "Japan") %>%
#   select(time, longitude, latitude, mag, depth) %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%
#   mutate(t_year = year(as.POSIXct(time)))
#
# # Plot the data
# tmap_mode("plot")
# j_plot <- tm_shape(japan_data) +
#   tm_dots(col="t_year",
#           title.col="Year",
#           palette="viridis",
#           style="cont") +
#   add_map_decorations()
#
# j_plot
#
# # Save the plot
# tmap_save(j_plot, "./images/japan_earthquakes.png", width=15, height=15, units="cm")
# ================================================================
# KDE plot
library(spatstat)
library(raster)

japan_coords <- st_coordinates(jp_d[,c("geometry")])
#pts <- as.matrix(japan_coords)
window <- owin(xrange = range(japan_coords[,1]), yrange = range(japan_coords[,2]))
p <- ppp(japan_coords[,1], japan_coords[,2], window = window)
kde_1 <- density(p, sigma=bw.ppl, dimyx = 512)
kde_1[kde_1<1e-7] <- NA
plot(kde_1)

dens_stars <- st_as_stars(kde_1)
st_crs(dens_stars) <- 4326

tmap_mode("plot")
map_kde <- tm_shape(dens_stars) +
  tm_raster(palette = "YlOrRd",
            title = "Density",
            style = "cont",
            alpha = 0.7) +
  tm_layout(main.title="Kernel Density Estimation of Earthquakes in Japan (2000-2025)",
            main.title.size=1) +
  add_map_decorations()

j_plot <- tm_shape(jp_d) +
  tm_dots(col="t_year",
          title.col="Year",
          palette="viridis",
          style="cont",
          title="Year of Earthquake Occurence") +
  tm_layout(main.title="Earthquake Occurences in Japan (2000-2025)",
            main.title.size=1) +
  add_map_decorations()

tmap_arrange(map_kde, j_plot, ncol = 2)

tmap_save(tmap_arrange(map_kde, j_plot, ncol = 2), "./images/japan_earthquake_kde.png", width=30, height=15, units="cm")

#==============================================================================
# Save csv
st_write(japan_data, dsn = "japan_earthquakes_2000_2025.gpkg", layer = "japan_earthquakes", driver = "GPKG", delete_dsn = TRUE)

new_jap <- st_read("japan_earthquakes_2000_2025.gpkg", layer = "japan_earthquakes")
