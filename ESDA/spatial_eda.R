# Import libraries
library(tidyverse)
library(stars)
library(lubridate)
library(tmap)
library(conflicted)
library(spatstat)
library(raster)
source("./utils/mapper.R")

# Use years from 1975 to 2025 for more consistent data
jp_d <- st_read("japan_earthquakes_1980_2025.gpkg")
jp_d$time <- strptime(jp_d$time, format="%Y-%m-%dT%H:%M:%S") %>% as.POSIXct()

# KDE
japan_coords <- st_coordinates(jp_d[,c("geom")])
#pts <- as.matrix(japan_coords)
window <- owin(xrange = range(japan_coords[,1]), yrange = range(japan_coords[,2]))
p <- ppp(japan_coords[,1], japan_coords[,2], window = window)
kde_1 <- density(p, sigma=bw.diggle, dimyx = 512)
kde_1[kde_1<1e-7] <- NA
plot(kde_1)

dens_stars <- st_as_stars(kde_1)
st_crs(dens_stars) <- 4326

# Plotting
tmap_mode("plot")
map_kde <- tm_shape(dens_stars) +
  tm_raster(palette = "YlOrRd",
            title = "Density",
            style = "cont",
            alpha = 0.7) +
  tm_layout(main.title="Kernel Density Estimation of Earthquakes in Japan (1980-2025)",
            main.title.size=1) +
  add_map_decorations()

tmap_arrange(map_kde)

tmap_save(map_kde, "./images/japan_earthquake_kde.png", width=15, height=15, units="cm")
