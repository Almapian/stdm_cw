library(dplyr)

folder_path <- "../../../coursework"

# Get all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSVs
merged_data <- bind_rows(lapply(csv_files, read.csv))

# Check the result
nrow(merged_data)
head(merged_data)


coords2country <- function(points) {
  countriesSP <- getMap(resolution='low')
  pointsSP <- SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))
  indices <- over(pointsSP, countriesSP)
  indices$ADMIN
}

merged_data$country <- coords2country(merged_data[, c("longitude", "latitude")])

# Define Japan's broader maritime bounding box
jp_lat_min <- 24
jp_lat_max <- 46
jp_lon_min <- 122
jp_lon_max <- 148

jp_d <- merged_data %>%
  filter(
    country == "Japan" |
      (is.na(country) &
         latitude  >= jp_lat_min & latitude  <= jp_lat_max &
         longitude >= jp_lon_min & longitude <= jp_lon_max)
  ) %>%
  select(time, longitude, latitude, mag, depth) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(t_year = year(as.POSIXct(time)))

st_write(jp_d, dsn = "japan_earthquakes_1960_2025.gpkg", driver = "GPKG", delete_dsn = TRUE)

