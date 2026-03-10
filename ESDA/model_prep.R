library(sf)
library(dplyr)
library(lubridate)
library(tidyr)

jp_d <- st_read("japan_earthquakes_1980_2025.gpkg")
jp_d$time <- strptime(jp_d$time, format = "%Y-%m-%dT%H:%M:%S") %>% as.POSIXct()

# Extract longitude and latitude back from geometry for convenience
jp_d <- jp_d %>%
  mutate(
    lon = st_coordinates(geom)[, 1],
    lat = st_coordinates(geom)[, 2],
    year_month = floor_date(time, "month")  # round each event down to its month
  )

# Create a complete grid of all year_month and spatial locations
grid_size <- 2 # degrees

# Define Japan's broader maritime bounding box
lat_min <- 24
lat_max <- 46
lon_min <- 122
lon_max <- 148

# Create grid cell IDs for each earthquake
jp_d <- jp_d %>%
  mutate(
    cell_lon = floor(lon / grid_size) * grid_size,  # bottom-left corner of cell
    cell_lat = floor(lat / grid_size) * grid_size,
    cell_id  = paste0(cell_lon, "_", cell_lat)       # unique cell identifier
  )

# Filter to active cells only
min_events <- 10

active_cells <- jp_d %>%
  st_drop_geometry() %>%
  group_by(cell_id) %>%
  summarise(total_events = n()) %>%
  filter(total_events >= min_events) %>%
  pull(cell_id)

cat("Total grid cells with >= ", min_events, " events: ", length(active_cells), "\n")

jp_active <- jp_d %>%
  filter(cell_id %in% active_cells)

# Aggregate to monthly counts per cell
all_months <- seq(
  from = as.POSIXct(floor_date(min(jp_d$time, na.rm = TRUE), "month")),
  to   = as.POSIXct(floor_date(max(jp_d$time, na.rm = TRUE), "month")),
  by   = "month"
)

cell_month_grid <- expand.grid(
  cell_id    = active_cells,
  year_month = all_months,
  stringsAsFactors = FALSE
)

# Aggregate observed events to monthly counts per cell
monthly_counts <- jp_active %>%
  st_drop_geometry() %>%
  group_by(cell_id, year_month) %>%
  summarise(
    event_count = n(),
    mean_mag    = mean(mag,   na.rm = TRUE),
    max_mag     = max(mag,    na.rm = TRUE),
    mean_depth  = mean(depth, na.rm = TRUE),
    .groups = "drop"
  )

# Join to complete grid so zero-event months are included
monthly_data <- cell_month_grid %>%
  left_join(monthly_counts, by = c("cell_id", "year_month")) %>%
  mutate(
    event_count = replace_na(event_count, 0),
    mean_mag    = replace_na(mean_mag,    0),
    max_mag     = replace_na(max_mag,     0),
    mean_depth  = replace_na(mean_depth,  0)
  ) %>%
  arrange(cell_id, year_month)

cat("Monthly panel dimensions: ", nrow(monthly_data), "rows x", ncol(monthly_data), "cols\n")

# Lagged features
monthly_data <- monthly_data %>%
  group_by(cell_id) %>%
  arrange(year_month) %>%
  mutate(
    # Event count lags (1, 3, 6, 12 months prior)
    lag_count_1  = lag(event_count, 1),
    lag_count_3  = lag(event_count, 3),
    lag_count_6  = lag(event_count, 6),
    lag_count_12 = lag(event_count, 12),

    # Rolling mean count over past 3 and 6 months
    roll_mean_3  = zoo::rollmeanr(event_count, k = 3,  fill = NA),
    roll_mean_6  = zoo::rollmeanr(event_count, k = 6,  fill = NA),

    # Magnitude lags
    lag_mag_1    = lag(mean_mag, 1),
    lag_mag_3    = lag(mean_mag, 3),

    # Time features
    month_of_year = month(year_month),  # seasonality signal if any
    year          = year(year_month)
  ) %>%
  ungroup()

# Train/Test split (train on 1980-2019, test on 2020-2025)
split_date <- as.POSIXct("2020-01-01")

train_data <- monthly_data %>% filter(year_month <  split_date)
test_data  <- monthly_data %>% filter(year_month >= split_date)

cat("Training rows: ", nrow(train_data), "\n")
cat("Test rows:     ", nrow(test_data),  "\n")

# Drop rows with NA lag values (first 12 months per cell will have NAs)
train_data <- train_data %>% drop_na(starts_with("lag_"), starts_with("roll_"))
test_data  <- test_data  %>% drop_na(starts_with("lag_"), starts_with("roll_"))

cat("Training rows after dropping NA lags: ", nrow(train_data), "\n")
cat("Test rows after dropping NA lags:     ", nrow(test_data),  "\n")

# Save prepared datasets for modeling
saveRDS(monthly_data, "monthly_grid_data.rds")
saveRDS(train_data,   "train_data.rds")
saveRDS(test_data,    "test_data.rds")

cat("Saved: monthly_grid_data.rds, train_data.rds, test_data.rds\n")
