# Libraries
library(zoo)
library(dplyr)
library(lubridate)
library(ggplot2)
# Use years from 1975 to 2025 for more consistent data

# Time Series
jp_d <- st_read("japan_earthquakes_1960_2025.gpkg")

jp_d$time <- strptime(jp_d$time, format="%Y-%m-%dT%H:%M:%S") %>% as.POSIXct()
jp_d.zoo <- zoo(jp_d$mag, jp_d$time)
# calculate moving average with a window of 12 months
jp_annual <- jp_d %>%
  group_by(t_year) %>%
  summarise(mean_mag = mean(mag, na.rm = TRUE),
            n_events = n())

jp_d$mag_av <- coredata(jp_annual$mean_mag)[match(jp_d$t_year, jp_annual$t_year)]

sum(is.na(jp_d$time))
#jp_d <- jp_d[!is.na(jp_d$time), ]

sum(is.infinite(jp_d$time))

ggplot(jp_d, aes(x = time)) +
  #geom_line(aes(y = mag, colour = "real")) +
  geom_line(aes(y = mag), size = 0.3, color = "black") +
  geom_line(aes(y = mag_av, colour = "moving")) +
  geom_smooth(aes(y = mag), method = "loess", span = 0.1, color = "blue", linetype = "dashed", se = FALSE) +
  scale_x_datetime(breaks = date_breaks("1 year"), date_labels = "%Y") +
  xlab("Time 1960 - 2025") + ylab("Earthquake Magnitude > 4.5") +
  scale_colour_manual("Lines", values = c("real" = "black", "moving" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Magnitude of Earthquakes in Japan (1960-2025)")

ggsave("./images/japan_earthquake_time_series.png", width=50, height=15, units="cm")
