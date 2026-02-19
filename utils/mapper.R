add_map_decorations <- function()
  {
  tm_basemap("CartoDB.Positron") +
  tm_compass(position = c("left", "top")) +
    tm_scale_bar(position = c("right", "bottom")) +
    # tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
    #         lwd = 0.1,
    #         alpha = 0.5,
    #         labels.inside.frame = FALSE,
    #         crs = 4326) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    labels.size = 0.7,
    lwd = 0.1,
    alpha = 0.5,
    labels.inside.frame = FALSE,
    crs = 4326) +
    tm_layout(
      main.title.size = 1,
      legend.outside = TRUE,
#      legend.position = c("left", "bottom"),
      legend.bg.color = "grey",
      legend.bg.alpha = 0.5,
      legend.frame = TRUE
    )
}
