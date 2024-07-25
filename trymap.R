# Load necessary libraries
libs <- c(
  "ecmwfr", "tidyverse", "metR",
  "terra", "sf", "giscoR", "classInt", "ggplot2", "gganimate", "lubridate", "hms", "av"
)

installed_libs <- libs %in% rownames(installed.packages())
if(any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = TRUE))

# Define API credentials
my_api <- "326651"
my_key <- "ea0f5bc3-8a68-4987-981d-c08d57bac85b"

# Define time sequence for 1 day with minute intervals
time <- seq(
  from = ymd_hms("2023-07-17 00:00:00"),
  to = ymd_hms("2023-07-17 23:59:00"),
  by = "1 min"  # Correct format for minute intervals
)

minutes <- format(time, "%H:%M:%S")

# Define map boundaries
ymin <- 37.0
xmin <- 39.0
ymax <- 43.0
xmax <- 50.0 

# Prepare API request
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c(
    "10m_u_component_of_wind",
    "10m_v_component_of_wind"
  ),
  year = "2024",
  month = "7",
  day = "17",
  time = minutes,
  area = c(ymax, xmin, ymin, xmax),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "armenia-wind.nc"
)

# Request data from API
ecmwfr::wf_set_key(user = my_api, key = my_key, service = "cds")
ecmwfr::wf_request(request = request, user = my_api, path = getwd())

# Load and process the data
armenia_wind <- terra::rast("armenia-wind.nc")
armenia_wind_df <- as.data.frame(armenia_wind, xy = TRUE, na.rm = TRUE)

# Transform data
wind_data <- armenia_wind_df %>%
  pivot_longer(cols = c(starts_with("u10"), starts_with("v10")), 
               names_to = c("component", "time"), 
               names_sep = "_") %>%
  rename(lon = x, lat = y) %>%
  pivot_wider(names_from = component, values_from = value) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(time = ymd_hms("2024-07-17 00:00:00") + minutes(time * 60))

# Calculate wind speed
wind_data <- wind_data %>%
  mutate(speed = sqrt(u10^2 + v10^2))

# Get European and Asian boundaries
get_europe_sf <- function(){
  europe_sf <- giscoR::gisco_get_countries(region = c("Europe", "Asia"), resolution = "3")
  return(europe_sf)
}

europe_sf <- get_europe_sf()

# Create the animation
p <- ggplot(data = wind_data) +
  geom_sf(data = europe_sf, fill = NA, color = "grey40", size = .25, alpha = .99) +
  geom_segment(aes(
    x = lon, y = lat,
    xend = lon + u10 * 0.1, 
    yend = lat + v10 * 0.1,
    color = speed,
    alpha = speed,
    linewidth = speed  # Use `linewidth` instead of `size`
  )) +
  scale_color_gradientn(
    name = "Wind speed (m/s)",
    colours = hcl.colors(12, "Plasma")
  ) +
  scale_alpha(range = c(.2, 1)) +
  scale_linewidth(range = c(.1, .5)) +
  coord_sf(crs = 4326, xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  guides(
    alpha = "none",
    linewidth = "none",
    color = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = 0,
      nrow = 1
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(.85, 1.01),
    legend.title = element_text(size = 11, color = "white"),
    legend.text = element_text(size = 9, color = "white"),
    plot.title = element_text(size = 16, color = "white", hjust = .1, vjust = -1),
    plot.subtitle = element_text(size = 9, color = "white", hjust = .2, vjust = -1),
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = unit(c(t = 0, r = -3, b = -3, l = -3), "lines")
  ) +
  labs(
    title = "Cyclone Animation - 17 July 2024",
    subtitle = "Source: Climate Change Service, ERA5 hourly data on single levels from 1940 to present",
    x = "",
    y = ""
  ) +
  transition_time(time) +
  ease_aes('linear')

output_dir <- "~/Desktop/ArmeniaWeatherLive/ArmeniaWeatherLive/animation"
output_file <- file.path(output_dir, "armenia_wind_quiver_animation.mp4")

anim_save("armenia_wind_quiver_animation.mp4", animation = animate(p, nframes = length(time), fps = 60, width = 800, height = 600, renderer = av_renderer()))
