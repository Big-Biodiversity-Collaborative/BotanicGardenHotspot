# Plot cities and gardens
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-28

require(dplyr)   # data wrangling
require(osmdata) # city boundaries
# require(sf)      # drawing garden polygons
require(ggplot2) # data viz
require(ggpubr)  # multi-panel plot

# Get a polygon for each city
# Add star for garden location and plus symbols for records from GBIF
# Label plots accordingly

gardens <- read.csv(file = "data/gardens.csv")

cities <- unique(gardens$city)

city_plots <- list()

for (city_i in 1:length(cities)) {
  # city_i <- 2
  
  # Need city and state abbreviation. Cludgy
  city_name <- cities[city_i]
  state_abbr <- gardens$state[gardens$city == city_name][1]
  
  # Make a character string we can use with OpenStreetMap data with city and 
  # state abbreviation
  city_state <- paste0(city_name, ", ", state_abbr)
  message(paste0("Creating plot for ", city_state))
  city_poly <- osmdata::getbb(place_name = city_state,
                              format_out = "polygon")
  # Most queries return a list, and we just want the first matrix element; when 
  # a single polygon is returned, it is already a matrix
  if (class(city_poly)[1] == "list") {
    city_poly <- city_poly[[1]]
  }
  
  # Now get GBIF observations for the city
  city_fileslug <- tolower(x = gsub(pattern = ", ",
                                    replacement = "_",
                                    x = city_state))
  city_fileslug <- gsub(pattern = " ",
                        replacement = "_",
                        x = city_fileslug)
  city_obs <- read.csv(file = paste0("data/gbif/", city_fileslug, "-obs.csv"))
  
  # Go ahead and add observations from the gardens
  city_gardens <- gardens[gardens$city == city_name, ]
  for (garden_i in 1:nrow(city_gardens)) {
    garden_name <- tolower(x = gsub(pattern = " ", 
                                    replacement = "_",
                                    x = city_gardens$name[garden_i]))
    garden_obs <- read.csv(file = paste0("data/gbif/", garden_name, "-obs.csv"))
    city_obs <- city_obs %>%
      dplyr::bind_rows(garden_obs)
  }
  # Drop duplicates
  city_obs <- city_obs %>%
    distinct()
  
  # Try using ggplot polygons
  city_df <- data.frame(lon = city_poly[, 1],
                        lat = city_poly[, 2])
  
  # Get dimensions of garden(s), to set plot boundaries
  lon_min <- min(c(city_poly[, 1], city_gardens$lon_min))
  lon_max <- max(c(city_poly[, 1], city_gardens$lon_max))
  lat_min <- min(c(city_poly[, 2], city_gardens$lat_min))
  lat_max <- max(c(city_poly[, 2], city_gardens$lat_max))
  
  city_plot <- ggplot(data = city_df, mapping = aes(x = lon, y = lat)) +
    geom_polygon(fill = "white", color = "black") +
    xlim(c(lon_min, lon_max)) + 
    ylim(c(lat_min, lat_max)) + 
    labs(title = city_state) + 
    ylab("Latitude") +
    xlab("Longitude") + 
    # theme_minimal()
    theme_void()
  # Add observations to plot
  city_plot <- city_plot +
    geom_point(data = city_obs, mapping = aes(x = decimalLongitude,
                                              y = decimalLatitude),
               shape = 3)
  
  # Add a triangle for each garden
  for (garden_i in 1:nrow(city_gardens)) {
    lon <- (city_gardens$lon_min[garden_i] + city_gardens$lon_max[garden_i])/2
    lat <- (city_gardens$lat_min[garden_i] + city_gardens$lat_max[garden_i])/2
    garden_df <- data.frame(lon = lon,
                            lat = lat,
                            garden = city_gardens$name[garden_i])
    city_plot <- city_plot +
      geom_point(data = garden_df,
                 mapping = aes(x = lon, y = lat),
                 shape = 17, 
                 color = "#2ca25f",
                 size = 5)
  }
  city_plots[[city_name]] <- city_plot
}

ggpubr::ggarrange(city_plots[[1]], 
                  city_plots[[2]], 
                  city_plots[[3]], 
                  city_plots[[4]],
                  city_plots[[5]],
                  city_plots[[7]],
                  ncol = 3, nrow = 2)

#### OLD BELOW HERE
# Convert the polygon to a simple feature for ease of filtering points
city_sf <- sf::st_polygon(x = list(city_poly), dim = "XY")

# Get dimensions of garden(s), to set plot boundaries
city_gardens <- gardens[gardens$city == city_name, ]

lon_min <- min(c(city_poly[, 1], city_gardens$lon_min))
lon_max <- max(c(city_poly[, 1], city_gardens$lon_max))
lat_min <- min(c(city_poly[, 2], city_gardens$lat_min))
lat_max <- max(c(city_poly[, 2], city_gardens$lat_max))

plot(city_sf, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))

# add each garden to the plot
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
for (garden_i in 1:nrow(city_gardens)) {
  # topleft, topright, bottomright, bottomleft
  garden_mat <- matrix(data = c(city_gardens$lon_min[garden_i], city_gardens$lat_max[garden_i],
                                city_gardens$lon_max[garden_i], city_gardens$lat_max[garden_i],
                                city_gardens$lon_max[garden_i], city_gardens$lat_min[garden_i],
                                city_gardens$lon_min[garden_i], city_gardens$lat_min[garden_i],
                                city_gardens$lon_min[garden_i], city_gardens$lat_max[garden_i]),
                       ncol = 2, byrow = TRUE)
  garden_sf <- sf::st_polygon(x = list(garden_mat), dim = "XY")
  plot(garden_sf, add = TRUE, 
       bg = "#2ca25f", 
       col =  "#2ca25f",
       lwd = 0.1)
}


