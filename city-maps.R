# Plot cities and gardens
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-28

require(dplyr)   # data wrangling
require(osmdata) # city boundaries
require(ggplot2) # data viz
require(ggpubr)  # multi-panel plot
require(extrafont) # So we can use Arial in figures
# This installation of Rttf2pt1 is required to avoid No FontName issue
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
# extrafont::fonttable()

# Get a polygon for each city
# Add star for garden location and plus symbols for records from GBIF
# Label plots accordingly

gardens <- read.csv(file = "data/gardens.csv")

cities <- unique(gardens$city)

city_plots <- list()

for (city_i in 1:length(cities)) {
  # city_i <- 1
  
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
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, vjust = 1),
          title = element_text(size = 6),
          text = element_text(family = "ArialMT"))

  # Add observations to plot
  city_plot <- city_plot +
    geom_point(data = city_obs, mapping = aes(x = decimalLongitude,
                                              y = decimalLatitude),
               shape = 3,
               size = 0.8,
               color = "#7fc97f")
  
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
                 shape = 24, 
                 fill = "#fdc086", # #FFFFFF
                 color = "#000000",
                 size = 3,
                 stroke = 0.6)
  }
  city_plots[[city_name]] <- city_plot
}

multi_city <- ggpubr::ggarrange(city_plots[[1]], 
                                city_plots[[2]], 
                                city_plots[[3]], 
                                city_plots[[4]],
                                city_plots[[5]],
                                ncol = 3, nrow = 2)
multi_city
ggsave(filename = "output/City-plot.pdf",
       plot = multi_city,
       width = 5,
       height = 3.33,
       units = "in")
ggsave(filename = "output/City-plot.png",
       plot = multi_city,
       width = 5,
       height = 3.33,
       units = "in")
