# Plot cities and gardens
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-28

require(dplyr)   # data wrangling
require(osmdata) # city boundaries
require(sf)      # drawing garden polygons

# Get a polygon for each city
# Add filled rectangle for each garden
# Label plots accordingly

gardens <- read.csv(file = "data/gardens.csv")

cities <- unique(gardens$city)

# TODO: Will eventually iterate over all cities
city_i <- 1

# Need city and state abbreviation. Cludgy
city_name <- cities[city_i]
state_abbr <- gardens$state[gardens$city == city_name][1]

city_state <- paste0(city_name, ", ", state_abbr)

city_poly <- osmdata::getbb(place_name = city_state,
                            format_out = "polygon")
# Most queries return a list, and we just want the first matrix element; when 
# a single polygon is returned, it is already a matrix
if (class(city_poly)[1] == "list") {
  city_poly <- city_poly[[1]]
}

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


