# Calculate city and garden sizes
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-07-18

require(osmdata) # city boundaries
require(sf)      # polygon area calculations
require(dplyr)   # data wrangling at the end

gardens <- read.csv(file = "data/gardens.csv")

# Will just add to this data frame
gardens$garden_area <- NA
gardens$city_area <- NA

# Iterate over all gardens
for (garden_i in 1:nrow(gardens)) {
  # Start with garden polygon; to calculate area, create a simple features 
  # object. Start by creating a polygon of the garden rectangle
  garden_coords <- matrix(data = c(gardens$lon_min[garden_i], gardens$lat_max[garden_i], 
                                 gardens$lon_max[garden_i], gardens$lat_max[garden_i],
                                 gardens$lon_max[garden_i], gardens$lat_min[garden_i],
                                 gardens$lon_min[garden_i], gardens$lat_min[garden_i],
                                 gardens$lon_min[garden_i], gardens$lat_max[garden_i]),
                        nrow = 5, byrow = TRUE)
  garden_st_poly <- sf::st_polygon(x = list(garden_coords), dim = "XY")
  
  # For area, we add the CRS and convert it to a simple feature
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  garden_sf <- sf::st_sfc(garden_st_poly, crs = wgs84)
  
  # Defaults to square meters, so we wrap in set_units to get km^2
  gardens$garden_area[garden_i] <- units::set_units(sf::st_area(garden_sf), 
                                                    "km^2")
  # Now we get the city data
  city_name <- gardens$city[garden_i]
  state_name <- gardens$state[garden_i]
  # Check first, for that case where the city area is already known; i.e. the 
  # second garden in Tucson
  city_area <- gardens$city_area[gardens$city == city_name]
  if (any(!is.na(city_area))) {
    # Have a non-missing city area, so just use that value
    city_area <- city_area[!is.na(city_area)][1] # hack
  } else { # Area not yet recorded, so do the calculations now
    city_string <- paste0(city_name, ", ", state_name)
    message(paste0("Running query for ", city_string, " on OSM."))
    city_poly <- osmdata::getbb(place_name = city_string,
                                format_out = "polygon")
    if (class(city_poly)[1] == "list") {
      city_poly <- city_poly[[1]]
    }
    city_st_poly <- sf::st_polygon(x = list(city_poly), dim = "XY")
    city_sf <- sf::st_sfc(city_st_poly, crs = wgs84)
    city_area <- units::set_units(sf::st_area(city_sf), 
                                  "km^2")
  }
  gardens$city_area[garden_i] <- city_area
}

# Write to file
gardens %>% 
  select(name, city, state, city_area, garden_area) %>%
  mutate(city = paste0(city, ", ", state)) %>%
  select(-state) %>%
  mutate(perc = round((garden_area / city_area) * 100, digits = 4)) %>%
  mutate(city_area = round(city_area, digits = 3),
         garden_area = round(garden_area, digits = 3)) %>%
  write.csv(file = "output/city-stats.csv",
            row.names = FALSE)
