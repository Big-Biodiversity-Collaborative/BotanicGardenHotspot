# Download observation data from GBIF
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-22

# Libraries
require(dplyr)   # data wrangling
require(osmdata) # city boundaries
require(sf)      # point filtering for cities
source(file = "functions/query_gbif.R")

# Load garden data
gardens <- read.csv(file = "data/gardens.csv")

# Indicate whether or not to overwrite data files that already exist
overwrite <- FALSE

# Download data for each garden, then for each city

# FYI, GBIF considers North American riodinids either lycaenids or, even better
# pierids (see E. ares). WTEF, GBIF?
taxon_keys <- c("Hesperiidae" = 6953,
                "Lycaenidae" = 5473,
                "Nymphalidae" = 7017,
                "Papilionidae" = 9417,
                "Pieridae" = 5481,
                "Riodinidae" = 1933999)

# For testing
# taxon_keys <- c("Hesperiidae" = 6953,
#                 "Papilionidae" = 9417,
#                 "Riodinidae" = 1933999)

for (garden_i in 1:nrow(gardens)) {
  # Make a nice filename for the data file
  garden_name <- tolower(x = gsub(pattern = " ", 
                                  replacement = "_",
                                  x = gardens$name[garden_i]))
  garden_file <- paste0("data/gbif/", garden_name, "-obs.csv")
  if (overwrite | !file.exists(garden_file)) {
    message("***  Downloading data for ", gardens$name[garden_i])
    # Count number of observations in garden rectangle, as pagination might be 
    # necessary; actually performs one search per taxonKey value (in this case, 
    # one search per family and returns list with one element for each family)
    garden_obs <- query_gbif(taxon_keys = taxon_keys,
                             lon_limits = c(gardens$lon_min[garden_i], 
                                            gardens$lon_max[garden_i]),
                             lat_limits = c(gardens$lat_min[garden_i], 
                                            gardens$lat_max[garden_i]),
                             verbose = TRUE)
    
    write.csv(x = garden_obs,
              file = garden_file,
              row.names = FALSE)
  } else {
    message("Skipping download for ", gardens$name[garden_i], ", already on disk.")
  }
}

# Now download data for each unique city
city_state_string <- paste(gardens$city, gardens$state, sep = ", ")
city_state_string <- unique(city_state_string)

for (city_state in city_state_string) {
  # Make a nice name for filename, have to do it twice to avoid double 
  # underscores
  city_name <- tolower(x = gsub(pattern = ", ",
                                replacement = "_",
                                x = city_state))
  city_name <- gsub(pattern = " ",
                    replacement = "_",
                    x = city_name)
  city_file <- paste0("data/gbif/", city_name, "-obs.csv")
  if (overwrite | !file.exists(city_file)) {
    
    
    message("***  Downloading data for ", city_state)
    city_poly <- osmdata::getbb(place_name = city_state, format_out = "polygon")
    # Most queries return a list, and we just want the first matrix element; when 
    # a single polygon is returned, it is already a matrix
    if (class(city_poly) == "list") {
      city_poly <- city_poly[[1]]
    }
    # First find the maximum containing rectangle coordinates and use those for 
    # the GBIF query
    min_lon <- min(city_poly[, 1])
    max_lon <- max(city_poly[, 1])
    min_lat <- min(city_poly[, 2])
    max_lat <- max(city_poly[, 2])
    
    city_obs <- query_gbif(taxon_keys = taxon_keys,
                           lon_limits = c(min_lon, max_lon),
                           lat_limits = c(min_lat, max_lat),
                           verbose = TRUE)
    
    # Convert the polygon to a simple feature for ease of filtering points
    city_sf <- sf::st_polygon(x = list(city_poly), dim = "XY")
    
    # Now make the city_obs into a simple feature  
    wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    city_obs_sf <- sf::st_as_sf(x = city_obs,
                                coords = c("decimalLongitude", "decimalLatitude"),
                                crs = wgs84)
    
    # and use it with sf::st_within; below returns logical vector indicating 
    # whether point is within the polygon; use that vector to select rows from 
    # city_obs that are within the city polygon
    points_within <- sf::st_within(x = city_obs_sf, y = city_sf) %>% lengths > 0
    city_obs <- city_obs[points_within, ]
    write.csv(x = garden_obs,
              file = city_file,
              row.names = FALSE)
  }
  else {
    message("Skipping download for ", city_state, ", already on disk.")
  }
}
