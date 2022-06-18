# Proof of concept of species richness test
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-17

# Libraries
require(rgbif)   # download observations
require(dplyr)   # data wrangling
require(osmdata) # city boundaries
require(sf)      # point filtering for cities

# Load garden data
gardens <- read.csv(file = "data/gardens.csv")

# For proof of concept, just do Tohono chul
gardens <- gardens[1, ]

# FYI, GBIF considers North American riodinids either lycaenids or, even better
# pierids (see E. ares). WTEF, GBIF?
# taxon_keys <- c("Hesperiidae" = 6953,
#                 "Lycaenidae" = 5473,
#                 "Nymphalidae" = 7017,
#                 "Papilionidae" = 9417,
#                 "Pieridae" = 5481,
#                 "Riodinidae" = 1933999)

# For testing
taxon_keys <- c("Hesperiidae" = 6953,
                "Papilionidae" = 9417,
                "Riodinidae" = 1933999)

for (garden_i in 1:nrow(gardens)) {
  state <- gardens$state[garden_i]
  # Download observations for state (?)
  
  # Count number of observations, as pagination might be necessary
  # Actually performs one search per family
  garden_count_query <- rgbif::occ_search(taxonKey = taxon_keys,
                                    limit = 1,
                                    decimalLatitude = paste0(gardens$lat_min[garden_i], ",",
                                                             gardens$lat_max[garden_i]),
                                    decimalLongitude = paste0(gardens$lon_min[garden_i], ",",
                                                              gardens$lon_max[garden_i]))
  
  # For each family, get count and paginate as necessary
  garden_family_obs <- list()
  for (taxon_i in 1:length(taxon_keys)) {
    taxon_key <- taxon_keys[taxon_i]
    taxon_name <- names(taxon_keys)[taxon_i]
    taxon_count <- garden_count_query[[as.character(taxon_key)]]$meta$count
    message(paste0(taxon_name, ": ", taxon_count))
    if (taxon_count > 0) {
      page <- 1
      start <- 0
      while(start <= taxon_count) {
        message(paste0("Downloading ", start, "-", (start+300), " for ", taxon_name))
        garden_obs <- rgbif::occ_search(taxonKey = taxon_key,
                                        decimalLatitude = paste0(gardens$lat_min[garden_i], ",",
                                                                 gardens$lat_max[garden_i]),
                                        decimalLongitude = paste0(gardens$lon_min[garden_i], ",",
                                                                  gardens$lon_max[garden_i]),
                                        start = start,
                                        limit = 300)
        if (page == 1) {
          garden_family_obs[[taxon_name]] <- garden_obs$data
        } else {
          garden_family_obs[[taxon_name]] <- dplyr::bind_rows(garden_family_obs[[taxon_name]],
                                                              garden_obs$data)
        }
        page <- page + 1
        start <- (page - 1) * 300
      }
    } else {
      garden_family_obs[[taxon_name]] <- NULL
    }
  }
  
  # We now have a list with observations for all families; combine to single 
  # data frame
  garden_obs <- dplyr::bind_rows(garden_family_obs)
  
  # Only want to retain iNaturalist and eButterfly records
  garden_obs <- garden_obs %>%
    dplyr::filter(datasetName %in% c("eButterfly", "iNaturalist research-grade observations"))
  

  
# Filter observations for
# 1 - botanic garden (can use rectangles for easy bounding)
# 2 - city (need to find shape files?)

# Calculate richness for garden

# Permutation tests
# TEST 1
# Sample 50% of total observations from garden, calculate richness
# Sample same number of observations from city, calculate richness
# Calculate delta of those two numbers; is 0 outside the 95% CI?

# TEST 2
# Calculate richness for all garden observations
# Sample same number of observations from city, calculate richness
# Is garden richness above the 95% CI of city richness?
}

# Data visualization
# May want to consider putting richness on a 0 - max scale. That is, make the 
# maximum richness = 1 for each site. So we can visualize multiple sites 
# simultaneously without weird scaling problems.

# Trying to get city boundaries from osmdata
# Returns a polygon, or rather, a list of polygons; first is greater Tucson
# area, next two are subtractions (South Tucson and ... ?)
# The polygons are just matrices...
# Note that some queries do not return a list, but just a single matrix
# (see Flowing Wells, AZ). Will need slightly different syntax for those cases
tucson_poly <- osmdata::getbb(place_name = "Tucson, AZ", 
                              format_out = "polygon")
plot(tucson_poly[[1]], type = "l", ylim = c(32, 32.4))
points(tucson_poly[[2]], col = "red", type = "l")
points(tucson_poly[[3]], col = "blue", type = "l")
points(x = garden_obs$decimalLongitude, y = garden_obs$decimalLatitude)

# Tohono Chul is outside of Tucson, what about other areas?
oro <- osmdata::getbb(place_name = "Oro Valley, AZ",
                      format_out = "polygon")

marana <- osmdata::getbb(place_name = "Marana, AZ",
                               format_out = "polygon")

plot(tucson_poly[[1]], type = "l", ylim = c(32, 32.4))
points(oro[[1]], type = "l", col = "red")
points(marana[[1]], type = "l", col = "blue")
points(x = garden_obs$decimalLongitude, 
       y = garden_obs$decimalLatitude, 
       col = "green")
# Nope

# El Paso has 7 polygons. Like Tucson, it looks like the first one is the only 
# one we need
elpaso_poly <- osmdata::getbb(place_name = "El Paso, TX", 
                              format_out = "polygon")
plot(elpaso_poly[[1]], type = "l")
points(elpaso_poly[[2]], col = "red", type = "l")
points(elpaso_poly[[3]], col = "blue", type = "l")
points(elpaso_poly[[4]], col = "orange", type = "l")
points(elpaso_poly[[5]], col = "green", type = "l")
points(elpaso_poly[[6]], col = "cadetblue", type = "l")
points(elpaso_poly[[7]], col = "darkred", type = "l")

# OK, we can get a polygon for a given city. A possible workflow:
# See sf package manual https://cran.r-project.org/web/packages/sf/sf.pdf
# Get bounding box for city. Use the first polygon and make a sf version of it
# sf::st_multipoint(tucson_poly[[1]], dim = "XY")
tucson_sf <- sf::st_multipoint(x = tucson_poly[[1]], dim = "XY")
tucson_sf <- sf::st_polygon(x = list(tucson_poly[[1]]), dim = "XY")
# Find extremes (lat_min, lat_max, lon_min, lon_max)
# Use the city extremes for gbif query. 
# Take the resulting observations and, after pulling out eButterfly and iNat 
# obs, convert to an sf (via sf::st_point?) 
# https://stackoverflow.com/questions/49181715/how-to-make-a-data-frame-into-a-simple-features-data-frame
# df <- data.frame(place = "London", 
#                  lat = 51.5074, lon = 0.1278,
#                  population = 8500000) # just to add some value that is plotable
# projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# df <- st_as_sf(x = df,                         
#                coords = c("lon", "lat"),
#                crs = projcrs)
test_points <- data.frame(lon = c(-111.0, -110.85, -110.75, -110.9),
                          lat = c(32.2, 32.3, 32.2, 32.18))
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
test_sf <- sf::st_as_sf(x = test_points,
                        coords = c("lon", "lat"),
                        crs = wgs84)
# and use it with sf::st_within; below returns logical vector indicating 
# whether point is within the polygon
points_within <- sf::st_within(x = test_sf, y = tucson_sf) %>% lengths > 0

plot(tucson_sf)
points(x = test_points$lon, y = test_points$lat,
       pch = as.character(1:4), 
       col = "red")
