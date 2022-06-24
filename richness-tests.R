# Proof of concept of species richness test
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-17

# Libraries
require(dplyr)   # data wrangling
require(tidyr)   # moar data wrangling
require(ggplot2) # data viz
require(osmdata) # city bounding boxes
require(sf)      # sampling within cities

# Load garden data
gardens <- read.csv(file = "data/gardens.csv")

nreps <- 100
min_obs <- 100 #TODO: Why 100? 

# For proof of concept, just do Tohono chul
# gardens <- gardens[1, ]

perm_tests <- list()
for (garden_i in 1:nrow(gardens)) {
  # Get file names for garden and corresponding city
  garden_name <- tolower(x = gsub(pattern = " ", 
                                  replacement = "_",
                                  x = gardens$name[garden_i]))
  garden_file <- paste0("data/gbif/", garden_name, "-obs.csv")
  garden_obs <- read.csv(file = garden_file)
  garden_obs <- garden_obs[!is.na(garden_obs$species), ]
  garden_richness <- length(unique(garden_obs$species))
  
  # Only proceed if number of garden observations is large enough
  if (nrow(garden_obs) >= 2 * min_obs) {
    city_state <- paste(gardens$city[garden_i], 
                        gardens$state[garden_i], 
                        sep = ", ")
    message(paste0("Running permutation test for ", city_state))
    city_name <- tolower(x = gsub(pattern = ", ",
                                  replacement = "_",
                                  x = city_state))
    city_name <- gsub(pattern = " ",
                      replacement = "_",
                      x = city_name)
    city_file <- paste0("data/gbif/", city_name, "-obs.csv")
    
    # Read in data
    city_obs <- read.csv(file = city_file)
    city_obs <- city_obs[!is.na(city_obs$species), ]

    # Randomly sampling points from the city will end up in lots and lots of 
    # zeros; make sampling a little smarter - pick random observations from 
    # the city data set, use that as a rectangle centroid for sample
    
    # TODO: Should exclude the garden from this sampling effort, as there will 
    # likely be high density of points in this area and will just end up 
    # replicating sampling in the garden, which is not what we want to do
    city_sample_points <- city_obs %>%
      slice_sample(n = nreps) %>%
      select(decimalLongitude, decimalLatitude)

    # Grab city bounding box, so we can pull out rectangles of similar size to 
    # the garden
    # city_poly <- osmdata::getbb(place_name = city_state, 
    #                             format_out = "polygon")
    # Most queries return a list, and we just want the first matrix element; when 
    # a single polygon is returned, it is already a matrix
    # if (class(city_poly) == "list") {
    #   city_poly <- city_poly[[1]]
    # }
    
    # Convert the polygon to a simple feature for random sampling of points
    # city_sf <- sf::st_polygon(x = list(city_poly), dim = "XY")
    # city_sample_points <- sf::st_coordinates(sf::st_sample(x = city_sf,
    #                                                        size = nreps))
    # Garden dimensions, we use these to dictate bounding box for sampling 
    # within the city
    lat_dim <- gardens$lat_max[garden_i] - gardens$lat_min[garden_i]
    lon_dim <- gardens$lon_max[garden_i] - gardens$lon_min[garden_i]
    
    city_rects <- data.frame(min_lon = city_sample_points$decimalLongitude - lon_dim/2,
                             max_lon = city_sample_points$decimalLongitude + lon_dim/2,
                             min_lat = city_sample_points$decimalLatitude - lat_dim/2,
                             max_lat = city_sample_points$decimalLatitude + lat_dim/2)
    
    # Do replicates  
    # results <- data.frame(replicate = 1:nreps,
    #                       garden = NA,
    #                       city = NA)
    richness_reps <- numeric(nreps)
    for (rep_i in 1:nreps) {
      # garden_sample <- garden_obs %>%
      #   dplyr::slice_sample(n = sample_size)

      # Use the rectangle to select points from the city_obs
      one_rect <- city_rects[rep_i, ]
      city_sample <- city_obs %>%
        filter(decimalLongitude <= one_rect$max_lon[1],
               decimalLongitude >= one_rect$min_lon[1],
               decimalLatitude <= one_rect$max_lat[1],
               decimalLatitude >= one_rect$min_lat[1])
      richness_reps[rep_i] <- 0
      if (nrow(city_sample) > 0) {
        richness_reps[rep_i] <- length(unique(city_sample$species))
      } else {
        message("Richness of zero encountered during replicate")
      }
      
      # city_sample <- city_obs %>%
      #   dplyr::slice_sample(n = sample_size)
      # Calculate richness for this sample
      # garden_richness <- length(unique(garden_sample$species))
      # city_richness <- length(unique(city_sample$species))
      # results$garden[rep_i] <- garden_richness
      # results$city[rep_i] <- city_richness
    }

    perm_tests[[garden_name]] <- list(garden_richness = garden_richness,
                                      city_richness = richness_reps)
  } else {
    message("Too few observations in ", gardens$name[garden_i], 
            " no permutation tests performed.")
  }

  # results_long <- results %>%
  #   tidyr::pivot_longer(cols = c(garden, city),
  #                       names_to = "source",
  #                       values_to = "richness")
  # 
  # 
  # ggplot(data = results_long, mapping = aes(x = source, y = richness)) +
  #   geom_violin() +
  #   theme_bw()
    
  
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

# Quick and dirty look at number of observations for each
