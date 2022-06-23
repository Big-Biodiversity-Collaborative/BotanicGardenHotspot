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
sample_size <- 100 #TODO: Why 100? 

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
  
  # Only proceed if number of garden observations is twice our sample_size
  if (nrow(garden_obs) >= 2 * sample_size) {
    city_state <- paste(gardens$city[garden_i], 
                        gardens$state[garden_i], 
                        sep = ", ")
    city_name <- tolower(x = gsub(pattern = ", ",
                                  replacement = "_",
                                  x = city_state))
    city_name <- gsub(pattern = " ",
                      replacement = "_",
                      x = city_name)
    city_file <- paste0("data/gbif/", city_name, "-obs.csv")
    
    # Read in data
    city_obs <- read.csv(file = city_file)

    # Grab city bounding box, so we can pull out rectangles of similar size to 
    # the garden
    city_poly <- osmdata::getbb(place_name = city_state, format_out = "polygon")
    # Convert the polygon to a simple feature for random sampling of points
    city_sf <- sf::st_polygon(x = list(city_poly), dim = "XY")
    city_sample_points <- sf::st_sample(x = city_sf,
                                        size = nreps)
    
    # Garden dimensions, we use these to dictate bounding box for sampling 
    # within the city
    lat_dim <- gardens$max_lat[garden_i] - gardens$min_lat[garden_i]
    lon_dim <- gardens$max_lon[garden_i] - gardens$min_lon[garden_i]
    
    city_rects <- data.frame(min_lat = city_sample_points - lat_dim/2,
                             max_lat = city_sample_points + lat_dim/2,
                             min_lon = city_sample_points - lon_dim/2,
                             max_lon = city_sample_points + lon_dim/2)
    # Do replicates  
    results <- data.frame(replicate = 1:nreps,
                          garden = NA,
                          city = NA)
    for (rep_i in 1:nreps) {
      garden_sample <- garden_obs %>%
        dplyr::slice_sample(n = sample_size)
      city_sample <- city_obs %>%
        dplyr::slice_sample(n = sample_size)
      # Calculate richness for this sample
      garden_richness <- length(unique(garden_sample$species))
      city_richness <- length(unique(city_sample$species))
      results$garden[rep_i] <- garden_richness
      results$city[rep_i] <- city_richness
    }

    richness_t <- t.test(x = results$garden, 
                         y = results$city, 
                         alternative = "less")
    perm_tests[[garden_name]] <- richness_t
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
