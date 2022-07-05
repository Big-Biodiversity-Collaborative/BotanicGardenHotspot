# Permutation tests looking at diversity and richness of botanic gardens
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-17

# Libraries
require(dplyr)   # data wrangling
require(tidyr)   # moar data wrangling
require(ggplot2) # data viz
require(stringr) # text formatting for plots

# Load garden data
gardens <- read.csv(file = "data/gardens.csv")

# We only want observations that occurred in or after 2000 
min_year <- 2000

# Drop singletons?
drop_single <- FALSE

nreps <- 1000
min_obs <- 40 # TODO: Why 40? 

perm_tests <- list()
for (garden_i in 1:nrow(gardens)) {
  # Get file names for garden and corresponding city
  garden_name <- tolower(x = gsub(pattern = " ", 
                                  replacement = "_",
                                  x = gardens$name[garden_i]))
  garden_file <- paste0("data/gbif/", garden_name, "-obs.csv")
  garden_obs <- read.csv(file = garden_file)
  # Keep only those records with species name and year >= min_year
  garden_obs <- garden_obs %>%
    filter(!is.na(species)) %>%
    filter(year >= min_year)

  # Drop duplicates
  garden_obs <- garden_obs %>%
    distinct(decimalLongitude, decimalLatitude, family, 
             species, year, month, day, .keep_all = TRUE)
  
  # Drop singletons, if appropriate
  if (drop_single) {
    garden_multiples <- garden_obs %>%
      group_by(species) %>%
      summarize(num_obs = n()) %>%
      filter(num_obs > 1)
    garden_obs <- garden_obs %>%
      filter(species %in% garden_multiples$species)
  }
    
  # Only proceed if number of garden observations is large enough
  if (nrow(garden_obs) >= min_obs) {
    # RICHNESS for this garden
    garden_richness <- length(unique(garden_obs$species))
    
    # DIVERSITY for this garden (Shannon's Index, H)
    garden_diversity <- garden_obs %>%
      group_by(species) %>%
      summarize(abun = n()) %>%
      ungroup() %>%
      mutate(total_abun = sum(abun)) %>%
      mutate(p = abun/total_abun) %>%
      mutate(plogp = -1 * (p * log(p))) %>%
      select(plogp) %>%
      sum()
    
    city_state <- paste(gardens$city[garden_i], 
                        gardens$state[garden_i], 
                        sep = ", ")
    message(paste0("Starting permutation test for ", gardens$name[garden_i]))
    city_name <- tolower(x = gsub(pattern = ", ",
                                  replacement = "_",
                                  x = city_state))
    city_name <- gsub(pattern = " ",
                      replacement = "_",
                      x = city_name)
    city_file <- paste0("data/gbif/", city_name, "-obs.csv")
    
    # Read in data
    city_obs <- read.csv(file = city_file)

    # Keep only those records with species name and year >= min_year
    city_obs <- city_obs %>%
      filter(!is.na(species)) %>%
      filter(year >= min_year)

    # Drop duplicates
    city_obs <- city_obs %>%
      distinct(decimalLongitude, decimalLatitude, family, 
               species, year, month, day, .keep_all = TRUE)
    
    # Drop singletons, if appropriate
    if (drop_single) {
      city_multiples <- city_obs %>%
        group_by(species) %>%
        summarize(num_obs = n()) %>%
        filter(num_obs > 1)
      city_obs <- city_obs %>%
        filter(species %in% city_multiples$species)
    }
    
    # Randomly sampling points from the city will end up in lots and lots of 
    # zeros; make sampling a little smarter - pick random observations from 
    # the city data set, use that as a rectangle centroid for sample
    
    # Exclude the garden from this sampling effort, as there will 
    # likely be high density of points in this area and will just end up 
    # replicating sampling in the garden, which is not what we want to do; for 
    # now will just remove any points from city_obs that also occur in 
    # garden_obs
    city_obs <- city_obs %>%
      dplyr::filter(!(gbifID %in% garden_obs$gbifID))
    
    # Also, drop duplicate lat/lon coordinates before selecting points, 
    # otherwise highly sampled areas will be over-represented in our sampling
    city_sample_points <- city_obs %>%
      distinct(decimalLongitude, decimalLatitude) %>%
      slice_sample(n = nreps, replace = TRUE)
    
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
    # Don't want to sample sampling deserts, so use a loop to continue sampling
    # until there is at least one point in each rectangle
    
    # Use garden dimensions to dictate bounding box size for sampling within 
    # the city
    lat_dim <- gardens$lat_max[garden_i] - gardens$lat_min[garden_i]
    lon_dim <- gardens$lon_max[garden_i] - gardens$lon_min[garden_i]
    
    # The data frame to hold box coordinates and resulting richness and 
    # diversity
    city_samples <- data.frame(min_lon = city_sample_points$decimalLongitude - lon_dim/2,
                               max_lon = city_sample_points$decimalLongitude + lon_dim/2,
                               min_lat = city_sample_points$decimalLatitude - lat_dim/2,
                               max_lat = city_sample_points$decimalLatitude + lat_dim/2,
                               richness = NA,
                               diversity = NA)

    message(paste0("Running ", nreps, " reps for ", gardens$name[garden_i]))
    for (rep_i in 1:nreps) {
      # Use the rectangle to select points from the city_obs that fall within 
      # the current rectangle
      one_sample <- city_samples[rep_i, ]
      city_sample <- city_obs %>%
        filter(decimalLongitude <= one_sample$max_lon[1],
               decimalLongitude >= one_sample$min_lon[1],
               decimalLatitude <= one_sample$max_lat[1],
               decimalLatitude >= one_sample$min_lat[1])
      
      # RICHNESS for this sample
      sample_richness <- 0
      if (nrow(city_sample) > 0) {
        sample_richness <- length(unique(city_sample$species))
        # message(paste0("Richness ", sample_richness, " in rep ", rep_i))
      } else {
        message("Richness of zero encountered during replicate")
      }
      
      city_samples$richness[rep_i] <- sample_richness
      
      # DIVERSITY for this sample (Shannon's Index, H)
      sample_diversity <- city_sample %>%
        group_by(species) %>%
        summarize(abun = n()) %>%
        ungroup() %>%
        mutate(total_abun = sum(abun)) %>%
        mutate(p = abun/total_abun) %>%
        mutate(plogp = -1 * (p * log(p))) %>%
        select(plogp) %>%
        sum()
      
      city_samples$diversity[rep_i] <- sample_diversity
    }
    
    # Find the probability of a garden richness value this large or larger by 
    # using the ecdf function (note ecdf itself returns a function, to which 
    # we immediately pass the garden richness)
    richness_quantile <- ecdf(x = city_samples$richness)(garden_richness)
    diversity_quantile <- ecdf(x = city_samples$diversity)(garden_diversity)

    perm_tests[[garden_name]] <- list(sample_values = city_samples,
                                      garden_richness = garden_richness,
                                      garden_diversity = garden_diversity,
                                      # garden_prob = garden_prob,
                                      # upper_95 = upper_95,
                                      richness_quantile = richness_quantile,
                                      diversity_quantile = diversity_quantile)
    
    # Permutation t-tests, where we bootstrap *both* garden and city 
    # observations, creating a random sample of 20% of the garden observations 
    # to test if city richness and diversity are higher than garden richness 
    # and diversity
    # Restricting to only those gardens with >= 100 observations.
    if (nrow(garden_obs) >= 100) {
      message(paste0("Running permutation t-test for ", gardens$name[garden_i]))
      sample_size <- ceiling(x = 0.2 * nrow(garden_obs))
      
      richness_mat <- matrix(data = NA, nrow = nreps, ncol = 2)
      diversity_mat <- matrix(data = NA, nrow = nreps, ncol = 2)
      for (rep_i in 1:nreps) {      
        garden_sample <- garden_obs %>%
          slice_sample(n = sample_size)
        
        city_sample <- city_obs %>%
          slice_sample(n = sample_size)
        
        # Garden richness in column 1, city richness in column 2
        richness_mat[rep_i, 1] <- length(unique(garden_sample$species))
        richness_mat[rep_i, 2] <- length(unique(city_sample$species))
        
        # Garden diversity in column 1, city diversity in column 2
        diversity_mat[rep_i, 1] <- garden_sample %>%
          group_by(species) %>%
          summarize(abun = n()) %>%
          ungroup() %>%
          mutate(total_abun = sum(abun)) %>%
          mutate(p = abun/total_abun) %>%
          mutate(plogp = -1 * (p * log(p))) %>%
          select(plogp) %>%
          sum()
        
        diversity_mat[rep_i, 2] <- city_sample %>%
          group_by(species) %>%
          summarize(abun = n()) %>%
          ungroup() %>%
          mutate(total_abun = sum(abun)) %>%
          mutate(p = abun/total_abun) %>%
          mutate(plogp = -1 * (p * log(p))) %>%
          select(plogp) %>%
          sum()
      }
      # Run t-test on resulting matrix; is garden richness significantly lower 
      # than city?
      richness_t <- t.test(x = richness_mat[, 1], 
                           y = richness_mat[, 2],
                           alternative = "less")
      diversity_t <- t.test(x = diversity_mat[, 1],
                            y = diversity_mat[, 2],
                            alternative = "less")
      
      perm_tests[[garden_name]][["richness_t"]] <- richness_t
      perm_tests[[garden_name]][["diversity_t"]] <- diversity_t
    } else {
      message(paste0("Skipping permutation t-test for ", gardens$name[garden_i],
                     ", too few observations"))
    }
    
    
  } else {
    message("Too few observations in ", gardens$name[garden_i], 
            " no permutation tests performed.")
  }
}

# Extract values from the list of permutations, putting them in one data frame 
# for ease of plotting. The garden column gets lots of mutates to make plot 
# a little nicer

perm_test_file <- "output/perm-test-results.rds"
saveRDS(object = perm_tests,
        file = perm_test_file)
if (file.exists(perm_test_file)) {
  perm_tests <- readRDS(file = perm_test_file)
} else {
  message("No perm test results file on disk")
}
sample_values_list <- lapply(X = perm_tests, FUN = "[[", "sample_values")
sample_values <- dplyr::bind_rows(sample_values_list, .id = "garden")
sample_values <- sample_values %>%
  rename(garden_name = garden) %>%
  mutate(garden_print = gsub(pattern = "_",
                       replacement = " ",
                       x = garden_name)) %>%
  mutate(garden_print = stringr::str_to_title(garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Abq",
                       replacement = "ABQ",
                       x = garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Botanic Garden", # To fit on plot
                       replacement = "\nBotanic Garden",
                       x = garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Botanical Garden",
                       replacement = "\nBotanical Garden",
                       x = garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Desert Garden",
                       replacement = "\nDesert Garden",
                       x = garden_print))

# Pull out values for each garden; similar mutations in garden name for plots
garden_richness <- unlist(lapply(X = perm_tests, 
                                 FUN = "[[", "garden_richness"))
garden_diversity <- unlist(lapply(X = perm_tests, 
                                  FUN = "[[", "garden_diversity"))
richness_quantiles <- unlist(lapply(X = perm_tests, 
                                    FUN = "[[", "richness_quantile"))
diversity_quantiles <- unlist(lapply(X = perm_tests, 
                                     FUN = "[[", "diversity_quantile"))
garden_values <- data.frame(garden = names(garden_richness),
                            richness = garden_richness,
                            richness_quantile = richness_quantiles,
                            diversity = garden_diversity,
                            diversity_quantile = diversity_quantiles)
rownames(garden_values) <- NULL

# Updates to garden names for printing purposes
garden_values <- garden_values  %>%
  rename(garden_name = garden) %>%
  mutate(garden_print = gsub(pattern = "_",
                             replacement = " ",
                             x = garden_name)) %>%
  mutate(garden_print = stringr::str_to_title(garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Abq",
                             replacement = "ABQ",
                             x = garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Botanic Garden", # To fit on plot
                             replacement = "\nBotanic Garden",
                             x = garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Botanical Garden",
                             replacement = "\nBotanical Garden",
                             x = garden_print)) %>%
  mutate(garden_print = gsub(pattern = "Desert Garden",
                             replacement = "\nDesert Garden",
                             x = garden_print))

richness_plot <- ggplot(data = sample_values, mapping = aes(x = garden_print, 
                                                            y = richness,
                                                            fill = garden_print)) +
  geom_violin() +
  # geom_boxplot() +
  ylab("Species Richness") +
  xlab(element_blank()) +
  scale_fill_discrete(type = rainbow(length(unique(sample_values$garden_print)))) +
  geom_point(data = garden_values, 
             mapping = aes(x = garden_print, y = richness),
             color = "black", shape = 17, size = 4.5) +
  theme_bw() +
  theme(legend.position = "none")
richness_plot
ggsave(filename = "output/Richness-plot.png",
       plot = richness_plot)

# Diversity
diversity_plot <- ggplot(data = sample_values, mapping = aes(x = garden_print, 
                                                             y = diversity,
                                                             fill = garden_print)) +
  geom_violin() + # Ahem
  # geom_boxplot() +
  ylab("Shannon's Index") +
  xlab(element_blank()) +
  scale_fill_discrete(type = rainbow(length(unique(sample_values$garden_print)))) +
  geom_point(data = garden_values, 
             mapping = aes(x = garden_print, y = diversity),
             color = "black", shape = 17, size = 4.5) +
  theme_bw() +
  theme(legend.position = "none")
diversity_plot
ggsave(filename = "output/Diversity-plot.png",
       plot = diversity_plot)

# And some final cleaning up before writing values to file
garden_values %>%
  select(-garden_print) %>%
  mutate(diversity = round(diversity, digits = 2)) %>%
  rename(garden = garden_name) %>%
  mutate(garden = gsub(pattern = "_",
                             replacement = " ",
                             x = garden)) %>%
  mutate(garden = stringr::str_to_title(garden)) %>%
  mutate(garden = gsub(pattern = "Abq",
                             replacement = "ABQ",
                             x = garden)) %>%
  write.csv(file = "output/perm-tests.csv",
            row.names = FALSE)
  
# Pull out values for those permutation t-tests
richness_t_list <- lapply(X = perm_tests, FUN = "[[", "richness_t")
diversity_t_list <- lapply(X = perm_tests, FUN = "[[", "diversity_t")
