# Quick check to see how many observations we have
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-22

library(dplyr)

# Load garden data
gardens <- read.csv(file = "data/gardens.csv")

# garden  garden_count  city_count
obs_counts <- data.frame(garden = gardens$name, 
                         garden_count = NA,
                         city_count = NA)

# keep a tally of # observations for each species
species_obs <- NULL

# We only want observations that occurred in or after 2000 
min_year <- 2000

# Three species warranted investigation:
# C. columella - turns out that GBIF considers Strymon istapa as subspecies of 
#                C. columella; all records are fine
# E. aveyrana - used to be in Phyciodes; all records are fine
# P. astylalus - only one observation in Tohono Chul; still questionable
sp_to_check <- c("Callicista columella", "Eresia aveyrana", "Papilio astyalus")
check_obs <- NULL

for (garden_i in 1:nrow(gardens)) {
  # Get file names for garden and corresponding city
  garden_name <- tolower(x = gsub(pattern = " ", 
                                  replacement = "_",
                                  x = gardens$name[garden_i]))
  garden_file <- paste0("data/gbif/", garden_name, "-obs.csv")
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
  garden_obs <- read.csv(file = garden_file)
  city_obs <- read.csv(file = city_file)
  
  # Drop rows missing species name and records that are too old
  garden_obs <- garden_obs %>%
    filter(!is.na(species)) %>%
    filter(year >= min_year)
  city_obs <- city_obs %>%
    filter(!is.na(species)) %>%
    filter(year >= min_year)

  # Stash and questionable ones
  ques <- garden_obs %>%
    bind_rows(city_obs) %>%
    filter(species %in% sp_to_check)
  if (nrow(ques) > 0) {
    if (is.null(check_obs)) {
      check_obs <- ques
    } else {
      check_obs <- check_obs %>%
        bind_rows(ques)
    }
  }
  
  # De-duplicate each data set
  garden_obs <- garden_obs %>%
    distinct(decimalLongitude, decimalLatitude, family, 
             species, year, month, day, .keep_all = TRUE)
  city_obs <- city_obs %>%
    distinct(decimalLongitude, decimalLatitude, family, 
             species, year, month, day, .keep_all = TRUE)

  obs_counts$garden_count[garden_i] <- nrow(garden_obs)
  obs_counts$city_count[garden_i] <- nrow(city_obs)
  
  # Now do counts for species, keeping year for that tally, too
  both_obs <- garden_obs %>%
    bind_rows(city_obs) %>%
    rename(longitude = decimalLongitude,
           latitude = decimalLatitude) %>%
    distinct(longitude, latitude, family, species, 
             year, month, day, .keep_all = TRUE)
  
  if (is.null(species_obs)) {
    species_obs <- both_obs
  } else {
    species_obs <- species_obs %>%
      bind_rows(both_obs)
  }
}

# Output table for observation counts
observation_counts <- obs_counts %>%
  left_join(gardens %>% select(name, city, state),
            by = c("garden" = "name")) %>%
  arrange(state, city)
# observation_counts
write.csv(x = observation_counts,
          file = "output/observation-counts.csv",
          row.names = FALSE)

# Output table for species counts
species_counts <- species_obs %>%
  group_by(family, species) %>%
  summarize(num_obs = n()) %>%
  ungroup()
# nrow(species_counts)
write.csv(x = species_counts,
          file = "output/species-counts.csv",
          row.names = FALSE)

# How many from iNat and eButterfly?
source_counts <- species_obs %>%
  group_by(datasetName) %>%
  summarize(dataset_count = n())
# source_counts