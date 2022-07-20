# Species richness and diversity calculations for gardens and cities
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-07-20

require(dplyr)   # data wrangling

# Load garden data
gardens <- read.csv(file = "data/gardens.csv")
min_obs <- 40

# We only want observations that occurred in or after 2000 
min_year <- 2000

gardens$garden_richness <- NA
gardens$garden_diversity <- NA
gardens$city_richness <- NA
gardens$city_diversity <- NA

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
  
  if (nrow(garden_obs) >= min_obs) {
    # RICHNESS for this garden
    gardens$garden_richness[garden_i] <- length(unique(garden_obs$species))
    
    # DIVERSITY for this garden (Shannon's Index, H)
    gardens$garden_diversity[garden_i] <- garden_obs %>%
      group_by(species) %>%
      summarize(abun = n()) %>%
      ungroup() %>%
      mutate(total_abun = sum(abun)) %>%
      mutate(p = abun/total_abun) %>%
      mutate(plogp = -1 * (p * log(p))) %>%
      select(plogp) %>%
      sum()
    
    # Grab city info
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
    
    # Keep only those records with species name and year >= min_year
    city_obs <- city_obs %>%
      filter(!is.na(species)) %>%
      filter(year >= min_year)
    
    # Drop duplicates
    city_obs <- city_obs %>%
      distinct(decimalLongitude, decimalLatitude, family, 
               species, year, month, day, .keep_all = TRUE)
    
    gardens$city_richness[garden_i] <- length(unique(city_obs$species))

    gardens$city_diversity[garden_i] <- city_obs %>%
      group_by(species) %>%
      summarize(abun = n()) %>%
      ungroup() %>%
      mutate(total_abun = sum(abun)) %>%
      mutate(p = abun/total_abun) %>%
      mutate(plogp = -1 * (p * log(p))) %>%
      select(plogp) %>%
      sum()
  }
}
# Write to file
gardens %>%
  select(name, city, state, garden_richness, garden_diversity, 
         city_richness, city_diversity) %>%
  mutate(perc_richness = garden_richness/city_richness,
         perc_diversity = garden_diversity/city_diversity) %>%
  write.csv(file = "output/richness-diversity.csv",
            row.names = FALSE)
