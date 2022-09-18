# Species list table for manuscript
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-09-16

library(dplyr)
library(tidyr)

# A table of all species and botanic gardens in which they were found

gardens <- read.csv(file = "data/gardens.csv")

# Arrange to match order of tables in manuscript (state, city, garden name)
gardens <- gardens %>%
  arrange(state, city, name)

# Iterate over all gardens included in the study
gardens_counts <- NULL
cities_counts <- NULL
completed_cities <- c()
for (garden_i in 1:nrow(gardens)) {
  # Start by doing the counts for this **GARDEN**
  # Make the compute-readable version of the garden name
  garden <- gardens$name[garden_i]
  garden_file <- paste0("data/gbif/", 
                      tolower(x = gsub(x = garden,
                                       pattern = " ",
                                       replacement = "_")), 
                      "-obs.csv")
  garden_obs <- read.csv(garden_file)
  # Turn off summarize annoyance
  options(dplyr.summarise.inform = FALSE)
  # Count the number of each species for this garden and add a column with the
  # garden name for downstream processing
  garden_counts <- garden_obs %>%
    filter(!is.na(species)) %>% # some rows have no species name
    group_by(family, species) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(garden = garden)
  if (is.null(gardens_counts)) {
    gardens_counts <- garden_counts
  } else {
    gardens_counts <- gardens_counts %>%
      bind_rows(garden_counts)
  }
  
  # Next do the count for the city (if it has not already been done)
  city_state <- paste0(gardens$city[garden_i], ", ", gardens$state[garden_i])
  
  if (!(city_state %in% completed_cities)) {
    # Need to get rid of spaces in city names for filenames
    nice_city <- tolower(gsub(x = gardens$city[garden_i],
                              pattern = " ",
                              replacement = "_"))

    # Get the city filename
    city_file <- paste0("data/gbif/", nice_city,
                        "_", tolower(gardens$state[garden_i]),
                        "-obs.csv")
    city_obs <- read.csv(city_file)
    # Count the number of each species for this city and add a column with the
    # city_state name for downstream processing
    city_counts <- city_obs %>%
      filter(!is.na(species)) %>% # some rows have no species name
      group_by(family, species) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      mutate(city_state = city_state)
    options(dplyr.summarise.inform = TRUE)
    if (is.null(cities_counts)) {
      cities_counts <- city_counts
    } else {
      cities_counts <- cities_counts %>%
        bind_rows(city_counts)
    }
    completed_cities <- c(completed_cities, city_state)
  }
  options(dplyr.summarise.inform = TRUE)
}

# Transform to wide, with counts for each garden as separate row
garden_out <- gardens_counts %>%
  pivot_wider(id_cols = c(family, species), 
              names_from = garden, 
              values_from = count) %>%
  arrange(family, species)

# Output to files
write.csv(x = garden_out,
          file = "data/gardens-species-list.csv",
          row.names = FALSE)

# Now do the same for cities
city_out <- cities_counts %>%
  pivot_wider(id_cols = c(family, species), 
              names_from = city_state, 
              values_from = count) %>%
  arrange(family, species)

# Output to files
write.csv(x = city_out,
          file = "data/cities-species-list.csv",
          row.names = FALSE)
