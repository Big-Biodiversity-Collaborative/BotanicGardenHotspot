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
  
  obs_counts$garden_count[garden_i] <- nrow(garden_obs)
  obs_counts$city_count[garden_i] <- nrow(city_obs)
}
obs_counts
