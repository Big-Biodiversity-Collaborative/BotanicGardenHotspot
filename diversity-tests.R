# Proof of concept of species richness test
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-06-17

# Libraries
require(dplyr)   # data wrangling
require(tidyr)   # moar data wrangling
# require(ggplot2) # data viz
# require(osmdata) # city bounding boxes
# require(sf)      # sampling within cities
require(vegan)   # diversity calculations

# Load garden data
gardens <- read.csv(file = "data/gardens.csv")

nreps <- 100
min_obs <- 50 #TODO: Why 50? 

# How should we test for "high" diversity? 
# We could sample some percentage of the garden observations, then the same 
# number (with replacement?) from the city and do diversity calculations

tohono_obs <- read.csv(file = "data/gbif/tohono_chul-obs.csv")
tohono_obs <- tohono_obs[!is.na(tohono_obs$species), ]
tohono_obs$source <- "Tohono Chul"
tucson_obs <- read.csv(file = "data/gbif/tucson_az-obs.csv")
tucson_obs <- tucson_obs[!is.na(tucson_obs$species), ]
tucson_obs$source <- "Tucson"

all_obs <- tohono_obs %>%
  bind_rows(tucson_obs)
# sample_size <- 0.5 * floor(nrow(tohono_obs))
species_counts <- all_obs %>%
  group_by(source, species) %>%
  summarize(count = n()) %>%
  pivot_wider(id_cols = source, 
              names_from = species, 
              values_from = count) %>%
  ungroup()
# Replace NA values with zeros
species_counts[is.na(species_counts)] <- 0
species_counts[, 1:5]

diversity <- vegan::diversity(x = species_counts[, -1],
                              index = "shannon")


