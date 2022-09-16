# Species list table for manuscript
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-09-16

library(dplyr)
library(tidyr)

# A table of all species and botanic gardens in which they were found

gardens <- read.csv(file = "data/gardens.csv")

# Iterate over all gardens included in the study
all_counts <- NULL
for (garden in gardens$name) {
  # Make the compute-readable version of the garden name
  nice_name <- tolower(x = gsub(x = garden,
                                pattern = " ",
                                replacement = "_"))
  file_name <- paste0("data/gbif/", nice_name, "-obs.csv")
  garden_obs <- read.csv(file_name)
  # Turn off summarize annoyance
  options(dplyr.summarise.inform = FALSE)
  # Count the number of each species for this garden and add a column with the
  # garden name for downstream processing
  spp_counts <- garden_obs %>%
    filter(!is.na(species)) %>% # some rows have no species name
    group_by(family, species) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(garden = garden)
  options(dplyr.summarise.inform = TRUE)
  if (is.null(all_counts)) {
    all_counts <- spp_counts
  } else {
    all_counts <- all_counts %>%
      bind_rows(spp_counts)
  }
}

# Transform to wide, with counts for each garden as separate row
out_table <- all_counts %>%
  pivot_wider(id_cols = c(family, species), 
              names_from = garden, 
              values_from = count) %>%
  arrange(family, species)

# Output to file
write.csv(x = out_table,
          file = "data/species-list.csv",
          row.names = FALSE)
