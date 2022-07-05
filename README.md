# Botanic Garden Hotspot

Investigation of botanical gardens as hotspots for urban biodiversity

Draft manuscript is on [Google Drive]( https://docs.google.com/document/d/16KyHO89o7zbOIxLlxg6Cm4S_jw1adECCRSkMj3ykA7g/edit?usp=sharing) 
(permission required).

## Summary

The project uses community science observations of butterflies to compare 
biodiversity in botanical gardens to that of surrounding urban areas in the 
southwestern United States of America. It uses two measures, species richness 
and Shannon's Index of diversity, along with bootstrapping replicates to 
illustrate how relatively small botanical gardens serve as biodiversity 
hotspots for urban areas.

## Dependencies

The data downloads, analyses, and visualization are written in R and use the 
following additional R packages:

+ dplyr: general data wrangling
+ ggplot2: data visualization
+ ggpubr: multi-panel plots
+ osmdata: querying [OpenStreetMaps](https://www.openstreetmap.org) for city 
boundaries
+ rgbif: downloading data from [GBIF](https://gbif.org)
+ sf: point filtering of city observations (with osmdata)
+ stringr: text formatting in data visualization
+ tidyr: general data wrangling

## Project organization

R scripts for data downloads, analysis, and visualization are included in the 
top-level folder. Additional folders and contents are:

+ data: information about gardens (coordinate bounding boxes, cities) and 
rainfall for corresponding cities
    + gbif: community science records for gardens and cities downloaded from 
    GBIF
+ functions: utility functions; at last check just one function to help with 
pageination during GBIF downloads
+ output: plots and output tables; most files are not under version control.
