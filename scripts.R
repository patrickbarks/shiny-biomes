
# libraries
library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)

# read df with long and short biome names
biome_names <- read.csv("biome_names.csv") %>% 
  mutate(biome = factor(biome, levels = biome))

# create temp file and directory for download
tmp <- tempfile()
tmp_dir <- tempdir()

# download and unzip WWF ecoregions file
# see https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
wwf_url <- "https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip?1349272619"
download.file(wwf_url, dest = tmp)
unzip(tmp, exdir = tmp_dir)

# read ecoregions shapefile
ecoregions <- readOGR(dsn = paste0(tmp_dir, "/official" ),
                      layer = "wwf_terr_ecos")

# aggregate by biome
biomes <- ecoregions %>%
  unionSpatialPolygons(ecoregions@data$BIOME)

# transform to spatial df
biomes_spdf <- as(biomes, "SpatialPolygonsDataFrame")
biomes_spdf@data$id <- as.integer(row.names(biomes))
biomes_spdf@data <- left_join(biomes_spdf@data, biome_names, by = "id")

# write to file
save(biomes_spdf, file = "biomes_spdf.RData")

