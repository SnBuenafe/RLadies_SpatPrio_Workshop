#packages
pacman::p_load(tidyverse, sf, lubridate)

# paths
inputFeat <- file.path("Input", "DATA", "Features")

# Load data
## Shape files
#important packages: sf
dat <- sf::st_read(file.path(inputFeat,"Abyssal_hills.shp"))
dat2 <- sf::st_read(file.path(inputFeat,"Chelonia_mydas.shp"))

### check data using ggplot
ggplot2::ggplot()+
  geom_sf(data = dat2)

## Raster
# important packages: terra, stars, raster (deprecated, don't use anymore, but might see in old code)

### check data using ggplot

## csv

### check data using ggplot

# Bonus: plotting with tmap

# Set crs (normally directly when loading data but here step by step)

# Spatial data wrangling
# important packages: 

# Spatial analysis
## Create a boundary

## Intersections