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
#Define crs
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Spatial data wrangling
# important packages: 

# Spatial analysis
## Create a boundary
GalapEcoregions <- c(20172:20174)

meowDat <- mregions::mr_shp(key = "Ecoregions:ecoregions") %>% 
  dplyr::filter(.data$eco_code %in% GalapEcoregions) %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  sf::st_transform(cCRS) %>%
  dplyr::rename(geometry = x) %>%
  sf::st_set_geometry(., "geometry") 

ggplot2::ggplot()+
  geom_sf(data = meowDat)

## Make grid

## Intersections