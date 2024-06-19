#packages
pacman::p_load(tidyverse, sf, rnaturalearth, mregions)

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
#Define crs ##also give ESRI example
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
### Use boundary to create grid
dat_PUs <- st_make_grid(meowDat, cellsize = 20000) %>% #cellsize: opposite edges
  st_sf() %>%
  mutate(cellID = row_number()) # Add a cell ID reference

### Plot grid
gg_PUs <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dat_PUs, colour = "grey80", fill = NA, size = 0.1, show.legend = FALSE)+
  ggplot2::coord_sf(xlim = sf::st_bbox(dat_PUs)$xlim, ylim = sf::st_bbox(dat_PUs)$ylim) +
  ggplot2::labs(subtitle = "Planning Units")

### Using centroids and intersections: creating a grid with hexagonal planning units and exclude land
#define PU settings
Shape <- "Hexagon" # "Shape of PUs
PU_size <- 200 # km2

#get landmass (to exclude from analysis)
landmass <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(cCRS)

#get PUs
diameter <- 2 * sqrt((CellArea * 1e6) / ((3 * sqrt(3) / 2))) * sqrt(3) / 2 # Diameter in m's

# First create a grid again
PUs <- sf::st_make_grid(meowDat,
                        square = FALSE,
                        cellsize = c(diameter, diameter),
                        what = "polygons") %>%
  sf::st_sf()

# Then get all the PUs partially/wholly within the planning region
logi_Reg <- sf::st_centroid(PUs) %>%
  sf::st_intersects(meowDat) %>%
  lengths() > 0 # Get logical vector instead of sparse geometry binary

PUs <- PUs[logi_Reg, ] # Get TRUE

# Second, get all the pu's with < 50 % area on land (approximated from the centroid)
logi_Ocean <- sf::st_centroid(PUs) %>%
  sf::st_intersects(landmass) %>%
  lengths() > 0 # Get logical vector instead of sparse geometry binary

dat_PUs <- PUs[!logi_Ocean, ] %>%
  dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference

### Plot grid
gg_PUsBasic <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dat_PUs, colour = "grey80", fill = NA, size = 0.1, show.legend = FALSE)

gg_PUsLand <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dat_PUs, colour = "grey80", fill = NA, size = 0.1, show.legend = FALSE)+
  geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) + #plot landmass
  coord_sf(xlim = st_bbox(meowDat)$xlim, ylim = st_bbox(meowDat)$ylim) + #crop landmass
  ggplot2::labs(subtitle = "Planning Units") + 
  theme_bw()
