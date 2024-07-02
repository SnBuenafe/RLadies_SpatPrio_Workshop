# Workshop Spatial Analysis and Prioritization in R for R Ladies Santa Barbara
# Part 1: Spatial Analysis
# 02/07/2024
# Sandra Neubert and Tin Buenafe 

#packages
pacman::p_load(tidyverse, sf, terra, stars, rnaturalearth, mregions, tmap, leaflet())

#Define crs 
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# paths
inputDat <- file.path("Input", "DATA")

# Load data
## Shape files
#important packages: sf
chelonia_mydas <- sf::st_read(file.path(inputDat, "shp","Chelonia_mydas.shp"))

### check data using ggplot
ggplot2::ggplot()+
  geom_sf(data = chelonia_mydas)

## Raster
# important packages: terra, stars, raster (deprecated, don't use anymore, but might see in old code)
dwCorals <- rast(file.path("Input", "Extra_Data", "YessonEtAl_Consensus.tif")) 
plot(dwCorals)

dwCorals <- read_stars(file.path("Input", "Extra_Data", "YessonEtAl_Consensus.tif")) 
plot(dwCorals)

## csv #data from Jaime Restrepo 
turtle1 <- read_csv(file.path("Input", "Extra_Data", "turtle_Argos.csv")) %>%
  drop_na(c("Latitude", "Longitude")) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = LatLon) 

### check data using ggplot
ggplot2::ggplot()+
  geom_sf(data = turtle1)

# add land data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(LatLon)

ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "black") +
  geom_sf(data = turtle1) +
  ggplot2::coord_sf(xlim = sf::st_bbox(turtle1)$xlim, ylim = sf::st_bbox(turtle1)$ylim) # crop land data to extent of tracking data

# Bonus: plotting with tmap
## color based on ID
tm_shape(turtle1) + 
  tm_dots(col = "DeployID",
          palette = "Blues", 
          title = "ID #")

# Creating an interactive map
print(object.size(turtle1), units = "Kb") #need to keep data small with leaflet; need to now check the size of our dataset

leaflet(turtle1) %>%
  addTiles() %>%
  addCircleMarkers(radius = 0.1)

## color based on time
turtle1 <- turtle1 %>%
  dplyr::mutate(date = sub(".* ", "", Date),
                time = sub(" .*", "", Date),
                date_time = dmy_hms(paste(date, time)))

turtleTimes <- range(turtle1$date_time)
oranges <- colorNumeric("YlOrRd", domain = turtleTimes)

leaflet(turtle1) %>%
  addTiles() %>%
  addCircleMarkers( radius = 3, 
                    color = 'grey80', 
                    weight = 0.1, 
                    fill = TRUE, 
                    fillOpacity = 0.7, 
                    fillColor = ~oranges(date_time))

# Creating an animation with a custom image
# install EBImage
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("EBImage")
# install ggimage
# install.packages("ggimage")
library(ggimage)
#install.packages("gganimate")
library(gganimate)

## prep data for animation: needs to be dataframe
turtle_anim <- read_csv(file.path("Input", "Extra_Data", "turtle_Argos.csv")) %>%
  drop_na(c("Latitude", "Longitude")) %>%
  dplyr::mutate(date = sub(".* ", "", Date),
                time = sub(" .*", "", Date),
                date_time = dmy_hms(paste(date, time))) %>%
  dplyr::mutate(image = sample(c("Input/turtleCartoon.png")))

## Animated plot of turtle tracks
p_animated <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "black") +  # Base map layer
  geom_sf(data = turtle1, size = 0.07) +  # Data points
  ggplot2::coord_sf(xlim = sf::st_bbox(turtle1)$xlim, ylim = sf::st_bbox(turtle1)$ylim) +
  geom_image(aes(x = Longitude, y = Latitude, image=image), data = turtle_anim, size = 0.06) + # uses the ggimage function geom_image()
  labs(title = 'Time: {frame_time}') +  # Title format with frame time
  transition_time(date_time) + # what time information to use: for us: turtle tracking points
  shadow_mark(exclude_layer = 3) + # previous data points remain on the plot, apart from the one that is in excluded_layer
  theme_bw()  # Remove default ggplot2 theme for clean appearance

## Animate the plot
#p_animated <- animate(p_animated, nframes = 100, duration = 15, fps = 10,height = 16,
#                      width = 8, units = "cm", res = 150)

## Save plot
anim_save("Figures/animated_mapTurtle.gif", animate(p_animated, nframes = 300, duration = 15, fps = 10, 
                                              detail = 10, height = 16,
                                              width = 8, units = "cm", res = 200))

# Spatial data wrangling and spatial analysis
# important packages: dplyr, sf

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
# code adapted from Jason Everett
#define PU settings
Shape <- "Hexagon" # "Shape of PUs
PU_size <- 200 # km2

#get landmass (to exclude from analysis)
landmass <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(cCRS)

#get PUs
diameter <- 2 * sqrt((PU_size * 1e6) / ((3 * sqrt(3) / 2))) * sqrt(3) / 2 # Diameter in m's

# First create a grid again
PUs <- sf::st_make_grid(meowDat,
                        square = FALSE,
                        cellsize = c(diameter, diameter),
                        what = "polygons") %>%
  sf::st_sf() %>%
  st_transform(cCRS)

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

ggsave(file.path("Figures", "gg_PUsLand.png"),  width = 6, height = 8, dpi = 200)
