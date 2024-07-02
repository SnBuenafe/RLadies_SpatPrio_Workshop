# Workshop Spatial Analysis and Prioritization in R for R Ladies Santa Barbara
# Part 2: Spatial Prioritization - Prepating input data
# 02/07/2024
# Sandra Neubert and Tin Buenafe 

#packages
pacman::p_load(tidyverse, sf, terra, stars, rnaturalearth, mregions, tmap, prioritizr)

#Define crs ##also give ESRI example
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

# paths
inputDat <- file.path("Input", "DATA")

# Load Boundary

# Load Planning Units
PUs <- sf::st_read(file.path(inputDat,  "PUs","Galapagos_Planning_Units.shp")) %>%
  st_transform(cCRS) %>%
  dplyr::select(-"cost") %>%
  dplyr::rename(cellID = puid)

ggplot2::ggplot()+
  geom_sf(data = PUs)

# Create an sf object for all features
features <- readRDS(file.path(inputDat, "Features", "fans.rds")) %>%
  left_join(readRDS(file.path(inputDat, "Features", "plateau.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "ridge.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "seamount.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_hills.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_plains.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_mountains.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "basin.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "blue_footed_booby.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "great_frigatebird.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "green_turtle.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "silky_shark.rds")) %>% sf::st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "tiger_shark.rds")) %>% sf::st_drop_geometry(), by = "cellID") 

# Add cost
cost <- sf::st_read(file.path(inputDat, "Cost","cost_surface.shp")) %>%
  st_transform(cCRS) %>%
  dplyr::rename(cellID = puid)

ggplot2::ggplot()+
  geom_sf(data = cost, aes(fill = .data$cost))

out_sf <- features %>%
  left_join(cost %>% sf::st_drop_geometry(), by = "cellID") 
