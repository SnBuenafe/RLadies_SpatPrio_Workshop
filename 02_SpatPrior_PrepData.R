# Workshop Spatial Analysis and Prioritization in R for R Ladies Santa Barbara
# Part 2: Spatial Prioritization - Preparing input data
# 02/07/2024
# Sandra Neubert (s.neubert@uq.edu.au) and Tin Buenafe (k.buenafe@uq.edu.au)

# Preliminaries -----------------------------------------------------------

# Load packages
pacman::p_load(tidyverse, sf, terra, stars, rnaturalearth, mregions, tmap, prioritizr)

# Define CRS ##also give ESRI example
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
#or 
cCRS <- "ESRI:54009"

# Define file paths
inputDat <- file.path("Input", "DATA")

# Load data ---------------------------------------------------------------

# Load Planning Units
PUs <- st_read(file.path(inputDat,  "PUs","Galapagos_Planning_Units.shp")) %>%
  st_transform(cCRS) %>%
  select(-"cost") %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = PUs)

# Create an sf object for all features
features <- readRDS(file.path(inputDat, "Features", "fans.rds")) %>%
  left_join(readRDS(file.path(inputDat, "Features", "plateau.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "ridge.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "seamount.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_hills.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_plains.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_mountains.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "basin.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "blue_footed_booby.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "great_frigatebird.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "green_turtle.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "silky_shark.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "tiger_shark.rds")) %>% st_drop_geometry(), by = "cellID") 

# Add cost
cost <- st_read(file.path(inputDat, "Cost", "cost_surface.shp")) %>%
  st_transform(cCRS) %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = cost, aes(fill = .data$cost))

out_sf <- features %>%
  left_join(cost %>% sf::st_drop_geometry(), by = "cellID") 
