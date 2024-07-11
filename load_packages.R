# Installing and loading all the necessary packages

install.packages("pacman")
library(pacman)

pacman::p_load(BiocManager, remotes)
BiocManager::install("EBImage")
pacman::p_load(tidyverse, sf, terra, stars, rnaturalearth, rnaturalearthdata, mregions, tmap, leaflet, BiocManager, ggimage, gganimate, prioritizr)

# Installing the solvers
# If you are a Windows user, lpsympony might work better
# if (!require(remotes)) install.packages("remotes")
# remotes::install_bioc("lpsymphony")
# Check their website for more details: https://www.bioconductor.org/packages/release/bioc/html/lpsymphony.html
# library(lpsymphony)

# If you are a Mac/Linux, rcbc might work better
# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("dirkschumacher/rcbc")
# Check their README for more details https://github.com/dirkschumacher/rcbc
library(rcbc)

# If they're having problems installing `mregions` from CRAN like I did, use this bit of code
devtools::install_github("ropensci/mregions") # https://github.com/ropensci/mregions