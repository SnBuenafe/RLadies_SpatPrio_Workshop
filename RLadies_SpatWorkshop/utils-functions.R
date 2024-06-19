# original code from Jason Everett
# adapted by Sandra Neubert

get_hexagonalPUs <- function(Bndry,
                             InnerB,
                             CellArea = 1000,
                             inverse = FALSE) {

  diameter <- 2 * sqrt((CellArea * 1e6) / ((3 * sqrt(3) / 2))) * sqrt(3) / 2 # Diameter in m's

  # First create planning units for the whole region
  PUs <- sf::st_make_grid(Bndry,
                          square = FALSE,
                          cellsize = c(diameter, diameter),
                          what = "polygons") %>%
    sf::st_sf()
  
  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- sf::st_centroid(PUs) %>%
    sf::st_intersects(Bndry) %>%
    lengths() > 0 # Get logical vector instead of sparse geometry binary
  
  PUs <- PUs[logi_Reg, ] # Get TRUE
  
  # Second, get all the pu's with < 50 % area on land (approximated from the centroid)
  logi_Ocean <- sf::st_centroid(PUs) %>%
    sf::st_intersects(InnerB) %>%
    lengths() > 0 # Get logical vector instead of sparse geometry binary
  
  if (inverse == FALSE) {
    PUs <- PUs[!logi_Ocean, ] # Get FALSE
  } else {
    PUs <- PUs[logi_Ocean == TRUE, ] # Get TRUE
  }
  
  PUs <- PUs %>%
    dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference
  
  return(PUs)
}
