splnr_plot_Solution <- function(soln, colorVals = c("TRUE" = "#3182bd", "FALSE" = "#c6dbef"),
                                legendTitle = "Planning Units") {
  soln <- soln %>%
    dplyr::select("solution_1") %>%
    dplyr::mutate(solution_1 = as.logical(.data$solution_1)) # Making it logical helps with the plotting
  
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution_1), colour = NA, size = 0.1, show.legend = TRUE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::scale_fill_manual(
      name = legendTitle,
      values = colorVals,
      labels = c("Not Selected", "Selected"),
      aesthetics = c("colour", "fill"),
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = 2,
        order = 1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) + 
    theme_bw()
}

