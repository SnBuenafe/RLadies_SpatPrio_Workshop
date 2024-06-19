#Run prioritisation
#source("1_PrepData_Basic.R")

#extract feature names
col_name <- dat_species_bin %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()

#add a cost layer
out_sf <- dat_species_bin %>%
  #mutate(CostArea = rep(1, 780))

#create targets object
targets <- rep(0.3, length(col_name))

#create a conservation problem
dat_problem <- problem(out_sf,
                       features = col_name,
                       cost_column = "CostArea") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

#solve conservation problem
dat_soln <- dat_problem %>%
  solve.ConservationProblem()

(gg_sol <- splnr_plot_Solution(dat_soln))
