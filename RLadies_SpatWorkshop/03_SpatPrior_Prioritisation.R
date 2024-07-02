# Workshop Spatial Analysis and Prioritization in R for R Ladies Santa Barbara
# Part 2: Spatial Prioritization - Creating and solving the conservation problem
# 02/07/2024
# Sandra Neubert and Tin Buenafe 

#Run prioritisation
source("02_SpatPrior_PrepData.R")
source("utils-functions.R")

#extract feature names
col_name <- features %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()

#create targets object
#same target for all
targets <- rep(0.3, length(col_name))

#higher target for species with tracking data
targets <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 8), rep("Tracking Data", 5))) %>%
  mutate(target = if_else(Category == "Tracking Data", 50 / 100, 5 / 100))

#create a conservation problem
dat_problem <- problem(out_sf,
                       features = col_name,
                       cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  #add_boundary_penalties(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

#solve conservation problem
dat_soln <- dat_problem %>%
  solve.ConservationProblem()

saveRDS(dat_soln, file.path("Output", "Solution1.rds"))

#plot solution with predefined function
(gg_sol <- splnr_plot_Solution(dat_soln))

ggsave(file.path("Figures", "gg_sol.png"),  width = 6, height = 8, dpi = 200)
