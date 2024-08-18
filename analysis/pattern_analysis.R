# This file contains the reproducible script for the Section ""Pattern in Belief about
# the Number of Remaining Faults" in the manuscript

# Load the package
library(scerrModel)
library(ggplot2)
library(patchwork)

# Goal: Attempt to Reproduce Pattern in Belief about the Number of Remaining Faults that has been Reported by F. Melinscak
# Simulate ten studies that have very different parameters and look for pattern
# For this parameters have been chosen to make multiple search rounds more probable based on the previous findings

# Simulation
agents <- create_agents(
  agent_id = paste0("A", seq(1, 10)),
  subj_effect_mu = seq(0.4, 1.4, length.out = 10),
  subj_effect_sigma = seq(0.1, 1.2, length.out = 10),
  subj_prob_fault_alpha = rep(2, 10),
  subj_prob_fault_beta = rep(20, 10),
  subj_error_size_mu = 0,
  subj_error_size_sigma = seq(5, 1, length.out = 10)
)

studies <- create_studies(
  study_id = paste0("S", 1:10),
  agent_id = paste0("A", 1:10),
  N = 200,
  resources = 100,
  cost = 0.03,
  benefit = 0.60,
  obj_effect_mu = seq(0.7, 1.1, length.out = 10),
  obj_effect_sigma = seq(0.1, 1.2, length.out = 10),
  obj_prob_fault = seq(0.2, 0.6, length.out = 10),
  obj_error_size_mu = 0,
  obj_error_size_sigma = seq(5, 1, length.out = 10)
)

lit <- simulate_literature(agents = agents, studies = studies, seed = 082024)
result <- simulation_summary(lit)

# Filter out studies that did not reach the 200th search round
stud_indices <- result$study_id[result$n_rounds >= 200]

# Generate Plots
ggs <- vector(mode = "list", length = length(stud_indices))
for (i in seq_along(stud_indices)) {
  study_id <- stud_indices[[i]]
  ggs[[i]] <- plot_distributions_heatmap(lit, study_id = study_id, type = "after_fault_ind") +
    scale_x_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 200)) +
    scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 200)) +
    labs(title = paste("Study", study_id))
}

# Build panel
combined_plot <- ggs[[2]] + ggs[[3]] + ggs[[4]] + ggs[[5]] +
  ggs[[6]] + ggs[[7]] + ggs[[1]] +
  plot_layout(ncol = 2, guides = "collect")
combined_plot

# Save as file
ggsave("analysis/images/sens_large_n_pattern.png", combined_plot, width = 8, height = 8, dpi = 1000)
