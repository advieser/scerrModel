# This file contains the reproducible example of the simulation that is described
# in Section "Example Simulation of One Literature Data Set".

# Load the package
library(scerrModel)
library(ggplot2)
library(patchwork)

# Create agents
agents <- create_agents(
  agent_id = paste0("A", 1:10),
  subj_effect_mu = seq(0.2, 0.75, length.out = 10),
  subj_effect_sigma = 0.4,
  subj_prob_fault_alpha = 1,
  subj_prob_fault_beta = 1,
  subj_error_size_mu = 0,
  subj_error_size_sigma = 0.5
)
str(head(agents))

# Create multiple studies
studies <- create_studies(
  study_id = paste0("S", 1:10),
  agent_id = paste0("A", 1:10),
  N = 15,
  resources = 100,
  cost = 5,
  benefit = 10,
  obj_effect_mu = 1.0,
  obj_effect_sigma = 0.5,
  obj_prob_fault = 0.4,
  obj_error_size_mu = 0,
  obj_error_size_sigma = 0.6
)
str(head(studies))

# See combined data.frame
cs <- combine_agents_studies(agents, studies)
str(head(cs))


# Simulation
lit <- simulate_literature(complete_studies = cs, seed = 1234L)
str(lit)

# Analysis
result <- simulation_summary(lit)
str(head(result))
result$n_rounds
table(result$reason)
result$n_faults_total
result$n_faults_discovered[result$study_id == "S5"]
result$real_effect_size
result$final_observed_effect_size
result$final_observed_effect_size[result$n_rounds == 1]
result$final_observed_effect_size - result$real_effect_size

# Plotting
p <- plot_summary_panel(lit, cbr = TRUE)
ggsave("analysis/images/ex_plot_summary_panel.png", plot = p, width = 7, height = 8, dpi = 1000)
p <- plot_distributions_heatmap(lit, study_id = "S7", type = "after_effect")
ggsave("analysis/images/ex_plot_distributions_heatmap.png", plot = p, width = 5, height = 4, dpi = 1000)
p <- plot_histogram_final_effect_sizes(lit, binwidth = 0.5)
ggsave("analysis/images/ex_plot_histogram_final_effect_sizes.png", plot = p, width = 4, height = 4, dpi = 1000)
p <- plot_histogram_n_faults(lit, type = "absolute")
ggsave("analysis/images/ex_plot_histogram_n_faults.png", plot = p, width = 4, height = 4, dpi = 1000)
