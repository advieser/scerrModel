# This file contains the reproducible script for the Section "Determinants of Decision to Stop" in the manuscript

# Load the package
library(scerrModel)
library(ggplot2)
library(patchwork)

# Question: Under which conditions does the agent perform a first search round?

# Changing Beta Prior and Number of Code Units

# Parameters of the Beta Distribution:
# From a very low expected value with low variance over a medium expected value
# with wide variance to a high expected value with low variance
alphas <- c(5, 5, 5, 5, 10, 15, 20)
betas <- c(20, 15, 10, 5, 5, 5, 5)
# Number of code units
ns <- seq(10, 50, by = 10)
# Allocating the result data.frame
res <- data.frame(
  a = alphas,
  b = betas,
  ns = rep(ns, each = length(alphas)),
  n_equal_one = rep(NA, length(alphas) * length(ns)),
  n_over_one = rep(NA, length(alphas) * length(ns))
)
# Running the Analysis
for (j in seq_along(ns)) {
  for (i in seq_along(alphas)) {

    agents <- create_agents(
      agent_id = paste0("A", 1),
      subj_effect_mu = 0.5,
      subj_effect_sigma = 0.1,
      subj_prob_fault_alpha = alphas[[i]],
      subj_prob_fault_beta = betas[[i]],
      subj_error_size_mu = 0.05,
      subj_error_size_sigma = 0.01
    )

    studies <- create_studies(
      study_id = paste0("S", 1:1000),
      agent_id = paste0("A", 1),
      N = ns[[j]],
      resources = 100,
      cost = 5,
      benefit = 8,
      obj_effect_mu = 0.5,
      obj_effect_sigma = 0.05,
      obj_prob_fault = 0.2,
      obj_error_size_mu = 0.1,
      obj_error_size_sigma = 0.03
    )

    lit <- simulate_literature(agents = agents, studies = studies, seed = 082024)
    result <- simulation_summary(lit)
    above_one <- sum(result$n_rounds > 1)
    res[i+(j-1)*7, c(4, 5)] <- c(1000 - above_one, above_one)
  }
}
res

# Plotting the result
df <- res
# Creating new columns
df$x <- rep(seq(1, 7), 5)

# Create a new column that combines `a` and `b` while preserving the order
df$alpha_beta <- factor(paste(df$a, df$b, sep = "-"), levels = unique(paste(df$a, df$b, sep = "-")))

# Convert `ns` into a factor and order it in descending order
df$ns <- factor(df$ns, levels = sort(unique(df$ns), decreasing = TRUE))
levels(df$ns) <- paste("N =", levels(df$ns))

# Create the bar plot
p <- ggplot(df, aes(x = alpha_beta, y = n_equal_one, fill = alpha_beta)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ ns, ncol = 3) +
  labs(
    x = "Alpha-Beta Combinations",
    y = "Number of Studies with No Search Round",
    fill = "Alpha-Beta"
  ) +
  scale_fill_grey() +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Save as file
ggsave("analysis/images/sen_plot_hyperparameters.png", p, width = 8, height = 5, dpi = 1000)
