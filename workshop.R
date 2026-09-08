devtools::load_all()

# ----------------------- SIMULATION ----------------------- #
agents <- create_agents(
  sbj_prob_alpha = seq(0.5, 5, length.out = 10),
  sbj_prob_beta = seq(5, 0.5, length.out = 10),
  sbj_effect_mu = 0.6,
  sbj_effect_sigma2 = 0.2,
  sbj_error_mu = 0,
  sbj_error_kappa = 1,
  sbj_error_var_beta = 2,
  sbj_error_var_alpha = 3
)

studies <- create_studies(
  N = 200,
  resources = 1000,
  cost = 1,
  benefit = 5,
  obj_prob_fault = 0.1,
  obj_effect_mu = 0.4,
  obj_effect_sigma2 = 0.1,
  obj_error_size_mu = 0,
  obj_error_size_sigma2 = 0.3,
  agent_id = paste0("a", seq(1, 10))
)

res <- simulate_literature(agents = agents, studies = studies, seed = 19032026, use_same_seed = TRUE)
simulation_summary(res)
plot_histogram_final_effect_sizes(res, binwidth = 1)
plot_study_panels(res)
