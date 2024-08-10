devtools::load_all()

ags <- create_agents(agent_id = "Alice",
                     subj_effect_mu = 1.0,
                     subj_effect_sigma = 0.5,
                     subj_prob_fault_alpha = 1,
                     subj_prob_fault_beta = 1,
                     subj_error_size_mu = 0,
                     subj_error_size_sigma = 3.0)

stud <- create_studies(study_id = NULL,
                       agent_id = "Alice",
                       N = 100,
                       resources = 100,
                       cost = 0.1,
                       benefit = seq(0.1, 0.2, by = 0.01),
                       obj_effect_mu = 1.0,
                       obj_effect_sigma = 0.5,
                       obj_prob_fault = 0.5,
                       obj_error_size_mu = 0.0,
                       obj_error_size_sigma = 3.0)

cs <- combine_agents_studies(ags, stud)

lit <- simulate_literature(complete_studies = cs, seed = 1234)

simulation_summary(lit)

# alpha and beta must make sense
# with increasing N should think about decreasing error sizes

# bcr is important parameter; however behavior might vary due to random influence
# what processes could underlie this?

# what are circumstances where agents breaks of before first search round?
# - bcr
# - prob of fault in this round -> influenced by randomness through likelihood of observed effect size (i.e. fault_ind and error sizes, thus through obj. reality)
# - prior belief about number of faults (beta-binomial)

ags <- create_agents(agent_id = "Alice",
                     subj_effect_mu = 1.0,
                     subj_effect_sigma = 0.5,
                     subj_prob_fault_alpha = 6,
                     subj_prob_fault_beta = 4,
                     subj_error_size_mu = 0,
                     subj_error_size_sigma = 3.0)

stud <- create_studies(study_id = NULL,
                       agent_id = "Alice",
                       N = 5,
                       resources = 100,
                       cost = 0.1,
                       benefit = 0.3,
                       obj_effect_mu = 1.0,
                       obj_effect_sigma = 0.5,
                       obj_prob_fault = 0.5,
                       obj_error_size_mu = 0.0,
                       obj_error_size_sigma = 3.0)

cs <- combine_agents_studies(ags, stud)

lit <- simulate_literature(complete_studies = cs, seed = 1234)

simulation_summary(lit)

plot_distributions_heatmap(lit, "1", "after_effect")
plot_distributions_heatmap(lit, "1", "after_fault_ind")
plot_prob_fault_this_round(lit, c("6", "7"))

ags <- create_agents(agent_id = "Alice",
                     subj_effect_mu = 1.0,
                     subj_effect_sigma = 0.5,
                     subj_prob_fault_alpha = 1,
                     subj_prob_fault_beta = 1,
                     subj_error_size_mu = 0,
                     subj_error_size_sigma = 3.0)

stud <- create_studies(study_id = NULL,
                       agent_id = "Alice",
                       N = seq(20, 140, by = 20),
                       resources = 100,
                       cost = 0.1,
                       benefit = seq(0.2, 0.5, by = 0.05),
                       obj_effect_mu = 1.0,
                       obj_effect_sigma = 0.5,
                       obj_prob_fault = 0.5,
                       obj_error_size_mu = 0.0,
                       obj_error_size_sigma = 3.0)

cs <- combine_agents_studies(ags, stud)

lit <- simulate_literature(complete_studies = cs, seed = 1234)
