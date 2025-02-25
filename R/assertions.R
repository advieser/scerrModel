# Checks whether vectors have the same length or length of 1
check_lengths_equal_or_one <- function(vecs) {
  assert_list(vecs, types = c("atomic", "null"))
  lens <- lengths(vecs)[lengths(vecs) != 0]  # to drop length(NULL)
  corr_length <- all(max(lens) == lens | lens == 1)
  if(corr_length) {
    corr_length
  } else {
    "All input vectors must either be as long as the longest vector or of length 1"
  }
}
assert_lengths_equal_or_one <- makeAssertionFunction(check_lengths_equal_or_one)

# Checks whether vectors have the maximum length of vectors in vecs
check_max_length <- function(must_max, vecs) {
  assert_list(vecs, types = c("atomic", "null"))
  assert_list(must_max, types = c("atomic", "null"), names = "named")

  lens_vecs <- lengths(vecs)[lengths(vecs) != 0]  # to drop length(NULL)
  lens_must <- lengths(must_max)[lengths(must_max) != 0]

  if(all(lens_must == max(lens_vecs))) {
    TRUE
  } else {
    paste("Input vector(s)", paste(names(must_max), collapse = ", "), "must have the same length as the longest input vector")
  }
}
assert_max_length <- makeAssertionFunction(check_max_length)

# Assert to combine assert_atomic_vector and assert_null
# with changed default values: any.missing = FALSE
assert_atomic_vector_or_null <- function(x, any.missing = FALSE, all.missing = TRUE, len = NULL,
                                         min.len = NULL, max.len = NULL, unique = FALSE, names = NULL) {
  assert(
    check_atomic_vector(x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len,
                        max.len = max.len, unique = unique, names = names),
    check_null(x),
    combine = "or"
  )
}

# Assertions for Agents
assert_agents_properties <- function(agent_id, subj_effect_mu, subj_effect_sigma, subj_prob_fault_alpha, subj_prob_fault_beta,
                                     subj_error_size_mu, subj_error_size_sigma) {
  agent_properties <- list(agent_id, subj_effect_mu, subj_effect_sigma, subj_prob_fault_alpha, subj_prob_fault_beta, subj_error_size_mu, subj_error_size_sigma)
  # We allow inputs of 1 or of equal length for convenience
  assert_lengths_equal_or_one(agent_properties)
  # agent_id should not be shorter than the the other properties, since we don't want it to be recycled
  assert_max_length(list(agent_id = agent_id), agent_properties)
  # Type assertions
  assert(
    check_integerish(agent_id, lower = 1, any.missing = FALSE),
    check_character(agent_id, any.missing = FALSE),
    check_null(agent_id),
    combine = "or"
  )
  assert_numeric(subj_effect_mu, finite = TRUE, any.missing = FALSE)
  assert_numeric(subj_effect_sigma, finite = TRUE, any.missing = FALSE)
  assert_numeric(subj_prob_fault_alpha, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(subj_prob_fault_beta, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(subj_error_size_mu, finite = TRUE, any.missing = FALSE)
  assert_numeric(subj_error_size_sigma, finite = TRUE, any.missing = FALSE)
}

assert_agents <- function(agents) {
  assert_data_frame(agents, ncols = 7)
  expected_names <- c("agent_id", "subj_effect_mu", "subj_effect_sigma", "subj_prob_fault_alpha", "subj_prob_fault_beta",
                      "subj_error_size_mu", "subj_error_size_sigma")
  assert_names(names(agents), type = "named", permutation.of = expected_names)
  assert_agents_properties(agent_id = agents[["agent_id"]],
                           subj_effect_mu = agents[["subj_effect_mu"]],
                           subj_effect_sigma = agents[["subj_effect_sigma"]],
                           subj_prob_fault_alpha = agents[["subj_prob_fault_alpha"]],
                           subj_prob_fault_beta = agents[["subj_prob_fault_beta"]],
                           subj_error_size_mu = agents[["subj_error_size_mu"]],
                           subj_error_size_sigma = agents[["subj_error_size_sigma"]])
}

# Assertion for Studies
assert_studies_properties <- function(study_id, agent_id, N, resources, cost, benefit, obj_effect_mu, obj_effect_sigma,
                                      obj_prob_fault, obj_error_size_mu, obj_error_size_sigma) {
  studies_properties <- list(study_id, agent_id, N, cost, benefit, obj_effect_mu, obj_effect_sigma,
                             obj_prob_fault, obj_error_size_mu, obj_error_size_sigma)
  # We allow inputs of 1 or of equal length for convenience
  assert_lengths_equal_or_one(studies_properties)
  # study_id and agent_id should not be shorter than the other properties, since we don't want them to be recycled
  assert_max_length(list(study_id = study_id), studies_properties)
  # Type assertions
  assert(
    check_integerish(study_id, lower = 1, any.missing = FALSE, unique = TRUE),
    check_character(study_id, any.missing = FALSE, unique = TRUE),
    check_null(study_id),
    combine = "or"
  )
  assert(
    check_integerish(agent_id, lower = 1, any.missing = FALSE),
    check_character(agent_id, any.missing = FALSE),
    check_null(agent_id),
    combine = "or"
  )
  assert_integerish(N, lower = 1, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(resources, finite = TRUE, any.missing = FALSE)
  assert_numeric(cost, finite = TRUE, any.missing = FALSE)
  assert_numeric(benefit, finite = TRUE, any.missing = FALSE)
  assert_numeric(obj_effect_mu, finite = TRUE, any.missing = FALSE)
  assert_numeric(obj_effect_sigma, finite = TRUE, any.missing = FALSE)
  assert_numeric(obj_prob_fault, lower = 0, upper = 1, any.missing = FALSE)
  assert_numeric(obj_error_size_mu, finite = TRUE, any.missing = FALSE)
  assert_numeric(obj_error_size_sigma, finite = TRUE, any.missing = FALSE)
}

assert_studies <- function(studies) {
  assert_data_frame(studies, ncols = 11)
  expected_names <- c("study_id", "agent_id", "resources", "N", "cost", "benefit", "obj_effect_mu", "obj_effect_sigma",
                      "obj_prob_fault", "obj_error_size_mu", "obj_error_size_sigma")
  assert_names(names(studies), type = "named", permutation.of = expected_names)
  assert_studies_properties(study_id = studies[["study_id"]],
                            agent_id = studies[["agent_id"]],
                            N = studies[["N"]],
                            resources = studies[["resources"]],
                            cost = studies[["cost"]],
                            benefit = studies[["benefit"]],
                            obj_effect_mu = studies[["obj_effect_mu"]],
                            obj_effect_sigma = studies[["obj_effect_sigma"]],
                            obj_prob_fault = studies[["obj_prob_fault"]],
                            obj_error_size_mu = studies[["obj_error_size_mu"]],
                            obj_error_size_sigma = studies[["obj_error_size_sigma"]])
}

# Assertions for Complete Studies (studies and agents combined)
assert_complete_studies <- function(complete_studies) {
  assert_data_frame(complete_studies, ncols = 17)
  expected_names <- c("study_id", "agent_id", "N", "resources", "cost", "benefit", "obj_effect_mu", "obj_effect_sigma",
                      "obj_prob_fault", "obj_error_size_mu", "obj_error_size_sigma",
                      "subj_effect_mu", "subj_effect_sigma", "subj_prob_fault_alpha", "subj_prob_fault_beta",
                      "subj_error_size_mu", "subj_error_size_sigma")
  assert_names(names(complete_studies), type = "named", permutation.of = expected_names)
  assert_studies_properties(study_id = complete_studies[["study_id"]],
                            agent_id = complete_studies[["agent_id"]],
                            N = complete_studies[["N"]],
                            resources = complete_studies[["resources"]],
                            cost = complete_studies[["cost"]],
                            benefit = complete_studies[["benefit"]],
                            obj_effect_mu = complete_studies[["obj_effect_mu"]],
                            obj_effect_sigma = complete_studies[["obj_effect_sigma"]],
                            obj_prob_fault = complete_studies[["obj_prob_fault"]],
                            obj_error_size_mu = complete_studies[["obj_error_size_mu"]],
                            obj_error_size_sigma = complete_studies[["obj_error_size_sigma"]])
  assert_agents_properties(agent_id = complete_studies[["agent_id"]],
                           subj_effect_mu = complete_studies[["subj_effect_mu"]],
                           subj_effect_sigma = complete_studies[["subj_effect_sigma"]],
                           subj_prob_fault_alpha = complete_studies[["subj_prob_fault_alpha"]],
                           subj_prob_fault_beta = complete_studies[["subj_prob_fault_beta"]],
                           subj_error_size_mu = complete_studies[["subj_error_size_mu"]],
                           subj_error_size_sigma = complete_studies[["subj_error_size_sigma"]])
}

# Assertions for Literature
assert_literature <- function(literature) {
  assert_list(literature, types = c("data.frame", "atomicvector", "list"), null.ok = TRUE)
  study_ids <- setdiff(names(literature), c("complete_studies", "seed", "use_same_seed"))
  studies <- literature[study_ids]
  lapply(studies, function(study) {
    assert_names(names(study), permutation.of = c("obj_effect_size", "fault_indicators", "error_sizes", "observed_effect_sizes",
                                                  "distr_after_observing_fault_ind", "distr_after_observing_effect", "stopped_in_round", "stopping_reason",
                                                  "belief_fault_this_round", "eu_criterion", "remaining_resources"))
  })
  # invisible since otherwise we'd return the list of names through lapply
  invisible(literature)
}
