#' Create data.frame of Agents with all Required Properties
#'
#' Generates a `data.frame` to define Agents by their properties in a standardized format.
#' The output can be combined with the output of [create_studies()] to define a \link[=combine_agents_studies]{complete study}.
#'
#' @param sbj_prob_alpha (`numeric()`)\cr
#'   The alpha parameter of the beta distribution for the probability of finding a fault in a search round. Must be greater than zero.
#' @param sbj_prob_beta (`numeric()`)\cr
#'   The beta parameter of the beta distribution for the probability of finding a fault in a search round. Must be greater than zero.
#' @param sbj_effect_mu (`numeric()`)\cr
#'   The mean of the agent's normal prior for the true effect.
#' @param sbj_effect_sigma2 (`numeric()`)\cr
#'   The variance of the agent's normal prior for the true effect. Must be greater than zero.
#' @param sbj_error_mu (`numeric()`)\cr
#'   The mean parameter of the agent's Normal-Inverse-Gamma belief for error sizes.
#' @param sbj_error_kappa (`numeric()`)\cr
#'   The strength parameter of the agent's Normal-Inverse-Gamma belief for error sizes. Must be greater than zero.
#' @param sbj_error_var_alpha (`numeric()`)\cr
#'   The shape parameter of the agent's Inverse-Gamma belief for the error variance. Must be greater than 1.
#' @param sbj_error_var_beta (`numeric()`)\cr
#'   The scale parameter of the agent's Inverse-Gamma belief for the error variance. Must be greater than zero.
#' @param agent_id (`character()` or `integer()`)\cr
#'   Identifiers for Agents, has to be **unique**, i.e. the same `agent_id` may only be used once.\cr
#'   If `NULL`, IDs are generated as `a1`, `a2`, and so on. Default is `NULL`.
#'
#' @return (`data.frame`)\cr
#' Agents `data.frame` with 9 columns and as many rows as the longest vector passed as an argument
#' to this function. Column names are standardized.
#'
#' @details
#' As a convenience functionality, this function can take atomic vectors of equal length or length 1 as input. If the input is of length 1,
#' the value is recycled to match the length of the longest input vector.
#' For more complex automatically generated inputs, users may want to use functions such as [rep()] or [seq()].
#'
#' @examples
#' # Generate one agent, ID is generated automatically
#' create_agents(
#'   sbj_prob_alpha = 2, sbj_prob_beta = 3,
#'   sbj_effect_mu = 0.3, sbj_effect_sigma2 = 1.2,
#'   sbj_error_mu = 0.6, sbj_error_kappa = 1,
#'   sbj_error_var_alpha = 3, sbj_error_var_beta = 2
#' )
#' # Generate multiple agents
#' create_agents(
#'   sbj_prob_alpha = c(2, 3), sbj_prob_beta = c(3, 4),
#'   sbj_effect_mu = c(0.3, 0.4), sbj_effect_sigma2 = c(1.2, 1.3),
#'   sbj_error_mu = c(0.6, 0.7), sbj_error_kappa = 1,
#'   sbj_error_var_alpha = 3, sbj_error_var_beta = 2,
#'   agent_id = c("Alice", "Bob")
#' )
#'
#' @export
create_agents <- function(sbj_prob_alpha, sbj_prob_beta, sbj_effect_mu, sbj_effect_sigma2,
                          sbj_error_mu, sbj_error_kappa, sbj_error_var_alpha, sbj_error_var_beta, agent_id = NULL) {
  # Assertions
  agent_properties <- mget(c("sbj_prob_alpha", "sbj_prob_beta", "sbj_effect_mu", "sbj_effect_sigma2",
                             "sbj_error_mu", "sbj_error_kappa", "sbj_error_var_alpha", "sbj_error_var_beta", "agent_id"))
  do.call(assert_agents_properties, agent_properties)

  # Auto-generate IDs if not specified by user
  if (is.null(agent_id)) {
    agent_properties$agent_id <- paste0("a", seq_len(max(lengths(agent_properties))))
  }

  # Generate standardized data.frame
  agents <- as.data.frame(agent_properties)
  assert_agents(agents)
  agents
}

#' Create data.frame of Studies with all Required Properties
#'
#' Generates a data.frame to define Studies by their properties in a standardized format.
#' The output can be combined with the output of [create_agents()] to define a \link[=combine_agents_studies]{complete study}.
#'
#' @param N (`integer()`)\cr
#'   The number of code units in the study.
#' @param resources (`numeric()`)\cr
#'   The total resources available for the study. Must be non-negative.
#' @param cost (`numeric()`)\cr
#'   The cost associated with one search round for the particular study. Must be greater than zero.
#' @param benefit (`numeric()`)\cr
#'   The benefit associated with one search round for the particular study. Must be non-negative.
#' @param obj_prob_fault (`numeric()`)\cr
#'   The probability of making an error in a search round (Bernoulli).
#' @param obj_effect_mu (`numeric()`)\cr
#'   The mean of the objective true effect distribution (normal).
#' @param obj_effect_sigma2 (`numeric()`)\cr
#'   The variance of the objective true effect distribution (normal). Must be non-negative.
#' @param obj_error_size_mu (`numeric()`)\cr
#'   The expected value of the objective error size distribution (normal).
#' @param obj_error_size_sigma2 (`numeric()`)\cr
#'   The variance of the objective error size distribution (normal). Must be non-negative.
#' @param study_id (`character()` or `integer()`)\cr
#'   Identifiers for Studies, has to be **unique**, i.e. the same `study_id` may only be used once.\cr
#'   If `NULL`, IDs are generated as `s1`, `s2`, and so on. Default is `NULL`.
#' @param agent_id (`character()` or `integer()`)\cr
#'   Identifier of the agent assigned to each study. An agent may be assigned to multiple studies.\cr
#'   If `NULL`, IDs are generated as `a1`, `a2`, and so on. Default is `NULL`.
#'
#' @return
#'   Studies `data.frame` with 11 columns and as many rows as the longest vector passed as an argument.
#'   Column names are standardized.
#'
#' @details
#' As a convenience functionality, this function can take atomic vectors of equal length or length 1 as input. If the input is of length 1,
#' the value is recycled to match the length of the longest vector.
#' For more complex automatically generated inputs, users may want to use functions such as [rep()] or [seq()].
#'
#' @examples
#' # Create a study
#' study <- create_studies(
#'   study_id = "Alice2022", agent_id = "Alice",
#'   N = 30, resources = 1000, cost = 400, benefit = 20,
#'   obj_prob_fault = 0.4,
#'   obj_effect_mu = 0.4, obj_effect_sigma2 = 0.5,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma2 = 0.2
#' )
#' # Create multiple studies
#' studies <- create_studies(
#'   study_id = c("Alice2022", "BobEtAl2024"), agent_id = c("Alice", "Bob"),
#'   N = c(30, 20), resources = 100, cost = c(5, 10), benefit = c(10, 15),
#'   obj_prob_fault = 0.4,
#'   obj_effect_mu = 0.4, obj_effect_sigma2 = 0.5,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma2 = 0.2
#' )
#' @export
create_studies <- function(N, resources, cost, benefit, obj_prob_fault, obj_effect_mu, obj_effect_sigma2,
                           obj_error_size_mu, obj_error_size_sigma2, study_id = NULL, agent_id = NULL) {
  # Assertions
  studies_properties <- mget(c("N", "resources", "cost", "benefit", "obj_prob_fault", "obj_effect_mu", "obj_effect_sigma2",
                               "obj_error_size_mu", "obj_error_size_sigma2", "study_id", "agent_id"))
  do.call(assert_studies_properties, studies_properties)

  # Auto-generate IDs if not specified by user
  if (is.null(study_id)) {
    studies_properties$study_id <- paste0("s", seq_len(max(lengths(studies_properties))))
  }
  if (is.null(agent_id)) {
    studies_properties$agent_id <- paste0("a", seq_len(max(lengths(studies_properties))))
  }

  # Generate standardized data.frame
  as.data.frame(studies_properties)
}

#' Combine Agents and Studies into one data.frame
#'
#' Function to create a `data.frame` containing the combined properties of agents and studies, refered to as `complete_studies`.
#'
#' @param agents (`data.frame`)\cr
#' A `data.frame` built with [create_agents()] containing the agents' properties.
#' @param studies (`data.frame`)\cr
#' A `data.frame` built with [create_studies()] containing the studies' properties.
#'
#' @returns (`data.frame`)
#' A `data.frame` containing the combined properties of agents and studies.
#' The `data.frame` is created by merging the two input `data.frame`s by the `agent_id` column,
#' i.e. agents' properties are added to the studies' properties based on the `agent_id` assigned to that study.
#'
#' @examples
#' # Create agents and studies
#' agents <- create_agents(
#'   sbj_prob_alpha = 2, sbj_prob_beta = 3,
#'   sbj_effect_mu = 0.3, sbj_effect_sigma2 = 1.2,
#'   sbj_error_mu = 0.6, sbj_error_kappa = 1,
#'   sbj_error_var_alpha = 3, sbj_error_var_beta = 2,
#'   agent_id = "Alice"
#' )
#' studies <- create_studies(
#'   N = 30, resources = 100, cost = 4, benefit = 3,
#'   obj_prob_fault = 0.4,
#'   obj_effect_mu = 0.4, obj_effect_sigma2 = 0.5,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma2 = 0.2,
#'   study_id = "Alice2022", agent_id = "Alice"
#' )
#' # Combine agents and studies
#' combine_agents_studies(agents, studies)
#' @export
combine_agents_studies <- function(agents, studies) {
  assert_agents(agents)
  assert_studies(studies)

  if (!all(studies[["agent_id"]] %in% agents[["agent_id"]])) {
    warning("Studies contain agent_id not defined in agents. These are being ignored.")
  }
  if (!all(agents[["agent_id"]] %in% studies[["agent_id"]])) {
    warning("Agents contain agent_id not given in studies. These are being ignored.")
  }

  # Merge agents and studies by agent_id
  study_specs <- merge(studies, agents, by = "agent_id")
  study_specs[c("study_id", "agent_id", setdiff(names(study_specs), c("study_id", "agent_id")))]
}
