#' Create data.frame of Studies with all Required Properties
#'
#' Generates a data.frame to define Studies by their properties in a standardized format.
#' The output can be combined with the output of [create_agents()] to define a \link[=combine_agents_studies]{complete study}.
#'
#' @param N (`integer()`)\cr
#'   The number of code units in the study.
#' @param resources (`numeric()`)\cr
#'   The total resources aviailable for the study.
#' @param cost (`numeric()`)\cr
#'   The cost associated with one search round for the particular study.
#' @param benefit (`numeric()`)\cr
#'   The benefit associated with one search round for the particular study.
#' @param obj_effect_mu (`numeric()`)\cr
#'   The expected value of the objective effect size distribution (normal).
#' @param obj_effect_sigma (`numeric()`)\cr
#'   The standard deviation of the objective effect size distribution (normal).
#' @param obj_prob_fault (`numeric()`)\cr
#'   The probability of making an error in a search round (Bernoulli).
#' @param obj_error_size_mu (`numeric()`)\cr
#'   The expected value of the objective error size distribution (normal).
#' @param obj_error_size_sigma (`numeric()`)\cr
#'   The standard deviation of the objective error size distribution (normal).
#' @param study_id (`character()` or `integer()`)\cr
#'   Identifiers for Studies, has to be **unique**, i.e. the same `study_id` may only be used once.\cr
#'   If `NULL` IDs are an integer sequence from one to the number of rows. Default is `NULL`.
#' @param agent_id (`character()` or `integer()`)\cr
#'   Identifiers for Agents, has to be **unique**, i.e. the same `agent_id` may only be used once.\cr
#'   If `NULL` IDs are an integer sequence from one to the number of rows. Default is `NULL`.
#'
#' @return
#'   Studies `data.frame` with 10 columns and as many rows as the longest vector passed as an argument.
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
#'   obj_effect_mu = 0.4, obj_effect_sigma = 0.5,
#'   obj_prob_fault = 0.4,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma = 0.2
#' )
#' # Create multiple studies
#' studies <- create_studies(
#'   study_id = c("Alice2022", "BobEtAl2024"), agent_id = c("Alice", "Bob"),
#'   N = c(30, 20), resources = 100, cost = c(5, 10), benefit = c(10, 15),
#'   obj_effect_mu = 0.4, obj_effect_sigma = 0.5,
#'   obj_prob_fault = 0.4,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma = 0.2
#' )
#' @export
create_studies <- function(N, resources, cost, benefit, obj_effect_mu, obj_effect_sigma, obj_prob_fault,
                           obj_error_size_mu, obj_error_size_sigma, study_id = NULL, agent_id = NULL) {
  # Assertions
  assert_studies_properties(study_id = study_id,
                            agent_id = agent_id,
                            N = N,
                            resources = resources,
                            cost = cost,
                            benefit = benefit,
                            obj_effect_mu = obj_effect_mu,
                            obj_effect_sigma = obj_effect_sigma,
                            obj_prob_fault = obj_prob_fault,
                            obj_error_size_mu = obj_error_size_mu,
                            obj_error_size_sigma = obj_error_size_sigma)

  # Auto-generate IDs if not specified by user
  studies_properties <- list(agent_id, study_id, N, resources, cost, benefit, obj_effect_mu, obj_effect_sigma, obj_prob_fault, obj_error_size_mu, obj_error_size_sigma)
  if (is.null(study_id)) {
    study_id <- seq_len(max(lengths(studies_properties)))
  }
  if (is.null(agent_id)) {
    agent_id <- seq_len(max(lengths(studies_properties)))
  }

  # Generate standardized data.frame
  data.frame(
    study_id = study_id,
    agent_id = agent_id,
    N = N,
    resources = resources,
    cost = cost,
    benefit = benefit,
    obj_effect_mu = obj_effect_mu,
    obj_effect_sigma = obj_effect_sigma,
    obj_prob_fault = obj_prob_fault,
    obj_error_size_mu = obj_error_size_mu,
    obj_error_size_sigma = obj_error_size_sigma
  )
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
#'   agent_id = "Alice",
#'   subj_effect_mu = 0.3, subj_effect_sigma = 1.2,
#'   subj_prob_fault_alpha = 0.2, subj_prob_fault_beta = 0.4,
#'   subj_error_size_mu = 0.6, subj_error_size_sigma = 0.2
#' )
#' studies <- create_studies(
#'   study_id = "Alice2022", agent_id = "Alice",
#'   N = 30, resources = 100, cost = 4, benefit = 3,
#'   obj_effect_mu = 0.4, obj_effect_sigma = 0.5,
#'   obj_prob_fault = 0.4,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma = 0.2
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
  merge(studies, agents, by = "agent_id")
}
