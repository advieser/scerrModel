#' Create data.frame of Agents with all Required Properties
#'
#' Generates a data.frame to define Agents by their properties in a standardized format.
#' The output can be combined with the output of [create_studies()] to define a \link[=combine_agents_studies]{complete study}.
#'
#' @param subj_effect_mu (`numeric()`)\cr
#'   The mean of the agents' initial belief for the effect size distribution (normal).
#' @param subj_effect_sigma (`numeric()`)\cr
#'   The variance of the agents' initial belief for the effect size distribution (normal).
#' @param subj_prob_fault_alpha (`numeric()`)\cr
#'   The alpha parameter of the beta distribution for the probability of making an error in a search round.
#' @param subj_prob_fault_beta (`numeric()`)\cr
#'   The beta parameter of the beta distribution for the probability of making an error in a search round.
#' @param subj_error_size_mu (`numeric()`)\cr
#'   The mean of the agents' initial belief for the error size distribution (normal).
#' @param subj_error_size_sigma (`numeric()`)\cr
#'   The variance of the agents' initial belief for the error size distribution (normal).
#' @param agent_id (`character()` or `integer()`)\cr
#'   Identifiers for Agents, has to be **unique**, i.e. the same `agent_id` may only be used once.\cr
#'   If `NULL` IDs are an integer sequence from one to the number of rows. Default is `NULL`.
#'
#' @return (`data.frame`)\cr
#' Agents `data.frame` with 7 columns and as many rows as the longest vector passed as an argument
#' to this function. Column names are standardized.
#'
#' @details
#' As a convenience functionality, this function can take atomic vectors of equal length or length 1 as input. If the input is of length 1,
#' the value is recycled to match the length of the longest vector.
#' For more complex automatically generated inputs, users may want to use functions such as [rep()] or [seq()].
#'
#' @examples
#' # Generate one agent, ID is generated automatically
#' create_agents(
#'   subj_effect_mu = 0.3, subj_effect_sigma = 1.2,
#'   subj_prob_fault_alpha = 0.2, subj_prob_fault_beta = 0.4,
#'   subj_error_size_mu = 0.6, subj_error_size_sigma = 0.2
#' )
#' # Generate multiple agents
#' create_agents(
#'   agent_id = c("Alice", "Bob"),
#'   subj_effect_mu = c(0.3, 0.4), subj_effect_sigma = c(1.2, 1.3),
#'   subj_prob_fault_alpha = c(0.2, 0.3), subj_prob_fault_beta = c(0.4, 0.5),
#'   subj_error_size_mu = c(0.6, 0.7), subj_error_size_sigma = c(1.2, 1.3)
#' )
#'
#' @export
create_agents <- function(subj_effect_mu, subj_effect_sigma, subj_prob_fault_alpha,
                          subj_prob_fault_beta, subj_error_size_mu, subj_error_size_sigma, agent_id = NULL) {
  # Assertions
  assert_agents_properties(agent_id = agent_id,
                           subj_effect_mu = subj_effect_mu,
                           subj_effect_sigma = subj_effect_sigma,
                           subj_prob_fault_alpha = subj_prob_fault_alpha,
                           subj_prob_fault_beta = subj_prob_fault_beta,
                           subj_error_size_mu = subj_error_size_mu,
                           subj_error_size_sigma = subj_error_size_sigma)

  # Auto-generate IDs if not specified by user
  agents_properties <- list(subj_effect_mu, subj_effect_sigma, subj_prob_fault_alpha,
                            subj_prob_fault_beta, subj_error_size_mu, subj_error_size_sigma)
  if (is.null(agent_id)) {
    agent_id <- seq_len(max(lengths(agents_properties)))
  }

  # Generate standardized data.frame
  data.frame(
    agent_id = agent_id,
    subj_effect_mu = subj_effect_mu,
    subj_effect_sigma = subj_effect_sigma,
    subj_prob_fault_alpha = subj_prob_fault_alpha,
    subj_prob_fault_beta = subj_prob_fault_beta,
    subj_error_size_mu = subj_error_size_mu,
    subj_error_size_sigma = subj_error_size_sigma
  )
}
