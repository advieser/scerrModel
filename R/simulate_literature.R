#' Simulation of literature data
#'
#' Simulates a literature data set based on the given parameters describing the studies.
#'
#' @param complete_studies (`data.frame`)\cr
#'   A `data.frame` built with [combine_agents_studies()] containing all properties of the agents and studies that are to be simulated.
#' @param agents (`data.frame`)\cr
#'   A `data.frame` built with [create_agents()] containing the agents' properties. If `complete_studies` is given, this parameter is ignored.
#'   Otherwise this is combined with `studies` to create `complete_studies`.
#' @param studies (`data.frame`)\cr
#'   A `data.frame` built with [create_studies()] containing the studies' properties. If `complete_studies` is given, this parameter is ignored.
#'   Otherwise this is combined with `agents` to create `complete_studies`.
#' @param seed (`integer()`)\cr
#'   The seed used for simulation. See Details for more information.
#'   If `NULL`, the seed is not set. Default is `NULL`.
#' @param use_same_seed (`logical()`)\cr
#'   If `TRUE`, the seed is kept constant for all studies. Will be ignored if `seed = NULL`. See Details for more information.
#'   Default is `FALSE`.
#'
#' @return
#' A named `list` containing the input (`complete_studies` (passed or generated with `agents` and `studies`), `seed`, `use_same_seed`) as well as
#' the simulation output as named lists (named as the respective `study_id`) for each study, with the following elements:
#'   * `obj_effect_size` (`numeric(1)`)\cr
#'     The simulated effect size in the objective reality.
#'   * `fault_indicators` (`numeric(N)`)\cr
#'     The simulated fault indicators per code unit.
#'   * `error_sizes` (`numeric(N)`)\cr
#'     The simulated error sizes per code unit.
#'   * `observed_effect_sizes` (`numeric(N+1)`)\cr
#'     The simulated effect sizes the agent might observe. The last entry is always equal to `obj_effect_size`.
#'   * `distr_after_observing_fault_ind` (`matrix(N+1, N+1)`)\cr
#'     Matrix of the discrete probability distribution of the number of remaining faults after observing the fault indicator.\cr
#'     Rows represent the search iteration, columns the number of remaining faults, where the first column represents zero remaining faults
#'     and column `N+1` represents `N` remaining faults.\cr
#'     One row contains probabilities for all possible numbers of remaining faults, thus giving the full probability distribution.
#'     Row one is initialized with the prior belief about the number of remaining faults (Beta-Binomial(`N`, `subj_prob_fault_alpha`, `subj_prob_fault_beta`)).
#'     and row `N+1` shows the distribution after the last search round.
#'   * `distr_after_observing_effect` (`matrix(N, N+1)`)\cr
#'     Matrix of the discrete probability distribution of the number of remaining faults after observing the current effect size.\cr
#'     Rows represent the search iteration, columns the number of remaining faults, where the first column represents zero remaining faults
#'     and column `N+1` represents `N` remaining faults. \cr
#'     One row contains probabilities for all possible numbers of remaining faults, thus giving the full probability distribution.
#'   * `stopped_in_round` (`integer(1)`)\cr
#'     The round in which the agent stopped searching. If `N+1`, the agent finished the last round successfully (instead of stopping in the last round.).
#'   * `stopping_reason` (`character(1)`)\cr
#'     The reason for stopping the search. Can be "Search completed", "Expected utility too low" or "Resources depleted".
#'   * `belief_fault_this_round` (`numeric(N)`)\cr
#'     The subjective probability of observing a fault in the respective search round. Used to calculate `eu_criterion`.
#'   * `eu_criterion` (`numeric(N)`)\cr
#'     The expected utility criterion for continuing to search in the respective search round. Used to decide whether to continue searching or not.
#'   * `remaining_resources` (`numeric(1)`)\cr
#'     The remaining resources at the point the agent decided to stop searching.
#'
#' @details
#' The user may [set.seed()] before calling this function or pass a seed to the argument `seed` to ensure reproducibility of the simulation.
#' If `use_same_seed = FALSE` (default), these two options should lead to the same results, although using the argument `seed` avoids modifying the global environment.
#' If `use_same_seed = TRUE`, the same seed will be used for all studies. This allows the user to analyse effects of modifying input parameters without differing randomly generated numbers.
#' However, changing parameters that affect the objective reality directly (e.g., `obj_effect_mu`, `obj_effect_sigma`, `obj_prob_fault`, `obj_error_size_mu`, `obj_error_size_sigma2`) will still lead to different results, since
#' these directly influence the random generation of the objective reality.
#'
#' @examples
#' # Create study and agent
#' agent <- create_agents(
#'   subj_effect_mu = 0.3, subj_effect_sigma = 1.2,
#'   subj_prob_fault_alpha = 0.2, subj_prob_fault_beta = 0.4,
#'   subj_error_size_mu = 0.6, subj_error_size_sigma = 0.2
#' )
#' study <- create_studies(
#'   study_id = "Alice2022", agent_id = "Alice",
#'   N = 30, resources = 1000, cost = 400, benefit = 20,
#'   obj_effect_mu = 0.4, obj_effect_sigma = 0.5,
#'   obj_prob_fault = 0.4,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma2 = 0.2
#' )
#' # Simulate the study
#' simulate_literature(agents = agent, studies = study, seed = 123)
#'
#' @export
simulate_literature <- function(complete_studies, agents = NULL, studies = NULL, seed = NULL, use_same_seed = FALSE) {
  # use full_studies if given, otherwise create complete_studies with combine_agents_studies (either complete_studies OR
  # agents and studies must be given)
  if (!is.null(agents) && !is.null(studies)) {
    assert_agents(agents)
    assert_studies(studies)
    cs <- combine_agents_studies(agents, studies)
  } else {
    assert_complete_studies(complete_studies)
    cs <- complete_studies
  }
  assert_int(seed, lower = 1, null.ok = TRUE)

  # Seed setting
  if (!is.null(seed)) {
    # Save the current random state and reinstate on exit
    global_seed <- .Random.seed
    on.exit({
      .Random.seed <<- global_seed
    }, add = TRUE)
    # Set seed for whole simulation
    # If use_same_seed is TRUE, the seed is set from within simulate_obj_reality().
    if (!use_same_seed) {
      set.seed(seed)
    }
  }

  # Pre-allocate list for simulation results and name elements by study IDs
  sim_res <- setNames(vector("list", nrow(cs)), cs[["study_id"]])

  # For each study, simulate reality and run the agent search model
  for (study in seq_len(nrow(cs))) {
    log_start(cs[study, "study_id"], cs[study, "agent_id"], cs[study, "N"])

    # Simulate objective reality
    obj_args <- get_params(cs, study, c("obj_prob_fault", "obj_effect_mu", "obj_effect_sigma2",
      "obj_error_size_mu", "obj_error_size_sigma2", "N"))
    obj_reality <- do.call(simulate_obj_reality, c(obj_args, list(seed = seed, use_same_seed = use_same_seed)))

    # Run the agent search model
    model_args <- get_params(cs, study, c("study_id", "agent_id", "N", "benefit", "cost", "resources", "sbj_prob_alpha",
      "sbj_prob_beta", "sbj_effect_mu", "sbj_effect_sigma2", "sbj_mean_mu", "sbj_mean_kappa", "sbj_var_alpha", "sbj_var_beta"))
    sim_res[[study]] <- do.call(run_agent_model, c(model_args, obj_reality))
  }

  sim_res
}


#' Objective Reality
#'
#' This simulates the objective reality based on the given parameters.
#'
#' @usage NULL
simulate_obj_reality <- function(obj_prob_fault, obj_effect_mu, obj_effect_sigma2, obj_error_size_mu, obj_error_size_sigma2,
                                 N, seed, use_same_seed) {
  # Use same seed for all studies. All other seed handling is done in simulate_literature().
  if (use_same_seed) {
    if (!is.null(seed)) {
      set.seed(seed)
    } else {
      warning("No seed given. Argument `use_same_seed = TRUE` will be ignored.")
    }
  }

  # Generate true effect size
  true_effect <- rnorm(1, obj_effect_mu, sqrt(obj_effect_sigma2))
  # Generate fault indicators per round, effectively sampling from Bernoulli(p = obj_prob_fault)
  fault_indicators <- sample(c("fault", "no_fault"), size = N, replace = TRUE, prob = c(obj_prob_fault, 1 - obj_prob_fault))
  # Save true number of faults
  true_K <- sum(fault_indicators == "fault")
  # Generate vector of true error sizes per round
  error_sizes <- numeric(N)
  error_sizes[fault_indicators == "fault"] <- rnorm(true_K, obj_error_size_mu, sqrt(obj_error_size_sigma2))

  list(
    true_effect = true_effect,
    true_K = true_K,
    fault_indicators = fault_indicators,
    error_sizes = error_sizes
  )
}

get_params <- function(df, row, cols) {
  if (!all(cols %in% names(df))) {
    stop("One or more columns not found in the data frame.")
  }
  if (row < 1 || row > nrow(df)) {
    stop("Row index out of bounds.")
  }
  # Extract values and name them with the column names
  setNames(as.list(df[row, cols, drop = FALSE]), cols)
}
