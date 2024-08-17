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
#' @param keep_seed_const (`logical()`)\cr
#'   If `TRUE`, the seed is kept constant for all studies. See Details for more information.
#'   Default is `FALSE`.
#'
#' @return
#' A named `list` containing the input (`complete_studies` (passed or generated with `agents` and `studies`), `seed`, `keep_seed_const`) as well as
#' the simulation output as named lists (named as the respective `study_id`) for each study, with the following elements:
#'   * `obj_effect_size` (`numeric(1)`)\cr
#'     The simulated effect size in the objective reality.
#'   * `fault_indicators` (`numeric(N)`)\cr
#'     The simulated fault indicators per code unit.
#'   * `error_sizes` (`numeric(N)`)\cr
#'     The simulated error sizes per code unit.
#'   * `observed_effect_sizes` (`numeric(N+1)`)\cr
#'     The simulated effect sizes the agent might observe. The last entry is always equal to `obj_effect_size`.
#'   * `p_after_fault_ind` (`matrix(N+1, N+1)`)\cr
#'     Matrix of the discrete probability distribution of the number of remaining faults after observing the fault indicator.\cr
#'     Rows represent the search iteration, columns the number of remaining faults, where column one represents zero remaining faults
#'     and column `N+1` represents `N` remaining faults. \cr
#'     One row contains probabilities of all realizations of a discrete probability distribution.
#'     Row one is initialized with the prior belief about the number of remaining faults (Beta-Binomial(`N`, `subj_prob_fault_alpha`, `subj_prob_fault_beta`)).
#'     and row `N+1` shows the distribution after the last search round.
#'   * `p_after_effect` (`matrix(N, N+1)`)\cr
#'     Matrix of the discrete probability distribution of the number of remaining faults after observing the current effect size.\cr
#'     Rows represent the search iteration, columns the number of remaining faults, where column one represents zero remaining faults
#'     and column `N+1` represents `N` remaining faults. \cr
#'     One row contains probabilities of all realizations of a discrete probability distribution.
#'   * `stopped_in_round` (`integer(1)`)\cr
#'     The round in which the agent stopped searching. If `N+1`, the agent finished the last round successfully (instead of stopping in the last round.).
#'   * `stopping_reason` (`character(1)`)\cr
#'     The reason for stopping the search. Can be "Search completed", "Expected utility too low" or "Resources depleted".
#'   * `p_fault_this_round` (`numeric(N)`)\cr
#'     The subjective probability of observing a fault in the respective search round. Used to calculate `eu_criterion`.
#'   * `eu_criterion` (`numeric(N)`)\cr
#'     The expected utility criterion for continuing to search in the respective search round. Used to decide whether to continue searching or not.
#'   * `remaining_resources` (`numeric(1)`)\cr
#'     The remaining resources at the point the agent decided to stop searching.
#'
#' @details
#' The user may [set.seed()] before calling this function to ensure reproducibility of the simulation. However, setting the arguments `seed` and additionally `keep_seed_const` can offer
#' some additional control.
#' If `keep_seed_const = FALSE` (default), the simulation uses all seeds from `seed` to `seed + nrow(complete_studies) - 1` to keep the results of different studies independent while enabling the user
#' to change properties of one study without affecting the results of the simulation of other studies.
#' If performing multiple separate simulations, to ensure generation of independent literature data, use seeds that are further apart.
#' If `keep_seed_const = TRUE`, the seed is kept constant for all studies. This can be used to analyse differences in the simulation output due to changes in the study parameters while keeping the `seed` constant.
#' Be aware that simulations will be dependent.
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
#'   obj_error_size_mu = 0.1, obj_error_size_sigma = 0.2
#' )
#' # Simulate the study
#' simulate_literature(agents = agent, studies = study, seed = 123)
#'
#' @export
simulate_literature <- function(complete_studies, agents = NULL, studies = NULL, seed = NULL, keep_seed_const = FALSE) {
  # use full_studies if given, otherwise create complete_studies with combine_agents_studies
  # either complete_studies OR agents and studies must be given
  if (!is.null(agents) && !is.null(studies)) {
    assert_agents(agents)
    assert_studies(studies)
    cs <- combine_agents_studies(agents, studies)
  } else {
    assert_complete_studies(complete_studies)
    cs <- complete_studies
  }
  assert_int(seed, lower = 1, null.ok = TRUE)

  # Pre-allocate list for simulation results and name it
  sim_res <- vector(mode = "list", length = nrow(cs))
  names(sim_res) <- cs[["study_id"]]

  # For each study, simulate reality and run the agent search model
  for (study in seq_len(nrow(cs))) {
    # Simulate objective reality
    obj_reality <- simulate_obj_reality(
      obj_effect_mu = cs[study, "obj_effect_mu"],
      obj_effect_sigma = cs[study, "obj_effect_sigma"],
      obj_prob_fault = cs[study, "obj_prob_fault"],
      obj_error_size_mu = cs[study, "obj_error_size_mu"],
      obj_error_size_sigma = cs[study, "obj_error_size_sigma"],
      N = cs[study, "N"],
      seed = seed,
      keep_seed_const = keep_seed_const,
      i = study
    )
    # Run the agent search model
    search_res <- run_agent_search(
      agent_id = cs[study, "agent_id"],
      study_id = cs[study, "study_id"],
      N = cs[study, "N"],
      resources = cs[study, "resources"],
      cost = cs[study, "cost"],
      benefit = cs[study, "benefit"],
      subj_effect_mu = cs[study, "subj_effect_mu"],
      subj_effect_sigma = cs[study, "subj_effect_sigma"],
      subj_prob_fault_alpha = cs[study, "subj_prob_fault_alpha"],
      subj_prob_fault_beta = cs[study, "subj_prob_fault_beta"],
      subj_error_size_mu = cs[study, "subj_error_size_mu"],
      subj_error_size_sigma = cs[study, "subj_error_size_sigma"],
      effect_size = obj_reality[["effect_size"]],
      fault_ind = obj_reality[["fault_indicators"]],
      error_sizes = obj_reality[["error_sizes"]],
      obs_effect_sizes = obj_reality[["observed_effect_sizes"]]
    )

    sim_res[[study]] <- c(obj_reality, search_res)
  }

  res <- c(
    list(complete_studies = cs, seed = seed, keep_seed_const = keep_seed_const),
    sim_res
  )

  return(res)
}


#' Objective Reality
#'
#' This simulates the objective reality based on the given parameters.
#'
#' @usage NULL
simulate_obj_reality <- function(obj_effect_mu, obj_effect_sigma, obj_prob_fault, obj_error_size_mu, obj_error_size_sigma, N, seed, keep_seed_const, i) {
  # Saving and restoring the seed
  if (!is.null(seed)) {
    # Save the current random state and reinstate on exit
    global_seed <- .Random.seed
    on.exit({
      .Random.seed <<- global_seed
    }, add = TRUE)
    # Set the new seed
    # Move seed by i to keep results of different studies independent
    if (keep_seed_const) {
      set.seed(seed)
    } else {
      set.seed(seed + i - 1)
    }
  }

  effect_size <- rnorm(1, obj_effect_mu, obj_effect_sigma)
  fault_ind <- rbinom(N, 1, obj_prob_fault)
  error_sizes <- rnorm(N, obj_error_size_mu, obj_error_size_sigma)
  # Concatenate effect_size to show that the last observed effect size is the real effect size
  # (with no faults remaining)
  obs_effect_sizes <- c(
    effect_size + rev(cumsum(rev(fault_ind * error_sizes))),
    effect_size
  )

  res <- list(
    obj_effect_size = effect_size,
    fault_indicators = fault_ind,
    error_sizes = error_sizes,
    observed_effect_sizes = obs_effect_sizes
  )

  return(res)
}


#' Agent Search Model
#'
#' This is the implementation of the agent search model.
#'
#' @usage NULL
run_agent_search <- function(agent_id, study_id, N, resources, cost, benefit,
                             subj_effect_mu, subj_effect_sigma,
                             subj_prob_fault_alpha, subj_prob_fault_beta,
                             subj_error_size_mu, subj_error_size_sigma,
                             effect_size, fault_ind, error_sizes, obs_effect_sizes) {
  # Initialize Expected Utility Criterion for Continuing to Search
  eu_criterion <- numeric(N)

  # Initialize stopping information
  stopped_in_round = N + 1  # N+1 to show that agent finished last round instead of stopping in last round
  stopping_reason = "Search completed"

  # Initialize (Discrete) Probability Distributions as Matrices:
  #
  # Subjective Probability of Remaining Faults after OBSERVATION OF THE FAULT INDICATOR
  #  - rows: search iteration (N+1 since it is initialized before the first search round)
  #  - cols: number of remaining faults (N+1 since 0 remaining faults are possible)
  p_after_fault_ind <- matrix(0, nrow = N + 1, ncol = N + 1)
  # Before First Round: Probability of Remaining Faults is given by a Beta-Binomial(N, alpha, beta)
  p_after_fault_ind[1, ] <- extraDistr::dbbinom(x = seq(0, N), size = N,
                                                alpha = subj_prob_fault_alpha,
                                                beta = subj_prob_fault_beta)

  # Subjective Probability of Remaining Faults after OBSERVATION OF THE CURRENT EFFECT SIZE
  #  - rows: search iteration (N since this is only calculated once per round)
  #  - cols: number of remaining faults (N+1 since 0 remaining faults are possible)
  p_after_effect <- matrix(0, nrow = N, ncol = N + 1)

  # Subjective Probability of Observing a Fault in the Current Search Round
  p_fault_this_round <- numeric(N)

  for (i in seq_len(N)) {
    # 1. Update Subjective Probability of Remaining Faults after Observing the Current Effect Size
    prior <- p_after_fault_ind[i, ]
    # Likelihood of the observed effect size is given by N(subj_effect_mu + n_rem_faults * subj_error_size_mu, subj_effect_sigma^2 + n_rem_faults * subj_error_size_sigma^2)
    likelihood <- dnorm(
      x = obs_effect_sizes[[i]],
      mean = subj_effect_mu + seq(0, N) * subj_error_size_mu,
      sd = sqrt(subj_effect_sigma^2 + seq(0, N) * subj_error_size_sigma^2)
    )
    p_after_effect[i, ] <- prior * likelihood / sum(prior * likelihood) # why does this sum work?

    # 2. Calculate Subjective Probability of Observing a Fault in this Search Round
    p_fault_this_round[[i]] <- sum(p_after_effect[i, ] * seq(0, N)) / (N-(i-1))
    eu_criterion[[i]] <- p_fault_this_round[[i]] * benefit / cost

    # Agent DECISION to Continue Searching or not
    if (eu_criterion[[i]] < 1) {
      stopped_in_round <- i
      stopping_reason <- "Expected utility too low"
      break
    } else if (resources < cost) {
      stopped_in_round <- i
      stopping_reason <- "Resources depleted"
      break
    } else {
      # Otherwise, update resources
      resources <- resources - cost
    }

    # 3. Update Subjective Probability of Remaining Faults after Observing the Fault Indicator
    prior <- p_after_effect[i, ]
    likelihood <- dbinom(
      x = fault_ind[[i]],
      size = 1,
      prob = seq(0, N) / N  # FIXME: possibly erroneous implementation of formal model
    )
    posterior <- prior * likelihood

    # Update posterior for number of remaining faults for next round
    posterior <- posterior[1 + seq(0, N - i) + fault_ind[[i]]]

    # Change support according to possible number of remaining faults
    p_after_fault_ind[i + 1, 1 + seq(0, N - i)] <- posterior / sum(posterior)
  }

  list(
    p_after_fault_ind = p_after_fault_ind,
    p_after_effect = p_after_effect,
    stopped_in_round = stopped_in_round,
    stopping_reason = stopping_reason,
    p_fault_this_round = p_fault_this_round,
    eu_criterion = eu_criterion,
    remaining_resources = resources
  )
}
