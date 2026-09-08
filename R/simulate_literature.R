#' Simulation of literature data
#'
#' Simulates a literature data set based on the given parameters describing the studies.
#'
#' @param complete_studies (`data.frame`)\cr
#'   A `data.frame` built with [combine_agents_studies()] containing all properties of the agents and studies that are to be simulated.
#'   This is used unless both `agents` and `studies` are given.
#' @param agents (`data.frame`)\cr
#'   A `data.frame` built with [create_agents()] containing the agents' properties. When both `agents` and `studies` are given,
#'   they are combined to create `complete_studies`.
#' @param studies (`data.frame`)\cr
#'   A `data.frame` built with [create_studies()] containing the studies' properties. When both `agents` and `studies` are given,
#'   they are combined to create `complete_studies`.
#' @param seed (`integer()`)\cr
#'   The seed used for simulation. See Details for more information.
#'   If `NULL`, the seed is not set. Default is `NULL`.
#' @param use_same_seed (`logical()`)\cr
#'   If `TRUE`, the seed is kept constant for all studies. Will be ignored if `seed = NULL`. See Details for more information.
#'   Default is `FALSE`.
#'
#' @return
#' A named `list` with one element for each `study_id`. Each study has the following elements:
#'   * `params` (`list`)\cr
#'     The input parameters for the study and its agent.
#'   * `objective_reality` (`list`)\cr
#'     The simulated `true_effect`, total number of faults `true_K`, `faults`, and `error_sizes` per code unit.
#'   * `stop_conditions` (`list`)\cr
#'     The `rounds_completed`, `stopping_reason`, `final_resources`, `final_effect_size`, and `n_faults_discovered`.
#'   * `history` (`list`)\cr
#'     The `fault_posteriors` matrix and the `next_fault_belief` and `eu_criterion` vectors across search rounds.
#'
#' @details
#' The user may [set.seed()] before calling this function or pass a seed to the argument `seed` to ensure reproducibility of the simulation.
#' If `use_same_seed = FALSE` (default), these two options should lead to the same results, although using the argument `seed` avoids modifying the global environment.
#' If `use_same_seed = TRUE`, the same seed will be used for all studies. This allows the user to analyse effects of modifying input parameters without differing randomly generated numbers.
#' However, changing parameters that affect the objective reality directly (e.g., `obj_effect_mu`, `obj_effect_sigma2`, `obj_prob_fault`, `obj_error_size_mu`, `obj_error_size_sigma2`) will still lead to different results, since
#' these directly influence the random generation of the objective reality.
#'
#' @examples
#' # Create study and agent
#' agent <- create_agents(
#'   sbj_effect_mu = 0.3, sbj_effect_sigma2 = 1.2,
#'   sbj_prob_alpha = 0.2, sbj_prob_beta = 0.4,
#'   sbj_error_mu = 0.6, sbj_error_kappa = 1,
#'   sbj_error_var_alpha = 3, sbj_error_var_beta = 2
#' )
#' study <- create_studies(
#'   study_id = "Alice2022", agent_id = "Alice",
#'   N = 30, resources = 1000, cost = 400, benefit = 20,
#'   obj_effect_mu = 0.4, obj_effect_sigma2 = 0.5,
#'   obj_prob_fault = 0.4,
#'   obj_error_size_mu = 0.1, obj_error_size_sigma2 = 0.2
#' )
#' # Simulate the study
#' simulate_literature(agents = agent, studies = study, seed = 123)
#'
#' @export
simulate_literature <- function(complete_studies, agents = NULL, studies = NULL, seed = NULL, use_same_seed = FALSE) {
  has_agents <- !is.null(agents)
  has_studies <- !is.null(studies)
  if (xor(has_agents, has_studies)) {
    stop("`agents` and `studies` must be supplied together.", call. = FALSE)
  }

  if (has_agents) {
    assert_agents(agents)
    assert_studies(studies)
    study_specs <- combine_agents_studies(agents, studies)
  } else {
    if (missing(complete_studies)) {
      stop("Supply `complete_studies` or both `agents` and `studies`.", call. = FALSE)
    }
    assert_complete_studies(complete_studies)
    study_specs <- complete_studies
  }
  assert_int(seed, lower = 1, null.ok = TRUE)
  assert_flag(use_same_seed)

  # Seed setting
  if (!is.null(seed)) {
    # Save the current random state and reinstate on exit
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) {
      saved_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
      if (had_seed) {
        assign(".Random.seed", saved_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = ".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    # Set seed for whole simulation
    # If use_same_seed is TRUE, the seed is set from within simulate_reality().
    if (!use_same_seed) {
      set.seed(seed)
    }
  }

  # Pre-allocate list for simulation results and name elements by study IDs
  literature <- setNames(vector("list", nrow(study_specs)), study_specs[["study_id"]])

  # For each study, simulate reality and run the agent search model
  for (study_idx in seq_len(nrow(study_specs))) {
    log_start(
      study_specs[study_idx, "study_id"],
      study_specs[study_idx, "agent_id"],
      study_specs[study_idx, "N"]
    )

    # Simulate objective reality
    reality_args <- extract_args(study_specs, study_idx, c("obj_prob_fault", "obj_effect_mu", "obj_effect_sigma2",
      "obj_error_size_mu", "obj_error_size_sigma2", "N"))
    reality <- do.call(simulate_reality, c(reality_args, list(seed = seed, use_same_seed = use_same_seed)))

    # Run the agent search model
    search_args <- extract_args(study_specs, study_idx, c("study_id", "agent_id", "N", "benefit", "cost", "resources", "sbj_prob_alpha",
      "sbj_prob_beta", "sbj_effect_mu", "sbj_effect_sigma2", "sbj_error_mu", "sbj_error_kappa", "sbj_error_var_alpha", "sbj_error_var_beta"))
    search_result <- do.call(
      run_agent_model,
      c(search_args, reality[c("true_effect", "faults", "error_sizes")])
    )

    literature[[study_idx]] <- c(
      list(
        params = as.list(study_specs[study_idx, setdiff(names(study_specs), "study_id")]),
        objective_reality = reality[c("true_effect", "true_K", "faults", "error_sizes")]
      ),
      search_result
    )
  }

  # TODO: Save seed in output, maybe as attribute? Don't know how idiomatic that would be
  literature
}


#' Objective Reality
#'
#' This simulates the objective reality based on the given parameters.
#'
#' @usage NULL
#'
#' @noRd
simulate_reality <- function(obj_prob_fault, obj_effect_mu, obj_effect_sigma2, obj_error_size_mu,
                             obj_error_size_sigma2, N, seed, use_same_seed) {
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
  # Sample whether each code unit has a fault.
  faults <- sample(c(TRUE, FALSE), size = N, replace = TRUE, prob = c(obj_prob_fault, 1 - obj_prob_fault))
  # Save true number of faults
  true_K <- sum(faults)
  # Generate vector of true error sizes per round
  error_sizes <- numeric(N)
  error_sizes[faults] <- rnorm(true_K, obj_error_size_mu, sqrt(obj_error_size_sigma2))

  list(
    true_effect = true_effect,
    true_K = true_K,
    faults = faults,
    error_sizes = error_sizes
  )
}

extract_args <- function(df, row, cols) {
  if (!all(cols %in% names(df))) {
    stop("One or more columns not found in the data frame.")
  }
  if (row < 1 || row > nrow(df)) {
    stop("Row index out of bounds.")
  }
  # Extract values and name them with the column names
  setNames(as.list(df[row, cols, drop = FALSE]), cols)
}
