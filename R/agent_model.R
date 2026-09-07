#' Agent Search Model
#'
#' This is the implementation of the agent search model. For documentation of the internally created data structures,
#' see the documentation of for the return value of [`simulate_literature()`][simulate_literature].
#'
#' @noRd
run_agent_model <- function(study_id, agent_id, N, benefit, cost, resources,
                            sbj_prob_alpha, sbj_prob_beta, sbj_effect_mu, sbj_effect_sigma2,
                            sbj_error_mu, sbj_error_kappa, sbj_error_var_alpha, sbj_error_var_beta,
                            true_effect, faults, error_sizes) {
  # Create a vector representing the possible number of faults
  faults_support <- seq(0, N)
  observed_effect <- true_effect + sum(error_sizes)
  observed_errors <- numeric()
  n_found <- 0
  resources_left <- resources

  # Subjective prior for the number of total faults K
  log_fault_prior <- dbetabinom(faults_support, N, sbj_prob_alpha, sbj_prob_beta, log = TRUE)
  fault_prior <- normalize_log_weights(log_fault_prior)

  # We'll record the posterior over K after each draw.
  fault_posteriors <- matrix(nrow = N + 1, ncol = N + 1)
  # At time 0, before any draws, the posterior is the prior
  fault_posteriors[1, ] <- fault_prior

  # Pre-allocate and initialize variables
  next_fault_belief <- setNames(numeric(N), seq_len(N))
  eu_criterion <- setNames(numeric(N), seq(1, N))

  # Perform decision making and search once per round or until stopped
  for (round in seq_len(N)) {
    # Calculate the belief for finding a fault in this upcoming round
    remaining_probs <- fault_posteriors[round, seq(n_found + 1, N + 1)]
    remaining_probs <- remaining_probs / sum(remaining_probs)
    expected_remaining <- sum(remaining_probs * seq(0, N - n_found))
    next_fault_belief[[round]] <- expected_remaining / (N - round + 1)

    # Calculate expected utility and make decision
    eu_criterion[[round]] <- next_fault_belief[[round]] * benefit / cost
    if (eu_criterion[[round]] < 1) {
      rounds_completed <- round - 1
      stopping_reason <- "Expected utility too low"
      log_stop(study_id, round, stopping_reason)
      break
    } else if (resources_left < cost) {
      rounds_completed <- round - 1
      stopping_reason <- "Resources depleted"
      log_stop(study_id, round, stopping_reason)
      break
    } else {
      # Otherwise, update resources and continue searching
      resources_left <- resources_left - cost
    }

    # Start observation, update information if a fault is observed
    if (faults[[round]]) {
      n_found <- n_found + 1
      error_size <- error_sizes[[round]]
      observed_errors <- c(observed_errors, error_size)
      observed_effect <- observed_effect - error_size
    }

    # Evaluate candidate values for the total number of faults.
    candidate_counts <- seq(n_found, N)

    # Likelihood: P(data | K) has two candidate-dependent components:
    # 1. Count of faults found in the completed rounds.
    log_count_lik <- dhyper(
      n_found, candidate_counts, N - candidate_counts, round, log = TRUE
    )

    # TODO: sbj_error_kappa is effectively a dead parameter because this term
    # is constant across candidate counts and cancels during normalization.
    log_error_lik <- calculate_obs_errors_marginal_likelihood(
      observed_errors,
      sbj_error_mu,
      sbj_error_kappa,
      sbj_error_var_alpha,
      sbj_error_var_beta,
      log = TRUE
    )

    # 2. Updated observed effect with unobserved faults:
    #    The observed effect is the true effect plus the errors from the remaining faults.
    log_effect_lik <- vapply(candidate_counts, function(candidate_count) {
      observed_effect_likelihood(
        observed_effect, candidate_count, n_found,
        sbj_effect_mu, sbj_effect_sigma2, sbj_error_mu,
        sbj_error_var_alpha = sbj_error_var_alpha,
        sbj_error_var_beta = sbj_error_var_beta,
        log = TRUE
      )
    }, numeric(1))

    # Normalize on the log scale to retain very small likelihoods.
    log_posterior <- log_fault_prior[candidate_counts + 1] + log_count_lik +
      log_error_lik + log_effect_lik
    posterior <- numeric(length(faults_support))
    posterior[candidate_counts + 1] <- normalize_log_weights(log_posterior)
    fault_posteriors[round + 1, ] <- posterior

    # Update the stopping information if we terminate naturally
    if (round == N) {
      rounds_completed <- N
      stopping_reason <- "Search completed"
      log_complete(study_id)
    }
  }

  list(
    stop_conditions = list(
      rounds_completed = rounds_completed,
      stopping_reason = stopping_reason,
      final_resources = resources_left,
      final_effect_size = observed_effect,
      n_faults_discovered = n_found
    ),
    history = list(
      fault_posteriors = fault_posteriors,
      next_fault_belief = next_fault_belief,
      eu_criterion = eu_criterion
    )
  )
}

normalize_log_weights <- function(log_weights) {
  if (anyNA(log_weights) || any(log_weights == Inf)) {
    stop("Cannot normalize non-finite posterior weights.", call. = FALSE)
  }

  max_log_weight <- max(log_weights)
  if (!is.finite(max_log_weight)) {
    stop("All posterior candidates have zero probability.", call. = FALSE)
  }

  weights <- exp(log_weights - max_log_weight)
  weights / sum(weights)
}

# Marginal likelihood under the Normal-Inverse-Gamma error prior.
calculate_obs_errors_marginal_likelihood <- function(
    x, sbj_error_mu, sbj_error_kappa, sbj_error_var_alpha,
    sbj_error_var_beta, log = FALSE) {
  n <- length(x)
  if (n == 0L) {
    return(if (log) 0 else 1)
  }

  x_bar <- mean(x)
  sum_squares <- sum((x - x_bar)^2)
  updated_kappa <- sbj_error_kappa + n
  updated_alpha <- sbj_error_var_alpha + n / 2
  updated_beta <- sbj_error_var_beta + 0.5 * sum_squares +
    sbj_error_kappa * n * (x_bar - sbj_error_mu)^2 / (2 * updated_kappa)

  log_likelihood <- lgamma(updated_alpha) - lgamma(sbj_error_var_alpha) +
    sbj_error_var_alpha * log(sbj_error_var_beta) -
    updated_alpha * log(updated_beta) +
    0.5 * log(sbj_error_kappa / updated_kappa) -
    n / 2 * log(2 * pi)

  if (log) log_likelihood else exp(log_likelihood)
}

# Likelihood for the observed effect size
#
# This function computes the likelihood of the observed effect, assuming that
# (candidate_count - n_found) faults remain unobserved and each produces a normally distributed error.
#
# Formally:
#   Let r = candidate_count - n_found be the number of unobserved faults.
#   The subjective true effect follows N(sbj_effect_mu, sbj_effect_sigma2).
#   Each unobserved error has mean sbj_error_mu. Its variance is replaced by
#   the mean of the Inverse-Gamma(sbj_error_var_alpha, sbj_error_var_beta)
#   distribution: sbj_error_var_beta / (sbj_error_var_alpha - 1).
#   Because the true effect and errors are independent, the observed effect follows:
#
#     N(sbj_effect_mu + r * sbj_error_mu,
#       sbj_effect_sigma2 + r * sbj_error_var_beta / (sbj_error_var_alpha - 1))
#
#   For r = 0, this reduces to the subjective distribution of the true effect.
#
# Arguments:
#   observed_effect: observed effect after correcting the discovered errors
#   candidate_count: candidate total number of faults
#   n_found: number of faults already observed
#   sbj_effect_mu: subjective mean of the true effect
#   sbj_effect_sigma2: subjective variance of the true effect
#   sbj_error_mu: subjective mean error size
#   sbj_error_var_alpha: shape of the Inverse-Gamma error-variance distribution; must be greater than 1
#   sbj_error_var_beta: scale of the Inverse-Gamma error-variance distribution
#
# Returns:
#   A scalar representing the likelihood
observed_effect_likelihood <- function(observed_effect, candidate_count, n_found,
                                       sbj_effect_mu, sbj_effect_sigma2, sbj_error_mu,
                                       sbj_error_var_alpha, sbj_error_var_beta,
                                       log = FALSE) {
  r <- candidate_count - n_found
  error_variance <- sbj_error_var_beta / (sbj_error_var_alpha - 1)
  expected_mean <- sbj_effect_mu + r * sbj_error_mu
  sd <- sqrt(sbj_effect_sigma2 + r * error_variance)
  dnorm(observed_effect, expected_mean, sd, log = log)
}
