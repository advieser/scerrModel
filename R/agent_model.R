#' Agent Search Model
#'
#' This is the implementation of the agent search model. For documentation of the internally created data structures,
#' see the documentation of for the return value of [`simulate_literature()`][simulate_literature].
#'
#' @export
run_agent_model <- function(study_id, agent_id, N, benefit, cost, resources,
                            sbj_prob_alpha, sbj_prob_beta, sbj_effect_mu, sbj_effect_sigma2,
                            sbj_mean_mu, sbj_mean_kappa, sbj_var_alpha, sbj_var_beta,
                            true_effect, true_K, fault_indicators, error_sizes) {
  # Create a vector representing the possible number of faults
  K_vals <- seq(0, N)
  total_error_size <- true_effect + sum(error_sizes)
  obs_error_sizes <- numeric(0)
  b <- 0  # number of observed faults

  # Subjective prior for the number of total faults K
  K_prior <- dbetabinom(K_vals, N, sbj_prob_alpha, sbj_prob_alpha)

  # We'll record the posterior over K after each draw.
  posterior_K_history <- matrix(nrow = N + 1, ncol = N + 1)  # filled with NAs
  # At time 0, before any draws, the posterior is the prior
  posterior_K_history[1, ] <- K_prior

  # Pre-allocate and initialize variables
  init_resources <- resources
  fault_belief <- setNames(numeric(N), seq(1, N))
  eu_criterion <- setNames(numeric(N), seq(1, N))

  # Perform decision making and search once per round i or until stopped
  for (i in seq_len(N)) {
    # Calculate the belief for finding a fault in this upcoming round
    num_faults_remaining <- posterior_K_history[i, seq(b + 1, N + 1)]
    total_mass <- sum(num_faults_remaining)
    if (sum(total_mass) != 0) {
      num_faults_remaining <- num_faults_remaining / sum(total_mass)  # normalize
      expected_num_faults_remaining <- sum(num_faults_remaining * seq(0, N - b))
    } else {
      # Handle edge case: division by zero when calculating expected value if total mass is zero
      expected_num_faults_remaining <- 0
    }
    fault_belief[[i]] <- expected_num_faults_remaining / (N - i + 1)

    # Calculate expected utility and make decision
    eu_criterion[[i]] <- fault_belief[[i]] * benefit / cost
    if (eu_criterion[[i]] < 1) {
      stopped_in_round <- i - 1  # -1 since we start with prior before first round
      stopping_reason <- "Expected utility too low"
      log_stop(study_id, i, stopping_reason)
      break
    } else if (resources < cost) {
      stopped_in_round <- i - 1
      stopping_reason <- "Resources depleted"
      log_stop(study_id, i, stopping_reason)
      break
    } else {
      # Otherwise, update resources and continue searching
      resources <- resources - cost
    }

    # Start observation, update information if a fault is observed
    if (fault_indicators[i] == "fault") {
      b <- b + 1
      error_size <- error_sizes[[i]]
      obs_error_sizes[[length(obs_error_sizes) + 1]] <- error_size
      total_error_size <- total_error_size - error_size
    }

    # For candidate k from b to N as possible total number of faults, compute an (unnormalized) posterior weight
    k <- seq(b, N)

    # Likelihood: P(data | K) is factorized into three conditionally independent components:
    # 1. Observed fault indicators:
    #    P(b | i, N, K) = Hypergeometric probability of observing b faults in i trials,
    #    given K total faults in N trials.
    lk_fault_indicators <- dhyper(b, k, N - k, i)

    # 2. Observed error sizes (for b observed faults):
    #    P(obs_error_sizes | K) = \int \int P(x | mu, sigma2)^b × P(mu, sigma) dμ dsigma2,
    #    where P(mu, sigma2) is the Normal-Inverse-Gamma prior and P(x | mu, sigma2) is the normal likelihood.
    #    This is the marginal likelihood under the conjugate prior.
    lk_error_sizes <- calculate_obs_errors_marginal_likelihood(obs_error_sizes, sbj_mean_mu, sbj_mean_kappa, sbj_var_alpha, sbj_var_beta)

    # 3. Remaining total error from (K - b) unobserved faults:
    #    P(total_error_size | K, b) = Normal(sum | mean = (K - b) * mu, variance = (K - b) * sigma2),
    #    assuming known mu, sigma2 (from prior), and that errors are independent.
    lk_total_error <- vapply(k, function(k_i) {
      calculate_remaining_total_error_likelihood(
        total_error_size, k_i, b, sbj_effect_mu, sbj_effect_sigma2, sbj_mean_mu, sbj_var_beta, sbj_var_alpha
      )
    }, numeric(1))

    # Calculate combined posterior weight: prior(K) * likelihood(data | K)
    # Note: We index by k + 1 because k ranges from 0 to N (length = k+1).
    posterior_K <- numeric(length(K_vals))
    posterior_K[k + 1] <- K_prior[k + 1] * lk_fault_indicators * lk_error_sizes * lk_total_error

    # Normalize the posterior over K:
    denom <- sum(posterior_K)
    posterior_K_history[i + 1, ] <- if (denom > 0) posterior_K / denom else rep(0, length(posterior_K))

    # Update the stopping information if we terminate naturally
    if (i == N) {
      stopped_in_round = N
      stopping_reason = "Search completed"
      log_complete(study_id)
    }
  }

  list(
    params = list(
      N = N, sbj_prob_alpha = sbj_prob_alpha, sbj_prob_beta = sbj_prob_beta,
      sbj_mean_mu = sbj_mean_mu, sbj_mean_kappa = sbj_mean_kappa,
      sbj_var_alpha = sbj_var_alpha, sbj_var_beta = sbj_var_beta
    ),
    stop_conditions = list(
      stopped_in_round = stopped_in_round,
      stopping_reason = stopping_reason,
      final_error_size = total_error_size
    ),
    resource_management = list(
      init_resources = init_resources,
      last_resources = resources,
      cost = cost,
      benefit = benefit
    ),
    history = list(
      posterior_K = posterior_K_history,
      fault_belief = fault_belief,
      eu_criterion = eu_criterion
    ),
    objective_reality = list(
      true_num_faults = true_K,
      error_sizes = error_sizes  # fault_indicators implied
    )
  )
}

# Computes the marginal likelihood of a vector of observed error sizes,
# integrating out both the unknown mean and variance using a Normal-Inverse-Gamma prior.
#
# Arguments:
#   x: vector of observed error sizes
#   mu0: prior mean for the normal component
#   kappa0: prior strength (pseudo-count) on the mean
#   alpha0: shape parameter of the inverse gamma prior on variance
#   beta0: scale parameter of the inverse gamma prior on variance
#
# Returns:
#   A scalar representing the marginal likelihood
calculate_obs_errors_marginal_likelihood <- function(x, mu0, kappa0, alpha0, beta0) {
  n <- length(x)
  if (n == 0) return(1)

  x_bar <- mean(x)
  s_sq <- sum((x - x_bar)^2)

  kappa_n <- kappa0 + n
  alpha_n <- alpha0 + n / 2
  beta_n <- beta0 + 0.5 * s_sq + (kappa0 * n * (x_bar - mu0)^2) / (2 * kappa_n)

  log_lik <- lgamma(alpha_n) - lgamma(alpha0) +  # gamma term
    alpha0 * log(beta0) - alpha_n * log(beta_n) +  # scale term
    0.5 * log(kappa0 / kappa_n) -  # norm term
    (n / 2) * log(2 * pi)  # const term

  exp(log_lik)
}

# Likelihood for remaining (unobserved) total error size
#
# This function computes the likelihood of observing the remaining total error size,
# assuming that (k - b) faults remain unobserved and each produces a normally distributed error.
#
# Formally:
#   Let r = k - b be the number of unobserved faults.
#   If each unobserved error is independently distributed as N(μ, σ²), then the sum of r such errors
#   follows a normal distribution: N(r * μ, r * σ²).
#
#   So the likelihood is:
#     P(total_error_size | K = k, b, μ, σ²) = N(total_error_size | mean = r * μ, sd = sqrt(r) * σ)
#
#   If r = 0 (i.e., no faults remain), the total remaining error must be 0.
#   To handle numerical imprecision, we check whether the total_error_size is within a small tolerance.
#
# Arguments:
#   total_error_size: remaining total error to explain
#   k: candidate total number of faults
#   b: number of faults already observed
#   mu: assumed mean error size (e.g., prior mean)
#   sigma: assumed standard deviation of error sizes (e.g., derived from prior)
#   tolerance: numerical tolerance for zero-error checking (default: 1e-8)
#
# Returns:
#   A scalar representing the likelihood
calculate_remaining_total_error_likelihood <- function(total_error_size, k, b, effect_mu, effect_sigma2, error_mu, error_alpha, error_beta, tolerance = 1e-8) {
  r <- k - b
  if (r > 0) {
    mean <- effect_mu + r * error_mu
    sd <- sqrt(effect_sigma2 + r * error_alpha / (error_beta - 1))
    dnorm(total_error_size, mean, sd)
  } else if (r == 0) {
    # else 0 since it is impossible for there to be no faults remaining and still a total error
    if (abs(total_error_size) < tolerance) 1 else 0
  }
}
