run_model <- function(N, sbj_alpha, sbj_beta, sbj_mu, sbj_sigma, benefit, cost, resources) {
  # Objective Reality ----------------------- #
  # Simulate the true number of faults
  # TODO: Instead, draw from objective reality, e.g. Bernoulli
  true_K <- rbetabinom(1, N, sbj_alpha, sbj_beta)

  # Generate fault indicators per round
  # TODO: Can combine this with code above if Bernoulli using sample()
  fault_indicators <- c(rep("no_fault", N - true_K), rep("fault", true_K))[sample(N)]

  # Sisbj_mulate true error sizes
  # TODO: Instead, draw from objective reality
  true_error_sizes <- rnorm(true_K, mean = sbj_mu, sd = sbj_sigma)

  # Generate vector of error sizes per round
  # TODO: Combine this with code from above, when objective reality implemented
  error_sizes <- numeric(N)
  error_sizes[which(fault_indicators == "fault")] <- true_error_sizes

  # Agent Model ----------------------------- #
  # TODO: Add logging.
  # Create a vector representing the possible number of faults
  K_vals <- seq(0, N)
  total_error_size <- sum(error_sizes)
  obs_error_sizes <- numeric(0)
  b <- 0  # number of observed faults

  # Subjective prior for the number of total faults K
  K_prior <- dbetabinom(K_vals, N, sbj_alpha, sbj_alpha)

  # We'll record the posterior over K after each draw.
  posterior_K_history <- matrix(nrow = N + 1, ncol = N + 1)
  # At time 0, before any draws, the posterior is the prior
  posterior_K_history[1, ] <- K_prior

  # Pre-allocate and initialize variables
  init_resources <- resources
  belief_fault <- setNames(numeric(N + 1), K_vals)
  eu_criterion <- setNames(numeric(N + 1), K_vals)

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
    belief_fault[[i]] <- expected_num_faults_remaining / (N - i + 1)

    # Calculate expected utility and make decision
    eu_criterion[[i]] <- belief_fault[[i]] * benefit / cost
    if (eu_criterion[[i]] < 1) {
      stopped_in_round <- i - 1  # -1 since we start with prior before first round
      stopping_reason <- "Expected utility too low"
      break
    } else if (resources < cost) {
      stopped_in_round <- i - 1
      stopping_reason <- "Resources depleted"
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

    # For candidate k from b to N, compute an (unnormalized) posterior weight
    posterior_K <- numeric(length(K_vals))

    for (k in seq(b, N)) {
      # Likelihood(data|K, sbj_mu, sbj_sigma) is factorized into three parts for different types of data, assuming conditional independence:
      # 1. observed fault indicators
      #    P(b | i, N, K) = probability of b faults in i rounds, for N total rounds, of which k contain faults
      lk_fault_indicators <- dhyper(b, k, N - k, i)  # TODO: move this out of the loop and vectorize
      # 2. observed error sizes
      #    P(obs_error_sizes | sbj_mu, sbj_sigma)
      lk_error_sizes <- if (b > 0) prod(dnorm(obs_error_sizes, sbj_mu, sbj_sigma)) else 1
      # 3. current remaining total error
      #    P(total error | K, b, sbj_mu, sbj_sigma)
      lk_total_error <- calculate_remaining_total_error_likelihood(total_error_size, k, b, sbj_mu, sbj_sigma)

      # Calculate combined posterior weight: prior(K) * likelihood(data | K)
      # Note: We index by k+1 because k ranges from 0 to N.
      posterior_K[k + 1] <- K_prior[k + 1] * lk_fault_indicators * lk_error_sizes * lk_total_error
    }

    # Normalize the posterior over K:
    posterior_K_history[i + 1, ] <- posterior_K / sum(posterior_K)

    # Update the stopping information if we terminate naturally
    if (i == N) {
      stopped_in_round = N
      stopping_reason = "Search completed"
    }
  }

  list(
    params = list(N = N, sbj_alpha = sbj_alpha, sbj_beta = sbj_beta, sbj_mu = sbj_mu, sbj_sigma = sbj_sigma),
    stop_conditions = list(
      stopped_in_round = stopped_in_round,
      stopping_reason = stopping_reason,
      total_error_size = total_error_size
    ),
    ressource_management = list(
      init_resources = init_resources,
      last_resources = resources,
      cost = cost,
      benefit = benefit
    ),
    history = list(
      posterior_K = posterior_K_history,
      belief_fault = belief_fault,
      eu_criterion = eu_criterion
    ),
    objective_reality = list(
      true_num_faults = true_K,
      error_sizes = error_sizes  # true_error_sizes and fault_indicators implied
    )
  )
}

# Aggregated likelihood for the remaining error sizes:
# If r = k - b > 0, then the sum of r independent N(sbj_mu_fixed, sigma_fixed^2) variables is distributed as N(r*sbj_mu_fixed, r*sigma_fixed^2).
# If r = 0, then S_current sbj_must be 0 since no errors remain.
calculate_remaining_total_error_likelihood <- function(total_error_size, k, b, mu, sigma, tolerance = 1e-8) {
  r <- k - b
  if (r > 0) {
    dnorm(total_error_size, r * mu, sqrt(r) * sigma)
  } else if (r == 0) {
    # else 0 since it is impossible to for there to be no faults remaining and still a total error
    if (abs(total_error_size) < tolerance) 1 else 0
  }
}
