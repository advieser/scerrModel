test_that("posterior normalization is stable for very small likelihoods", {
  agent <- make_agent(
    sbj_effect_mu = 10,
    sbj_effect_sigma2 = 0.01,
    sbj_error_var_beta = 0.01
  )
  study <- make_study(
    obj_prob_fault = 0,
    obj_effect_mu = 0,
    obj_effect_sigma2 = 0.01,
    obj_error_size_sigma2 = 0.01
  )

  result <- simulate_test_study(agent, study)[[1]]
  completed <- seq_len(result$stop_conditions$rounds_completed + 1L)
  posterior <- result$history$fault_posteriors[completed, , drop = FALSE]

  expect_true(all(is.finite(posterior)))
  expect_equal(rowSums(posterior), rep(1, nrow(posterior)), tolerance = 1e-14)
})

test_that("log-scale posterior matches a direct small-model calculation", {
  args <- c(
    list(
      study_id = "worked-study",
      agent_id = "worked-agent",
      N = 5,
      benefit = 10,
      cost = 1,
      resources = 5
    ),
    agent_args,
    list(
      true_effect = 0.5,
      faults = c(FALSE, TRUE, FALSE, TRUE, FALSE),
      error_sizes = c(0, 0.4, 0, -0.2, 0)
    )
  )
  result <- do.call(scerrModel:::run_agent_model, args)

  N <- args$N
  K <- 0:N
  prior <- choose(N, K) * beta(K + 2, N - K + 3) / beta(2, 3)
  expected <- matrix(0, nrow = N + 1, ncol = N + 1)
  expected[1, ] <- prior

  for (round in seq_len(N)) {
    observed <- seq_len(round)
    n_found <- sum(args$faults[observed])
    effect <- args$true_effect + sum(args$error_sizes) - sum(args$error_sizes[observed])

    for (candidate_count in n_found:N) {
      count_likelihood <- choose(candidate_count, n_found) *
        choose(N - candidate_count, round - n_found) / choose(N, round)
      remaining <- candidate_count - n_found
      effect_likelihood <- dnorm(effect, 0.5, sqrt(0.25 + remaining * 0.5))
      expected[round + 1, candidate_count + 1] <-
        prior[candidate_count + 1] * count_likelihood * effect_likelihood
    }
    expected[round + 1, ] <- expected[round + 1, ] / sum(expected[round + 1, ])
  }

  expect_equal(result$history$fault_posteriors, expected, tolerance = 1e-14)
})

test_that("candidate-independent error likelihood cannot overflow posterior", {
  agent <- make_agent(sbj_error_var_beta = 1e-24)
  study <- make_study(
    N = 50,
    resources = 50,
    obj_error_size_mu = 1e-12
  )

  expect_no_error(result <- simulate_test_study(agent, study)[[1]])
  completed <- seq_len(result$stop_conditions$rounds_completed + 1L)
  posterior <- result$history$fault_posteriors[completed, , drop = FALSE]
  expect_equal(rowSums(posterior), rep(1, nrow(posterior)), tolerance = 1e-14)
})

test_that("zero-size faults are counted when they are discovered", {
  study <- make_study(obj_error_size_mu = 0)
  result <- simulate_test_study(study = study)[[1]]

  expect_equal(result$objective_reality$true_K, 5)
  expect_equal(result$objective_reality$faults, rep(TRUE, 5))
  expect_equal(result$stop_conditions$rounds_completed, 5)
  expect_equal(result$stop_conditions$n_faults_discovered, 5)
})

test_that("fault discovery plots count zero-size faults", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggtext")
  skip_if_not_installed("reshape2")

  study <- make_study(obj_error_size_mu = 0)
  literature <- simulate_test_study(study = study)
  plot_data <- plot_fault_discovery(literature)[[1]]$data
  discovered <- plot_data$count[plot_data$status == "num_discovered"]

  expect_equal(discovered, seq_len(5))
})
