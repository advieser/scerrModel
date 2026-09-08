test_that("one-round searches identify the true fault count", {
  uniform_agent <- utils::modifyList(
    agent_args,
    list(sbj_prob_alpha = 1, sbj_prob_beta = 1)
  )

  for (fault in c(FALSE, TRUE)) {
    result <- run_test_model(
      faults = fault,
      error_sizes = 0,
      resources = 1,
      cost = 1,
      benefit = 1e6,
      agent = uniform_agent
    )
    true_K <- as.integer(fault)

    expect_equal(
      result$history$next_fault_belief[[1]],
      result$history$fault_posteriors[1, 2]
    )
    expect_equal(
      result$history$eu_criterion[[1]],
      result$history$next_fault_belief[[1]] * 1e6
    )
    expect_equal(result$stop_conditions$rounds_completed, 1)
    expect_equal(result$stop_conditions$stopping_reason, "Search completed")
    expect_equal(result$stop_conditions$final_resources, 0)
    expect_equal(result$history$fault_posteriors[2, ], as.numeric(0:1 == true_K))
  }
})

test_that("stopping conditions include their exact boundaries and precedence", {
  uniform_agent <- utils::modifyList(
    agent_args,
    list(sbj_prob_alpha = 1, sbj_prob_beta = 1)
  )

  low_utility <- run_test_model(
    faults = TRUE,
    error_sizes = 2,
    resources = 1,
    cost = 1,
    benefit = 0,
    agent = uniform_agent
  )
  both_conditions_fail <- run_test_model(
    faults = TRUE,
    error_sizes = 2,
    resources = 0.5,
    cost = 1,
    benefit = 0,
    agent = uniform_agent
  )
  no_resources <- run_test_model(
    faults = TRUE,
    error_sizes = 2,
    resources = 0,
    cost = 1,
    benefit = 1e6,
    agent = uniform_agent
  )
  exact_resources <- run_test_model(
    faults = TRUE,
    error_sizes = 2,
    resources = 1,
    cost = 1,
    benefit = 1e6,
    agent = uniform_agent
  )

  expect_equal(low_utility$stop_conditions$rounds_completed, 0)
  expect_equal(low_utility$stop_conditions$stopping_reason, "Expected utility too low")
  expect_equal(low_utility$stop_conditions$final_resources, 1)
  expect_equal(low_utility$stop_conditions$final_effect_size, 2.5)

  expect_lt(both_conditions_fail$history$eu_criterion[[1]], 1)
  expect_equal(both_conditions_fail$stop_conditions$rounds_completed, 0)
  expect_equal(both_conditions_fail$stop_conditions$stopping_reason, "Resources depleted")
  expect_equal(both_conditions_fail$stop_conditions$final_resources, 0.5)
  expect_equal(both_conditions_fail$stop_conditions$final_effect_size, 2.5)
  expect_equal(both_conditions_fail$stop_conditions$n_faults_discovered, 0)

  expect_equal(no_resources$stop_conditions$rounds_completed, 0)
  expect_equal(no_resources$stop_conditions$stopping_reason, "Resources depleted")
  expect_equal(no_resources$stop_conditions$n_faults_discovered, 0)

  expect_equal(exact_resources$stop_conditions$rounds_completed, 1)
  expect_equal(exact_resources$stop_conditions$stopping_reason, "Search completed")
  expect_equal(exact_resources$stop_conditions$final_resources, 0)
  expect_equal(exact_resources$stop_conditions$final_effect_size, 0.5)
})

test_that("early stopping accounts only for inspected units", {
  result <- run_test_model(
    faults = c(TRUE, TRUE, FALSE),
    error_sizes = c(2, 3, 0),
    resources = 1,
    cost = 1,
    benefit = 1e6
  )

  expect_equal(result$stop_conditions$rounds_completed, 1)
  expect_equal(result$stop_conditions$stopping_reason, "Resources depleted")
  expect_equal(result$stop_conditions$final_resources, 0)
  expect_equal(result$stop_conditions$n_faults_discovered, 1)
  expect_equal(result$stop_conditions$final_effect_size, 3.5)
  expect_true(all(is.na(result$history$fault_posteriors[3:4, ])))
  expect_equal(unname(result$history$next_fault_belief[3]), 0)
  expect_equal(unname(result$history$eu_criterion[3]), 0)
})

test_that("completed searches maintain posterior support and accounting", {
  faults <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
  errors <- c(0, 0.4, 0, -0.2, 0)
  result <- run_test_model(
    faults = faults,
    error_sizes = errors,
    resources = 10,
    cost = 2,
    benefit = 1e6
  )

  posterior <- result$history$fault_posteriors
  expect_equal(rowSums(posterior), rep(1, 6), tolerance = 1e-14)
  expect_true(all(is.finite(posterior)))
  expect_true(all(posterior >= 0 & posterior <= 1))

  for (round in 0:5) {
    n_found <- if (round == 0) 0 else sum(faults[seq_len(round)])
    feasible <- n_found:(n_found + 5 - round)
    impossible <- setdiff(0:5, feasible)
    expect_equal(posterior[round + 1, impossible + 1], numeric(length(impossible)))
  }

  expect_equal(result$history$next_fault_belief * 1e6 / 2, result$history$eu_criterion)
  expect_true(all(result$history$next_fault_belief >= 0 & result$history$next_fault_belief <= 1))
  expect_equal(result$history$fault_posteriors[6, ], as.numeric(0:5 == 2))
  expect_equal(result$stop_conditions$rounds_completed, 5)
  expect_equal(result$stop_conditions$final_resources, 0)
  expect_equal(result$stop_conditions$n_faults_discovered, 2)
  expect_equal(result$stop_conditions$final_effect_size, 0.5, tolerance = 1e-15)
})
