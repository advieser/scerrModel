make_random_studies <- function() {
  make_study(
    study_id = c("s1", "s2"),
    agent_id = "a1",
    N = 20,
    resources = 20,
    obj_prob_fault = 0.4,
    obj_effect_sigma2 = 0.5,
    obj_error_size_sigma2 = 0.3
  )
}

test_that("both public simulation input modes produce the same result", {
  agents <- make_agent()
  studies <- make_random_studies()
  complete_studies <- combine_agents_studies(agents, studies)

  separate <- simulate_literature(agents = agents, studies = studies, seed = 123)
  combined <- simulate_literature(complete_studies, seed = 123)

  expect_identical(separate, combined)
  expect_named(separate, c("s1", "s2"))
})

test_that("simulation results have dimensions determined by each study", {
  agents <- make_agent()
  studies <- make_study(
    study_id = c("short", "long"),
    agent_id = "a1",
    N = c(1, 7),
    resources = 7,
    obj_prob_fault = 0.4,
    obj_effect_sigma2 = 0.5,
    obj_error_size_sigma2 = 0.3
  )
  literature <- simulate_literature(agents = agents, studies = studies, seed = 123)

  for (study_id in names(literature)) {
    study <- literature[[study_id]]
    N <- study$params$N

    expect_length(study$objective_reality$faults, N)
    expect_length(study$objective_reality$error_sizes, N)
    expect_equal(dim(study$history$fault_posteriors), c(N + 1L, N + 1L))
    expect_length(study$history$next_fault_belief, N)
    expect_length(study$history$eu_criterion, N)
  }
})

test_that("each study uses its own objective parameters", {
  agents <- make_agent()
  studies <- make_study(
    study_id = c("none", "all"),
    agent_id = "a1",
    N = 3,
    resources = 3,
    obj_prob_fault = c(0, 1),
    obj_effect_mu = c(1, 2),
    obj_effect_sigma2 = 0,
    obj_error_size_mu = c(0, 3),
    obj_error_size_sigma2 = 0
  )
  literature <- simulate_literature(agents = agents, studies = studies, seed = 123)

  expect_equal(literature$none$objective_reality$true_effect, 1)
  expect_equal(literature$none$objective_reality$true_K, 0)
  expect_equal(literature$none$objective_reality$error_sizes, numeric(3))

  expect_equal(literature$all$objective_reality$true_effect, 2)
  expect_equal(literature$all$objective_reality$true_K, 3)
  expect_equal(literature$all$objective_reality$error_sizes, rep(3, 3))
})

test_that("each study uses the parameters of its assigned agent", {
  agents <- make_agent(
    agent_id = c("low", "high"),
    sbj_prob_alpha = c(1, 3),
    sbj_prob_beta = c(3, 1)
  )
  studies <- make_study(
    study_id = c("low-study", "high-study"),
    agent_id = c("low", "high"),
    N = 1,
    resources = 0
  )
  literature <- simulate_literature(
    agents = agents,
    studies = studies,
    seed = 123,
    use_same_seed = TRUE
  )

  expect_lt(
    literature[["low-study"]]$history$next_fault_belief[[1]],
    literature[["high-study"]]$history$next_fault_belief[[1]]
  )
  expect_identical(
    literature[["low-study"]]$objective_reality,
    literature[["high-study"]]$objective_reality
  )
})

test_that("explicit seeds are reproducible and preserve the global RNG", {
  agents <- make_agent()
  studies <- make_random_studies()

  results <- with_preserved_rng({
    set.seed(987)
    state_before <- .Random.seed
    first <- simulate_literature(agents = agents, studies = studies, seed = 123)
    state_after <- .Random.seed
    second <- simulate_literature(agents = agents, studies = studies, seed = 123)

    list(
      first = first,
      second = second,
      state_before = state_before,
      state_after = state_after
    )
  })

  expect_identical(results$first, results$second)
  expect_identical(results$state_before, results$state_after)
})

test_that("explicit and external seed setting produce the same simulation", {
  agents <- make_agent()
  studies <- make_random_studies()

  results <- with_preserved_rng({
    set.seed(123)
    external_seed <- simulate_literature(agents = agents, studies = studies)
    explicit_seed <- simulate_literature(agents = agents, studies = studies, seed = 123)
    list(external = external_seed, explicit = explicit_seed)
  })

  expect_identical(results$external, results$explicit)
})

test_that("explicit seeds restore an initially absent RNG state", {
  agents <- make_agent()
  studies <- make_study()

  seed_exists_after <- with_preserved_rng({
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = ".Random.seed", envir = .GlobalEnv)
    }
    simulate_literature(agents = agents, studies = studies, seed = 123)
    exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  })

  expect_false(seed_exists_after)
})

test_that("shared seeds create matched objective realities across studies", {
  agents <- make_agent()
  studies <- make_random_studies()

  shared <- simulate_literature(
    agents = agents,
    studies = studies,
    seed = 123,
    use_same_seed = TRUE
  )
  sequential <- simulate_literature(
    agents = agents,
    studies = studies,
    seed = 123,
    use_same_seed = FALSE
  )

  expect_identical(shared[[1]]$objective_reality, shared[[2]]$objective_reality)
  expect_false(identical(
    sequential[[1]]$objective_reality,
    sequential[[2]]$objective_reality
  ))
})
