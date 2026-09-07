test_that("summaries report completed and early-stopped searches", {
  studies <- make_study(
    study_id = c("s10", "s2", "s1"),
    agent_id = "a1",
    N = c(2, 3, 4),
    resources = c(2, 1, 4),
    benefit = c(10, 10, 0),
    obj_prob_fault = c(1, 1, 0),
    obj_effect_mu = c(0.5, 1, 1.5),
    obj_error_size_mu = c(1, -0.25, 0)
  )
  literature <- simulate_test_study(study = studies)[studies$study_id]

  expected <- data.frame(
    study_id = c("s10", "s2", "s1"),
    agent_id = rep("a1", 3),
    rounds_completed = c(2, 1, 0),
    stopping_reason = c(
      "Search completed", "Resources depleted", "Expected utility too low"
    ),
    final_resources = c(0, 0, 4),
    n_faults_total = c(2L, 3L, 0L),
    n_faults_discovered = c(2, 1, 0),
    true_effect = c(0.5, 1, 1.5),
    init_obs_effect = c(2.5, 0.25, 1.5),
    final_obs_effect = c(0.5, 0.5, 1.5),
    row.names = c("s10", "s2", "s1")
  )

  expect_equal(simulation_summary(literature), expected)
  expect_equal(
    simulation_summary(literature["s2"]),
    expected["s2", , drop = FALSE]
  )
})

test_that("detailed summaries preserve parameter values, types, and study order", {
  agents <- make_agent(agent_id = c(11L, 2L), sbj_prob_alpha = c(2, 4))
  studies <- make_study(
    study_id = c("zeta", "alpha", "middle"),
    agent_id = c(11L, 2L, 11L),
    N = c(2L, 3L, 4L),
    resources = c(2, 1, 4)
  )
  complete_studies <- combine_agents_studies(agents, studies)
  complete_studies <- complete_studies[
    match(studies$study_id, complete_studies$study_id),
    , drop = FALSE
  ]
  literature <- simulate_literature(complete_studies, seed = 123)
  original <- literature

  simple <- simulation_summary(literature)
  detailed <- simulation_summary(literature, simple = FALSE)

  expect_identical(detailed[names(simple)], simple)
  expect_identical(detailed$study_id, studies$study_id)
  expect_identical(detailed$agent_id, studies$agent_id)
  expect_identical(detailed$N, studies$N)
  expect_identical(detailed$init_resources, studies$resources)
  expect_false("resources" %in% names(detailed))
  expect_identical(anyDuplicated(names(detailed)), 0L)

  names(complete_studies)[names(complete_studies) == "resources"] <- "init_resources"
  rownames(complete_studies) <- complete_studies$study_id
  expect_identical(detailed[names(complete_studies)], complete_studies)
  expect_identical(
    simulation_summary(literature["alpha"], simple = FALSE),
    detailed["alpha", , drop = FALSE]
  )
  expect_identical(literature, original)
})

test_that("summary mode must be a single non-missing logical value", {
  literature <- simulate_test_study()

  for (simple in list(NA, NULL, logical(), c(TRUE, FALSE), 1, "TRUE")) {
    expect_error(simulation_summary(literature, simple = simple), "simple")
  }
})

test_that("empty literature can be summarized in either mode", {
  for (literature in list(list(), setNames(list(), character()))) {
    for (simple in c(TRUE, FALSE)) {
      summary <- simulation_summary(literature, simple = simple)
      expect_s3_class(summary, "data.frame")
      expect_equal(nrow(summary), 0)
    }
  }
})
