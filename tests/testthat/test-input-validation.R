test_that("agent distribution parameters are validated", {
  expect_error(make_agent(sbj_effect_mu = "invalid"), "sbj_effect_mu")
  expect_error(make_agent(sbj_effect_sigma2 = 0), "sbj_effect_sigma2")
  expect_error(make_agent(sbj_effect_sigma2 = -1), "sbj_effect_sigma2")
  expect_error(make_agent(sbj_prob_alpha = 0), "sbj_prob_alpha")
  expect_error(make_agent(sbj_error_kappa = 0), "sbj_error_kappa")
  expect_error(make_agent(sbj_error_var_alpha = 1), "sbj_error_var_alpha")
  expect_error(make_agent(sbj_error_var_beta = 0), "sbj_error_var_beta")
  expect_error(
    make_agent(
      sbj_error_var_alpha = 1 + .Machine$double.eps,
      sbj_error_var_beta = .Machine$double.xmax
    ),
    "mean subjective error variance"
  )
})

test_that("study parameters are validated", {
  expect_error(make_study(resources = -1), "resources")
  expect_error(make_study(cost = 0), "cost")
  expect_error(make_study(cost = -1), "cost")
  expect_error(make_study(benefit = -1), "benefit")
  expect_error(make_study(obj_effect_mu = "invalid"), "obj_effect_mu")
  expect_error(make_study(obj_effect_sigma2 = -1), "obj_effect_sigma2")
  expect_error(make_study(obj_error_size_sigma2 = -1), "obj_error_size_sigma2")

  expect_no_error(make_study(resources = 0, benefit = 0))
  expect_no_error(make_study(obj_effect_sigma2 = 0, obj_error_size_sigma2 = 0))
})

test_that("agent identifiers determine row count and remain unique", {
  agents <- make_agent(agent_id = c("Alice", "Bob"))
  expect_equal(agents$agent_id, c("Alice", "Bob"))
  expect_equal(nrow(agents), 2)

  expect_error(
    make_agent(sbj_prob_alpha = c(2, 3), agent_id = c("Alice", "Alice")),
    "duplicated"
  )
})

test_that("all study properties follow scalar recycling rules", {
  expect_error(
    make_study(
      study_id = paste0("s", 1:4),
      agent_id = "a1",
      resources = c(1, 2)
    ),
    "length 1"
  )
})

test_that("simulation inputs must form one complete input mode", {
  expect_error(simulate_literature(agents = make_agent()), "supplied together")
  expect_error(simulate_literature(studies = make_study()), "supplied together")
  expect_error(simulate_literature(), "Supply `complete_studies`")
  expect_error(
    simulate_literature(agents = make_agent(), studies = make_study(), use_same_seed = 1),
    "use_same_seed"
  )
})
