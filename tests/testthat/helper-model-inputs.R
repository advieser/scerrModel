agent_args <- list(
  sbj_prob_alpha = 2,
  sbj_prob_beta = 3,
  sbj_effect_mu = 0.5,
  sbj_effect_sigma2 = 0.25,
  sbj_error_mu = 0,
  sbj_error_kappa = 1,
  sbj_error_var_alpha = 3,
  sbj_error_var_beta = 1
)

lgr::get_logger("scerrModel")$set_threshold("fatal")

study_args <- list(
  N = 5,
  resources = 5,
  cost = 1,
  benefit = 10,
  obj_prob_fault = 1,
  obj_effect_mu = 0.5,
  obj_effect_sigma2 = 0,
  obj_error_size_mu = 1,
  obj_error_size_sigma2 = 0
)

make_agent <- function(...) {
  do.call(create_agents, utils::modifyList(agent_args, list(...)))
}

make_study <- function(...) {
  do.call(create_studies, utils::modifyList(study_args, list(...)))
}

simulate_test_study <- function(agent = make_agent(), study = make_study()) {
  simulate_literature(agents = agent, studies = study, seed = 123)
}

run_test_model <- function(
    faults = rep(FALSE, 5),
    error_sizes = numeric(length(faults)),
    true_effect = 0.5,
    resources = length(faults),
    cost = 1,
    benefit = 10,
    agent = agent_args) {
  args <- c(
    list(
      study_id = "test-study",
      agent_id = "test-agent",
      N = length(faults),
      benefit = benefit,
      cost = cost,
      resources = resources
    ),
    agent,
    list(
      true_effect = true_effect,
      faults = faults,
      error_sizes = error_sizes
    )
  )

  do.call(scerrModel:::run_agent_model, args)
}

with_preserved_rng <- function(code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = ".Random.seed", envir = .GlobalEnv)
    }
  })

  force(code)
}
