test_that("objective reality handles deterministic fault boundaries", {
  no_faults <- scerrModel:::simulate_reality(
    obj_prob_fault = 0,
    obj_effect_mu = 0.25,
    obj_effect_sigma2 = 0,
    obj_error_size_mu = 2,
    obj_error_size_sigma2 = 0,
    N = 4,
    seed = NULL,
    use_same_seed = FALSE
  )
  all_faults <- scerrModel:::simulate_reality(
    obj_prob_fault = 1,
    obj_effect_mu = -0.25,
    obj_effect_sigma2 = 0,
    obj_error_size_mu = -2,
    obj_error_size_sigma2 = 0,
    N = 4,
    seed = NULL,
    use_same_seed = FALSE
  )

  expect_equal(no_faults$true_effect, 0.25)
  expect_equal(no_faults$true_K, 0)
  expect_equal(no_faults$faults, rep(FALSE, 4))
  expect_equal(no_faults$error_sizes, numeric(4))

  expect_equal(all_faults$true_effect, -0.25)
  expect_equal(all_faults$true_K, 4)
  expect_equal(all_faults$faults, rep(TRUE, 4))
  expect_equal(all_faults$error_sizes, rep(-2, 4))
})

test_that("random objective reality preserves structural invariants", {
  reality <- with_preserved_rng({
    set.seed(42)
    scerrModel:::simulate_reality(
      obj_prob_fault = 0.35,
      obj_effect_mu = 0.25,
      obj_effect_sigma2 = 0.4,
      obj_error_size_mu = -0.1,
      obj_error_size_sigma2 = 0.7,
      N = 100,
      seed = NULL,
      use_same_seed = FALSE
    )
  })

  is_fault <- reality$faults
  expect_length(reality$faults, 100)
  expect_length(reality$error_sizes, 100)
  expect_equal(reality$true_K, sum(is_fault))
  expect_true(all(reality$error_sizes[!is_fault] == 0))
  expect_true(is.finite(reality$true_effect))
  expect_true(all(is.finite(reality$error_sizes)))
})
