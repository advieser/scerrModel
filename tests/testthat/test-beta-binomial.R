test_that("beta-binomial probabilities match the closed-form density", {
  K <- 0:8
  expected <- choose(8, K) * beta(K + 2, 8 - K + 5) / beta(2, 5)

  density <- scerrModel:::dbetabinom(K, 8, 2, 5)
  log_density <- scerrModel:::dbetabinom(K, 8, 2, 5, log = TRUE)

  expect_equal(density, expected, tolerance = 1e-15)
  expect_equal(exp(log_density), expected, tolerance = 1e-15)
  expect_equal(sum(density), 1, tolerance = 1e-15)
})

test_that("extreme beta-binomial priors can be normalized", {
  for (parameters in list(c(1e-8, 1e-8), c(1e-8, 1e6), c(1e6, 1e-8))) {
    log_density <- scerrModel:::dbetabinom(
      0:1000,
      N = 1000,
      alpha = parameters[[1]],
      beta = parameters[[2]],
      log = TRUE
    )
    density <- scerrModel:::normalize_log_weights(log_density)

    expect_false(anyNA(log_density))
    expect_true(all(is.finite(density)))
    expect_true(all(density >= 0))
    expect_equal(sum(density), 1, tolerance = 1e-15)
  }
})

test_that("log-weight normalization retains relative mass at tiny scales", {
  normalized <- scerrModel:::normalize_log_weights(c(-10000, -10001, -Inf))

  expect_equal(normalized[1:2], c(1, exp(-1)) / (1 + exp(-1)))
  expect_equal(normalized[[3]], 0)
  expect_error(
    scerrModel:::normalize_log_weights(c(-Inf, -Inf)),
    "zero probability"
  )
  expect_error(
    scerrModel:::normalize_log_weights(c(0, Inf)),
    "non-finite"
  )
})

test_that("beta-binomial sampling is reproducible and stays on its support", {
  samples <- with_preserved_rng({
    set.seed(314)
    first <- scerrModel:::rbetabinom(200, N = 12, alpha = 0.2, beta = 4)
    set.seed(314)
    second <- scerrModel:::rbetabinom(200, N = 12, alpha = 0.2, beta = 4)
    list(first = first, second = second)
  })

  expect_identical(samples$first, samples$second)
  expect_length(samples$first, 200)
  expect_true(all(samples$first %in% 0:12))
})
