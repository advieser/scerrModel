dbetabinom <- function(candidate_counts, N, alpha, beta, log = FALSE) {
  log_density <- lchoose(N, candidate_counts) +
    lbeta(candidate_counts + alpha, N - candidate_counts + beta) -
    lbeta(alpha, beta)

  if (log) log_density else exp(log_density)
}

rbetabinom <- function(n, N, alpha, beta) {
  support <- seq(0, N)
  probs <- dbetabinom(support, N, alpha, beta)
  sample(support, size = n, replace = TRUE, prob = probs)
}
