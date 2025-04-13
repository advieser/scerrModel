dbetabinom <- function(k, N, alpha, beta) {
  exp(lchoose(N, k) + lbeta(k + alpha, N - k + beta) - lbeta(alpha, beta))
}

rbetabinom <- function(nn, N, alpha, beta) {
  vals = seq(0, N)
  prob = dbetabinom(vals, N, alpha, beta)
  sample(vals, size = nn, replace = TRUE, prob = prob)
}
