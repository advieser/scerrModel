N = 1000
# with large N = 100000, custom solution breaks as early as k = 89
Q = seq(0, N)
alpha0 = 3
beta0 = 5
ngen = 10000

# Random generation
microbenchmark::microbenchmark(
  custom = {
    rbetabinom(nn = ngen, N = N, alpha = alpha0, beta = beta0)
  },
  lib = {
    extraDistr::rbbinom(n = ngen, size = N, alpha = alpha0, beta = beta0)
  },
  VGAM = {
    VGAM::rbetabinom.ab(n = ngen, size = N, shape1 = alpha0, shape2 = beta0)
  },
  EMD = {
    emdbook::rbetabinom(n = ngen, size = N, shape1 = alpha0, shape2 = beta0)
  }
  # rmutil = {  # it's so bad
  #   rmutil::rbetabinom(ngen, size = N, m = alpha0 / (alpha0 + beta0), s = alpha0 + beta0)
  # }
)

# PMF generation
microbenchmark::microbenchmark(
  custom = {
    dbetabinom(k = Q, N = N, alpha = alpha0, beta = beta0)
  },
  lib = {
    extraDistr::dbbinom(x = Q, size = N, alpha = alpha0, beta = beta0)
  },
  VGAM = {
    VGAM::dbetabinom.ab(x = Q, size = N, shape1 = alpha0, shape2 = beta0)
  },
  EMD = {
    emdbook::dbetabinom(x = Q, size = N, shape1 = alpha0, shape2 = beta0)
  },
  rmutil = {
    rmutil::dbetabinom(y = Q, size = N, m = alpha0 / (alpha0 + beta0), s = alpha0 + beta0)
  }
)
