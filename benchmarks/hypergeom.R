# Hypergeometric probability for drawing b black balls in m draws given total k black balls:
H_func <- function(m, k, b, N) {
  if (k < b) return(0)
  return(choose(k, b) * choose(N - k, m - b) / choose(N, m))
}

N <- 10
for (i in seq_len(N)) {
  for (b in seq_len(i)) {
    for (k in b:N) {
      if (!isTRUE(all.equal(
        dhyper(b, k, N - k, i),
        H_func(i, k, b, N)
      ))) {
        stop ("möp")
      }
    }
  }
}
