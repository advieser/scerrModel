plan(multisession, workers = 8)
microbenchmark::microbenchmark(
  sequential = {
    set.seed(9999)
    res <- vector("list", n_runs)
    res <- setNames(res, paste0("study", seq_len(n_runs)))
    for (i in seq_len(n_runs)) {
      args <- as.list(inparams[i, ])
      res[[i]] <- do.call(run_model, args)
    }
  },
  parallel = {
    set.seed(9999)
    res <- vector("list", n_runs)
    res <- setNames(res, paste0("study", seq_len(n_runs)))
    res <- future.apply::future_lapply(seq_len(n_runs), function(i) {
      args <- as.list(inparams[i, ])
      do.call(run_model, args)
    }, future.seed = TRUE)
  }
)
