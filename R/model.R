# is faster than extraDistr::dbbinom() according to benchmark
dbetabinom <- function(k, N, alpha, beta) {
  choose(N, k) * beta(k + alpha, N - k + beta) / beta(alpha, beta)
}
rbetabinom <- function(nn, N, alpha, beta) {
  vals = seq(0, N)
  prob = dbetabinom(vals, N, alpha, beta)
  sample(vals, size = nn, replace = TRUE, prob = prob)
}
# but maybe numerically less stable for large N? Yes, breaks for large N
# check: replace custom function with others and see whether there is a systematic difference in model behavior
#        EMD seems best for this since it is most efficient and uses log internally
# posible improvement: calculate PMF in log space to avoid overflow

# Hypergeometric probability for drawing b black balls in m draws given total k black balls:
H_func <- function(m, k, b, N) {
  if (k < b) return(0)
  return(choose(k, b) * choose(N - k, m - b) / choose(N, m))
}

# Aggregated likelihood for the remaining (undrawn) black balls:
# If r = k - b > 0, then the sum of r independent N(mu_fixed, sigma_fixed^2) variables is
# distributed as N(r*mu_fixed, r*sigma_fixed^2). (If r=0, then S_current must be 0.)
# TODO: rename variables
aggregated_likelihood <- function(S_val, k, b, mu_fixed, sigma_fixed) {
  r <- k - b
  if (r == 0) {
    return(ifelse(abs(S_val) < 1e-8, 1, 0))
  } else {
    return(dnorm(S_val, mean = r * mu_fixed, sd = sqrt(r) * sigma_fixed))
  }
}


# Prior for K = beta-binomial

# N: total number of balls
# Prior for K (no. of black balls):
# * alpha0
# * beta0
# Distribution of black ball labels:
# * mu
# * sigma
run_model <- function(N, alpha0, beta0, mu, sigma,
                      benefit = 5, cost = 1, resources = 100) {
  # Vector of possible number of faults
  K_vals <- seq(0, N)

  # Prior for number of faults
  K_prior <- dbetabinom(K_vals, N, alpha0, beta0)

  # Simulate true number of faults
  # TODO: Separate this from subjective prior given by alpha0 and beta0.
  #       Probably best to replace with Bernoulli distribution like we had in the first model.
  true_K <- rbetabinom(1, N, alpha0, beta0)

  # Simulate true error sizes
  # TODO:Separate this from subjective parameters
  black_labels <- rnorm(true_K, mean = mu, sd = sigma)

  # Create urn: black = fault, white = no_fault
  urn <- c(rep("no_fault", N - true_K), rep("fault", true_K))[sample(N)]

  # Generate vector of error sizes per round (i.e. ball labels)
  ball_labels <- rep(NA, N)
  # alt: numeric(N), why not 0 instead of NA?
  ball_labels[which(urn == "fault")] <- black_labels

  # ----------------------------------------- #
  # Agent Model ----------------------------- #
  # TODO: Add logging.
  total_error_size <- sum(black_labels)
  b <- 0  # number of drawn black balls
  drawn_black <- numeric(0)  # observed labels for drawn black balls

  # We'll record the posterior over K after each draw.
  posterior_K_history <- matrix(nrow = N + 1, ncol = N + 1)
  # At time 0, before any draws, the posterior is the prior:
  posterior_K_history[1, ] <- K_prior

  # Pre-allocate and initialize variables
  init_resources <- resources
  belief_fault_this_round <- setNames(numeric(N+1), K_vals)
  eu_criterion <- setNames(numeric(N+1), K_vals)

  for (i in seq_len(N + 1)) {  # +1 to save last decision parameters
    # Make decision
    belief_fault_this_round[[i]] <- sum(posterior_K_history[i, ] * seq(0, N)) / (N - i + 1)
    eu_criterion[[i]] <- belief_fault_this_round[[i]] * benefit / cost
    # FIXME: belief can get above 1

    if (eu_criterion[[i]] < 1) {
      stopped_in_round <- i - 1  # -1 since we start with prior before first round
      stopping_reason <- "Expected utility too low"
      break
    } else if (resources < cost) {
      stopped_in_round <- i - 1
      stopping_reason <- "Resources depleted"
      break
    } else if (i == N + 1) {
      stopped_in_round = i - 1
      stopping_reason = "Search completed"
      break
    } else {
      # Otherwise, update resources and continue searching
      resources <- resources - cost
    }

    # Start observation
    ball_type <- urn[i]
    # Update information if a fault is observed
    if (ball_type == "fault") {
      b <- b + 1
      error_size <- ball_labels[[i]]
      drawn_black[[length(drawn_black) + 1]] <- error_size
      total_error_size <- total_error_size - error_size
    }

    # For candidate k from b to N, compute an (unnormalized) posterior weight
    posterior_K <- numeric(length(K_vals))

    for (k in seq(b, N)) {  # TODO: check that these indices are conceptually correct
      # Hypergeometric probability for drawing b black balls in i draws given total k black balls of N total balls:
      # I.e. what is the probability of b faults in i rounds, for N balls of which k are black (k being what we assume for this element of the posterior)
      H_val <- dhyper(b, k, N - k, i)
      # TODO: move this out of the loop and vectorize

      # Likelihood for the drawn black ball labels:
      # TODO: extract into its own function for simplification
      if (b > 0) {
        drawn_ll <- prod(dnorm(drawn_black, mean = mu, sd = sigma))
      } else {
        drawn_ll <- 1
      }

      # Likelihood for the remaining (undrawn) black balls (their total S_current)
      agg_ll <- aggregated_likelihood(total_error_size, k, b, mu, sigma)
      # TODO: understand this

      # Combined weight (prior × hypergeometric likelihood × drawn data likelihood × aggregated likelihood)
      posterior_K[k + 1] <- K_prior[k + 1] * H_val * drawn_ll * agg_ll
      # Note: We index with k+1 because k ranges from 0 to N.
      # TODO: understand the underlying model (hierarchical?)
      #       Does it make sense to always use the prior from the first round?
      # TODO: Can this all be vectorized?
    }

    # Normalize the posterior over K:
    posterior_K_history[i + 1, ] <- posterior_K / sum(posterior_K)
  }

  list(
    params = list(N = N, alpha0 = alpha0, beta0 = beta0, mu = mu, sigma = sigma),
    stop_conditions = list(stopped_in_round = stopped_in_round, stopping_reason = stopping_reason),
    ressource_management = list(
      init_resources = init_resources,
      last_resources = resources,
      cost = cost,
      benefit = benefit
    ),
    history = list(
      posterior_K = posterior_K_history,
      belief_fault_this_round = belief_fault_this_round,
      eu_criterion = eu_criterion
    )
  )
}

sim_res <- run_model(100, 1, 2, 0, 1, benefit = 100, resources = 10000)


df <- reshape2::melt(sim_res$history$posterior_K)
colnames(df) <- c("round", "K", "prob")

# Plot using ggplot
library(ggplot2)
ggplot(df, aes(x = round, y = K, fill = prob)) +
  geom_tile() +
  scale_fill_gradient(low = "grey", high = "blue") +
  theme_minimal() +
  coord_fixed() +  # Ensures square tiles
  theme_bw()
# should be impossible, agent should not believe that 30 faults remain in round 100



#### 5. Plot the Final Posterior over K
final_post <- posterior_K_history[[length(posterior_K_history)]]
barplot(final_post, main = "Final Posterior Probability for K",
        xlab = "K", ylab = "Probability")

#### 6. (Optional) Plot the Evolution of Posterior Mass on the True K

posterior_trueK <- sapply(posterior_K_history, function(p) p[as.character(true_K)])
plot(0:(length(posterior_K_history) - 1), posterior_trueK,
     type = "b", col = "blue", pch = 16,
     xlab = "Draw Number", ylab = "Posterior Mass on True K",
     main = "Evolution of Posterior Mass on True K")


# 1. Combine the list of posterior vectors into a single matrix.
#    Each row will correspond to one iteration (time = 0, 1, ..., N),
#    and columns correspond to K=0,...,N.
posteriorMatrix <- do.call(rbind, posterior_K_history)
# posteriorMatrix will have dimension (N+1) x (N+1).

# 2. Plot using matplot. We transpose because matplot expects one curve per column by default.
#    We'll label the x-axis as K, and the y-axis as posterior probability.
matplot(K_vals, t(posteriorMatrix), type = "l",
        xlab = "K",
        ylab = "Posterior Probability",
        lty  = 1,
        col  = rainbow(nrow(posteriorMatrix)),
        main = "Posterior over K, across iterations")

# 3. Add a legend labeling the iteration number for each curve.
legend("topright",
       legend = paste("Iteration", 0:(length(posterior_K_history) - 1)),
       col    = rainbow(nrow(posteriorMatrix)),
       lty    = 1,
       cex    = 0.7)


iteration_vals <- 0:N   # will be x-axis
K_vals        <- 0:N   # will be y-axis

# For a color scale, you can try heat.colors(), topo.colors(), or a custom palette:
myColors <- heat.colors(100)  # or e.g.: colorRampPalette(c("white","blue"))(50)

image(x      = iteration_vals,
      y      = K_vals,
      z      = posteriorMatrix,  # transpose so row->K, col->iteration
      col    = myColors,
      xlab   = "Iteration",
      ylab   = "K",
      main   = "Posterior Probability Heatmap")
