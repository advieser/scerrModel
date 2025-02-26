# Print current round
print_round <- function(round) {
  cat("\n----", "Round:", round, "----\n")
}

# Print which number of rem faults gets assigned the highest likelihood
print_first_step <- function(effect_size, lk) {
  cat("First Updating Step\n")
  cat("Observed effect size:", round(effect_size, 2), "\n")
  cat("Most likelihood assigned to", which.max(lk) - 1, "remaining faults in first updating step.\n")
}

plot_first_likelihood <- function(subj_effect_mu, subj_effect_sigma, subj_error_size_mu, subj_error_size_sigma, obs_effect, round, N, xlim = c(-1, 1), ylim = c(0, 5)) {
  means <- subj_effect_mu + seq(0, N) * subj_error_size_mu
  sds <- sqrt(subj_effect_sigma^2 + seq(0, N) * subj_error_size_sigma^2)

  colors <- rainbow(length(means))
  labels <- paste("rem. faults:", seq(0, N))

  plot(NULL, main = paste("First likelihood, Round:", round),
       xlim = xlim, ylim = ylim,
       xlab = "observed error size", ylab = "Density")

  x_vals = seq(xlim[[1]], xlim[[2]], length.out = 100)
  for (i in seq_along(means)) {
    y_vals = dnorm(x_vals, mean = means[i], sd = sds[i])
    lines(x_vals, y_vals, lwd = 2, col = colors[[i]])
  }

  abline(v = obs_effect, col = "black", lty = 2, lwd = 2)

  legend("topleft", legend = labels, col = colors, lwd = 2)
  # Interpretation: See what distribution (with which parameters, where larger absolute parameters mean more)
}

plot_first_distribution <- function(distribution, round, N) {
  x_labels <- seq(0, N)

  barplot(
    height = distribution,
    names.arg = x_labels,
    col = "skyblue",
    border = "black",
    xlab = "remaining faults",
    ylab = "Probability",
    main = paste("Distribution after observing effect, Round:", round)
  )
}

print_decision <- function(belief, cost, benefit, resources) {
  cat("\nAgent Decision\n")
  cat("Resources:", resources, "   ", "Cost:", cost, "   ", "Benefit:", benefit, "\n")
  cat("Cost-to-benefits-ratio:", round(cost / benefit, 2), "\n")
  cat("Belief about finding a fault in this round:", round(belief, 2), "\n")
  decision <- if (belief < cost / benefit) {
    TRUE
  } else if (!cost > resources) {
    TRUE
  } else {
    FALSE
  }
  decision <- if (decision) "Continue searching" else "Stop searching"
  cat("Decision:", decision, "\n")
}

print_second_step <- function(fault_ind) {
  cat("\nSecond Updating Step\n")
  cat("Fault detected:", fault_ind, "\n")
}

plot_second_likelihood <- function(likelihood, round, N) {
  values_matrix = matrix(data = c(likelihood, 1 - likelihood), nrow = 2)

  barplot(
    values_matrix,
    beside = FALSE,
    col = c("lightgreen", "pink"),
    border = "black",
    names.arg = seq(0, N),
    xlab = "remaining faults",
    ylab = "Bernoulli Distribution",
    main = paste("Second likelihood, Round:", round),
    legend.text = rownames(values_matrix)
  )
}

plot_second_distribution <- function() {

}
