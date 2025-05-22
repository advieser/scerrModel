devtools::load_all()
library(mlr3misc)
library(ggplot2)
library(ggtext)  # for markdown labs
library(patchwork)

# -------------------------------------------------------------------- #
# ----------------------- FUNCTION DEFINITIONS ----------------------- #
# -------------------------------------------------------------------- #
plot_posteriors <- function(literature) {
  # Extract basic information of the simulated study
  params <- map(literature, function(x) unlist(x$params))
  # Generate informative subtitles
  subtitles <- imap(params, function(ll, nn) {
    paste0("<b>", nn, "</b>: ", paste(names(ll), ll, sep = " = ", collapse = ", "))
  })

  # Extract posteriors from simulation result
  posteriors <- map(literature, function(study) study$history$posterior_K)
  # Modify format to be compatible with ggplot
  # posteriors <- map(posteriors, function(post) setNames(reshape2::melt(post), c("round", "K", "prob")))
  posteriors <- map(posteriors, function(post) reshape2::melt(post, varnames = c("round", "K"), value.name = "prob"))
  # TODO: simplify into one map call

  plots <- vector("list", length(posteriors))
  for (study in seq_along(posteriors)) {
    plots[[study]] <- ggplot(posteriors[[study]], aes(x = round, y = K, fill = prob)) +
      geom_tile() +
      scale_fill_gradientn(
        colours = c("white", "lightblue", "blue", "darkblue"),
        values = scales::rescale(c(0, 0.02, 0.4, 1)),
        limits = c(0, 1),
        na.value = "lightgray"
      ) +
      coord_fixed() +  # Ensures square tiles
      labs(
        title = "Posterior for number of faults across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Number of faults",
        fill = "Probability"
      ) +
      theme_bw() +
      theme(
        # Ensure coordinate system is visible above tiles
        panel.ontop = TRUE,
        panel.background = element_blank(),
        panel.grid = element_line(color = "lightgray"),
        # Allow markdown formatting in labs
        plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 12)
      )
  }
  plots
}

# TODO: add legend for cbr dashed line
plot_fault_beliefs <- function(literature) {
  # Extract basic information of the simulated study
  params <- map(literature, function(x) unlist(x$params))
  # Generate informative subtitles
  subtitles <- imap(params, function(ll, nn) {
    paste0("<b>", nn, "</b>: ", paste(names(ll), ll, sep = " = ", collapse = ", "))
  })

  # Extract beliefs for finding a fault across rounds from simulation result
  fault_beliefs <- map(literature, function(study) study$history$fault_belief)
  # Generate data.frame to be compatible with ggplot
  fault_beliefs <- map(fault_beliefs, function(study) data.frame(round = seq_along(study), fault_belief = study))
  # TODO: simplify into one map call

  # Extract cost-benefit-ratios per study
  cbrs <- map(literature, function(study) {
    rm <- study$resource_management
    rm$cost / rm$benefit
  })

  plots <- vector("list", length(fault_beliefs))
  for (study in seq_along(fault_beliefs)) {
    plots[[study]] <- ggplot(fault_beliefs[[study]], aes(x = round, y = fault_belief)) +
      geom_line() +
      geom_hline(aes(yintercept = cbrs[[study]]), linetype = "dashed") +
      labs(
        title = "Belief for finding a fault across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Belief"
      ) +
      theme_bw() +
      theme(
        # Allow markdown formatting in labs
        plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 12)
      )
  }
  plots
}

# TODO: Differentiate between reached and unreached (lightgray) rounds + make sure that y axis is always up to true_num_faults
plot_fault_discovery <- function(literature) {
  # Extract basic information of the simulated study
  params <- map(literature, function(x) unlist(x$params))
  # Generate informative subtitles
  subtitles <- imap(params, function(ll, nn) {
    paste0("<b>", nn, "</b>: ", paste(names(ll), ll, sep = " = ", collapse = ", "))
  })

  # Generate data.frame with number of faults discovered and undiscovered across round per study
  discovery <- map(literature, function(study) {
    reality <- study$objective_reality

    true_num_faults <- reality$true_num_faults
    num_discovered <- cumsum(reality$error_sizes != 0)
    num_undiscovered <- true_num_faults - num_discovered

    df <- data.frame(
      round = seq_along(num_discovered),
      num_discovered = num_discovered,
      num_undiscovered = num_undiscovered
    )
    reshape2::melt(df, id.vars = "round", measure.vars = c("num_undiscovered", "num_discovered"),  # vars order important
                   variable.name = "status", value.name = "count")
  })

  plots <- vector("list", length(discovery))
  for (study in seq_along(discovery)) {
    plots[[study]] <- ggplot(discovery[[study]], aes(x = round, y = count, fill = status)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(
        values = c("num_undiscovered" = "darkred", "num_discovered" = "darkgreen"),
        labels = c("undiscovered", "discovered"),
        name = "Fault Status"
      ) +
      labs(
        title = "Number of faults discovered across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Belief"
      ) +
      theme_bw() +
      theme(
        # Ensure coordinate system is visible above bars
        # panel.ontop = TRUE,
        # panel.background = element_blank(),
        # panel.grid = element_line(color = "lightgray"),
        # Allow markdown formatting in labs
        plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 12)
      )
  }
  plots
}

plot_total_errors <- function(literature) {
  # Extract basic information of the simulated study
  params <- map(literature, function(x) unlist(x$params))
  # Generate informative subtitles
  subtitles <- imap(params, function(ll, nn) {
    paste0("<b>", nn, "</b>: ", paste(names(ll), ll, sep = " = ", collapse = ", "))
  })

  # Extract total error sizes observed across rounds per study
  total_error_sizes <- map(literature, function(study) {
    error_sizes <- study$objective_reality$error_sizes
    total_error_sizes <- rev(cumsum(error_sizes))
    data.frame(
      round = seq_along(total_error_sizes),
      total_error_size = total_error_sizes
    )
  })

  plots <- vector("list", length(total_error_sizes))
  for (study in seq_along(total_error_sizes)) {
    plots[[study]] <- ggplot(total_error_sizes[[study]], aes(x = round, y = total_error_size)) +
      geom_line() +
      geom_hline(aes(yintercept = 0), linetype = "dashed") +
      labs(
        title = "Total error size observed across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Total error size"
      ) +
      theme_bw() +
      theme(
        # Allow markdown formatting in labs
        plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 12)
      )
  }
  plots
}

plot_panel <- function(literature) {
  posterior_plots <- plot_posteriors(literature)
  belief_plots <- plot_fault_beliefs(literature)
  discovery_plots <- plot_fault_discovery(literature)
  total_error_plots <- plot_total_errors(literature)

  # Re-generate informative subtitle (for all plots)
  params <- map(literature, function(x) unlist(x$params))
  titles <- imap(params, function(ll, nn) {
    paste0("<b>", nn, "</b>: ", paste(names(ll), ll, sep = " = ", collapse = ", "))
  })

  plots <- vector("list", length(res))
  for (study in seq_along(res)) {
    posterior_plot <- posterior_plots[[study]] + labs(subtitle = NULL)
    belief_plot <- belief_plots[[study]] + labs(subtitle = NULL)
    discovery_plot <- discovery_plots[[study]] + labs(subtitle = NULL)
    total_error_plot <- total_error_plots[[study]] + labs(subtitle = NULL)

    plots[[study]] <- (posterior_plot | (belief_plot / discovery_plot / total_error_plot)) +
      plot_annotation(title = titles[[study]]) &
      theme(plot.title = ggtext::element_markdown())
  }
  plots
}

# ----------------------- SIMULATION ----------------------- #
inparams <- data.frame(
  N = rep(seq(100, 1000, by = 100), each = 1),
  sbj_prob_alpha = 2, sbj_prob_beta = 2,
  sbj_mean_mu = 0, sbj_mean_kappa = 1,
  sbj_var_alpha = 2, sbj_var_beta = 2,
  benefit = 0.5, cost = 0.1, resources = 100
)
n_runs <- nrow(inparams)

set.seed(9999)
res <- vector("list", n_runs)
res <- setNames(res, paste0("study", seq_len(n_runs)))
for (i in seq_len(n_runs)) {
  args <- as.list(inparams[i, ])
  res[[i]] <- do.call(run_model, args)
}

agents <- create_agents(
  sbj_prob_alpha = 2,
  sbj_prob_beta = 3,
  sbj_mean_mu = 0,
  sbj_mean_kappa = 1,
  sbj_var_beta = 2,
  sbj_var_alpha = 3
)
studies <- create_studies(
  N = seq(50, 500, by = 50),
  resources = 1000,
  cost = 5,
  benefit = 10,
  obj_prob_fault = 0.15,
  obj_error_size_mu = 0,
  obj_error_size_sigma = 0.3,
  agent_id = 1
)

set.seed(1234)
res <- simulate_literature(agents = agents, studies = studies)

# ----------------------- PLOT RESULTS ----------------------- #
panels <- plot_panel(res)
panels
