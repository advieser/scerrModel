require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("The %s package is needed for this function to work. Please install it.", package), call. = FALSE)
  }
}

generate_subtitles <- function(literature) {
  # Format each group
  format_group <- function(group) {
    paste(names(group), format(group, digits = 3), sep = " = ", collapse = ", ")
  }

  lapply(seq_along(literature), function(i) {
    params <- unlist(literature[[i]]$params)
    # Group keys
    general <- params[!grepl("^(obj|sbj)_", names(params))]
    obj     <- params[grepl("^obj_", names(params))]
    sbj     <- params[grepl("^sbj_", names(params))]

    paste0(
      "<b>STUDY ", names(literature)[i], "</b>: ",
      format_group(general),
      "<br><b>Objective:</b> ", format_group(obj),
      "<br><b>Subjective:</b> ", format_group(sbj)
    )
  })
}


#' Plot Posterior for Number of Faults across Rounds as Heatmap
#'
#' Plots the discrete probability distributions simulated through [simulate_literature()] per round as a heatmap.
#'
#' @examples
#' # only want a few? index literature
#'
#' @export
plot_posteriors <- function(literature) {
  require_package("ggplot2")
  require_package("ggtext")
  assert_literature(literature)

  # Extract posteriors from simulation result and modify format to be compatible with ggplot
  posteriors <- lapply(literature, function(study) {
    reshape2::melt(study$history$posterior_K, varnames = c("round", "K"), value.name = "prob")
  })

  # Generate informative subtitles
  subtitles <- generate_subtitles(literature)

  plots <- vector("list", length(posteriors))
  for (study in seq_along(posteriors)) {
    plots[[study]] <- ggplot2::ggplot(posteriors[[study]], ggplot2::aes(x = round, y = K, fill = prob)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(
        colours = c("white", "lightblue", "blue", "darkblue"),
        values = scales::rescale(c(0, 0.02, 0.4, 1)),
        limits = c(0, 1),
        na.value = "lightgray"
      ) +
      ggplot2::coord_fixed() +  # Ensures square tiles
      ggplot2::labs(
        title = "Posterior for number of faults across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Number of faults",
        fill = "Probability"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        # Ensure coordinate system is visible above tiles
        panel.ontop = TRUE,
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_line(color = "lightgray"),
        # Allow markdown formatting in labs
        plot.title = ggtext::element_markdown(size = 16),
        plot.subtitle = ggtext::element_markdown(size = 12)
      )
  }
  plots
}

#' Plot Belief for Finding a Fault across Rounds
#'
#' @export
plot_fault_beliefs <- function(literature) {
  # TODO: add legend for cbr dashed line
  require_package("ggplot2")
  require_package("ggtext")
  assert_literature(literature)

  # Extract beliefs for finding a fault across rounds from simulation result
  fault_beliefs <- lapply(literature, function(study) {
    belief = study$history$fault_belief
    data.frame(round = seq_along(belief), fault_belief = belief)
  })

  # Extract cost-benefit-ratios per study
  cbrs <- lapply(literature, function(study) {
    params <- study$params
    params$cost / params$benefit
  })

  # Generate informative subtitles
  subtitles <- generate_subtitles(literature)

  plots <- vector("list", length(fault_beliefs))
  for (study in seq_along(fault_beliefs)) {
    plots[[study]] <- ggplot2::ggplot(fault_beliefs[[study]], ggplot2::aes(x = round, y = fault_belief)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = cbrs[[study]], linetype = "dashed") +
      ggplot2::labs(
        title = "Belief for finding a fault across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Belief"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        # Allow markdown formatting in labs
        plot.title = ggtext::element_markdown(size = 16),
        plot.subtitle = ggtext::element_markdown(size = 12)
      )
  }
  plots
}

#' Plot Fault Discovery across Rounds
#'
#' @export
plot_fault_discovery <- function(literature) {
  # TODO: Differentiate between reached and unreached (lightgray) rounds + make sure that y axis is always up to true_num_faults
  require_package("ggplot2")
  require_package("ggtext")
  assert_literature(literature)

  subtitles <- generate_subtitles(literature)

  # Generate data.frame with number of faults discovered and undiscovered across round per study
  discovery <- lapply(literature, function(study) {
    reality <- study$objective_reality

    true_num_faults <- reality$true_K
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
    plots[[study]] <- ggplot2::ggplot(discovery[[study]], ggplot2::aes(x = round, y = count, fill = status)) +
      ggplot2::geom_bar(stat = "identity", width = 1) +
      ggplot2::scale_fill_manual(
        values = c("num_undiscovered" = "darkred", "num_discovered" = "darkgreen"),
        labels = c("undiscovered", "discovered"),
        name = "Fault Status"
      ) +
      ggplot2::labs(
        title = "Number of faults discovered across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Belief"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        # Ensure coordinate system is visible above bars
        # panel.ontop = TRUE,
        # panel.background = ggplot2::element_blank(),
        # panel.grid = element_line(color = "lightgray"),
        # Allow markdown formatting in labs
        plot.title = ggtext::element_markdown(size = 16),
        plot.subtitle = ggtext::element_markdown(size = 12)
      )
  }
  plots
}

#' Plot Observed Effect Sizes across Rounds
#'
#' @export
plot_obsereved_effect_sizes <- function(literature) {
  require_package("ggplot2")
  require_package("ggtext")
  assert_literature(literature)

  subtitles <- generate_subtitles(literature)

  # Extract effect sizes observed across rounds per study
  observed_effect_sizes <- lapply(literature, function(study) {
    error_sizes <- study$objective_reality$error_sizes
    true_effect <- study$objective_reality$true_effect
    observed_effect_sizes <- true_effect + rev(cumsum(error_sizes))
    data.frame(
      round = seq_along(observed_effect_sizes),
      effect_sizes = observed_effect_sizes
    )
  })

  true_effects <- lapply(literature, function(study) study$objective_reality$true_effect)

  plots <- vector("list", length(observed_effect_sizes))
  for (study in seq_along(observed_effect_sizes)) {
    plots[[study]] <- ggplot2::ggplot(observed_effect_sizes[[study]], ggplot2::aes(x = round, y = effect_sizes)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = true_effects[[study]], linetype = "dashed") +
      ggplot2::labs(
        title = "Effect size observed across rounds",
        subtitle = subtitles[[study]],
        x = "Rounds",
        y = "Effect Size"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        # Allow markdown formatting in labs
        plot.title = ggtext::element_markdown(size = 16),
        plot.subtitle = ggtext::element_markdown(size = 12)
      )
  }
  plots
}

#' Plot Summary Panel per Study
#'
#' @export
plot_study_panels <- function(literature) {
  require_package("ggplot2")
  require_package("ggtext")
  require_package("patchwork")
  assert_literature(literature)

  posterior_plots <- plot_posteriors(literature)
  belief_plots <- plot_fault_beliefs(literature)
  discovery_plots <- plot_fault_discovery(literature)
  effect_size_plots <- plot_obsereved_effect_sizes(literature)

  # Re-generate informative subtitle (for all plots)
  titles <- generate_subtitles(literature)

  plots <- vector("list", length(res))
  for (study in seq_along(res)) {
    posterior_plot <- posterior_plots[[study]] + ggplot2::labs(subtitle = NULL)
    belief_plot <- belief_plots[[study]] + ggplot2::labs(subtitle = NULL)
    discovery_plot <- discovery_plots[[study]] + ggplot2::labs(subtitle = NULL)
    effect_size_plot <- effect_size_plots[[study]] + ggplot2::labs(subtitle = NULL)

    combined_plots <- patchwork::wrap_plots(
      posterior_plot,
      patchwork::wrap_plots(belief_plot, discovery_plot, effect_size_plot, ncol = 1),
      ncol = 2
    )
    plots[[study]] <- combined_plots +
      patchwork::plot_annotation(title = titles[[study]]) &
      ggplot2::theme(plot.title = ggtext::element_markdown())
  }
  plots
}


plot_histogram_final_effect_sizes <- function(literature, binwidth = 0.3) {
  require_package("ggplot2")

  assert_literature(literature)
  assert_number(binwidth, lower = 0, null.ok = TRUE)

  # Extract vectors of final observed effect sizes
  final_effect_sizes <- data.frame(
    final_effect_size = sapply(literature, function(x) x$stop_conditions$final_effect_size)
  )

  ggplot2::ggplot(final_effect_sizes, ggplot2::aes(x = final_effect_size)) +
    ggplot2::geom_histogram(binwidth = binwidth, color = "black", fill = "gray") +
    ggplot2::labs(title = "Histogram of Final Observed Effect Sizes",
                  x = "Final observed effect size",
                  y = "Frequency") +
    ggplot2::theme_bw()
}

plot_histogramm_faults <- function() NULL

# bar plot of stopping reasons
plot_reasons <- function() NULL

# reached rounds
plot_histogramm_rounds <- function() NULL

plot_literature_panel <- function() NULL

