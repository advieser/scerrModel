% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_prob_fault_this_round}
\alias{plot_prob_fault_this_round}
\title{Plot Probability of Finding a Fault per Round}
\usage{
plot_prob_fault_this_round(literature, study_id = NULL, cbr = TRUE)
}
\arguments{
\item{literature}{(\code{list()})\cr
A list containing the simulated literature, generated by \code{\link[=simulate_literature]{simulate_literature()}}.}

\item{study_id}{(\code{character()} or \code{integer()})\cr
A vector of (possibly multiple) unique identifiers of studies indicating for which studies the plot should be generated.
If \code{NULL}, all studies in \code{literature} are plotted. Default is \code{NULL}.}

\item{cbr}{(\code{logical()})\cr
If \code{TRUE}, the cost-benefit-ratios will be plotted additionally. Default is \code{TRUE}.}
}
\value{
A \code{ggplot} object that can be further customized using the \code{ggplot2} package.
}
\description{
Plot Probability of Finding a Fault per Round
}
\details{
The cost-benefit-ratios can be plotted as dashed lines in the same plot as the probability of finding a fault in the current round.
If this probability is smaller than the cost-benefit-ratio, the agent will stop searching.
}
\examples{
if (requireNamespace(c("ggplot2", "patchwork"), quietly = TRUE)) {
library(ggplot2)
library(patchwork)

# Create study and agent
agent <- create_agents(
  agent_id = "Alice",
  subj_effect_mu = 0.3, subj_effect_sigma = 1.2,
  subj_prob_fault_alpha = 0.2, subj_prob_fault_beta = 0.4,
  subj_error_size_mu = 0.6, subj_error_size_sigma = 0.2
)
study <- create_studies(
  study_id = "Alice2022", agent_id = "Alice",
  N = 30, resources = 1000, cost = 400, benefit = 20,
  obj_effect_mu = 0.4, obj_effect_sigma = 0.5,
  obj_prob_fault = 0.4,
  obj_error_size_mu = 0.1, obj_error_size_sigma = 0.2
)
# Simulate the study
lit <- simulate_literature(agents = agent, studies = study, seed = 123)
# Plot the probability of finding a fault per round
plot_prob_fault_this_round(lit)
}

}
