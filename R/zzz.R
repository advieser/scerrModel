#' @import checkmate
#' @import lgr
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats dhyper
#' @importFrom stats dnorm
#' @importFrom stats rnorm
#' @importFrom stats setNames
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

globalVariables(c(
  "K", "count", "effect_sizes", "next_fault_belief", "final_effect_size",
  "prob", "status"
))
