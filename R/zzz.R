#' @import checkmate
#' @import lgr
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom extraDistr dbbinom
#' @importFrom stats dbinom
#' @importFrom stats dnorm
#' @importFrom stats rbinom
#' @importFrom stats reshape
#' @importFrom stats rnorm
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  # setup logger
  lg = lgr::get_logger(pkgname)
  lg$set_threshold("info")
  assign("lg", lg, envir = parent.env(environment()))
  f = function(event) {
    event$msg = paste0("[scerrModel] ", event$msg)
    TRUE
  }
  lg$set_filters(list(f))
}
