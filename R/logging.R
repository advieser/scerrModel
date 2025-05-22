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

log_start <- function(study_id, agent_id, N) {
  lg$info(emph("STARTING SIMULATION of STUDY '%s' run by AGENT '%s' with %s rounds.", bold = TRUE),
          emph(study_id, "blue"),
          emph(agent_id, "blue"),
          emph(sprintf("N = %i", N), "blue"))
}

log_stop <- function (study_id, i, stopping_reason) {
  lg$info("['%s'] [%s] %s: %s (%s)",
          emph(study_id, "blue", bold = TRUE),
          emph(sprintf("N = %i", i), bold = TRUE),
          emph("DECISION", bold = TRUE),
          emph("stopped searching", "red", bold = TRUE),
          emph(stopping_reason, "red"))
}

log_complete <- function(study_id) {
  lg$info("['%s'] [%s] %s",
          emph(study_id, "blue", bold = TRUE),
          emph(sprintf("N = %i", i), bold = TRUE),
          emph("SEARCH COMPLETED", "green", bold = TRUE))
}

emph <- function(text, color = NULL, bold = FALSE) {
  color_codes <- c(
    red = "31", green = "32", yellow = "33", blue = "34",
    magenta = "35", cyan = "36", gray = "90", default = "39"
  )

  start <- ""
  end <- ""
  if (!is.null(color)) {
    start <- paste0(start, "\033[", color_codes[[color]], "m")
    end <- paste0("\033[39m", end)  # reset just color
  }
  if (bold) {
    start <- paste0("\033[1m", start)
    end <- paste0(end, "\033[22m")  # reset bold
  }

  paste0(start, text, end)
}
