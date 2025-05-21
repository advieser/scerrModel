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


# used for do.call calls in simulate_literature()
get_params <- function(df, row, cols) {
  if (!all(cols %in% names(df))) {
    stop("One or more columns not found in the data frame.")
  }
  if (row < 1 || row > nrow(df)) {
    stop("Row index out of bounds.")
  }
  # Extract values and name them with the column names
  setNames(as.list(df[row, cols, drop = FALSE]), cols)
}
