#' Title
#'
#' @param x  a difftime object
#' @param sql set to TRUE if you're working with remote tables and using dbplyr
#'
#' @return a difftime object in seconds

as_seconds <- function(x, sql = FALSE) {
  # If given as a number, units = "secs" is ignored
  ret <- as.double(x, units = "secs")

  ret
}
