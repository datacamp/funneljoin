#' Title
#'
#' @param .data
#' @param time_col the name of the time column
#' @param user_col the name of the user identifying column
#' @param type the type of after_join ("first-first", "first-firstafter", etc.)
#'
#' @return
#'
#' @examples
distinct_events <- function(.data, time_col, user_col, type) {

  if (type == "first") {
    data_sorted <- .data %>%
      arrange(!!sym(time_col))
  } else if (type == "last") {
    data_sorted <- .data %>%
      arrange(desc(!!sym(time_col)))
  }

  if (inherits(.data, "tbl_lazy")) {
    ret <- data_sorted %>%
      group_by(!!sym(user_col)) %>%
      mutate(..rank = row_number()) %>%
      ungroup() %>%
      filter(..rank == 1) %>%
      select(-..rank)
  } else {
    ret <- data_sorted %>%
      distinct(!!sym(user_col), .keep_all = T)
  }

  ret
}
