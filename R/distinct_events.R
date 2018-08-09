#' Distinct events
#'
#' @param .data a dataset, either local or remote
#' @param time_col the name of the time column
#' @param user_col the name of the user identifying column
#' @param type the type of after_join ("first-first", "first-firstafter", etc.)
#'
#' @return
#'
#' @examples
distinct_events <- function(.data, time_col, user_col, type) {
  if (inherits(.data, "tbl_lazy")) {
    desc <- if (type == "first") " DESC" else ""
    rank_sql <- sql(glue::glue('RANK() OVER (PARTITION BY "{ user_col }" ORDER BY "{ time_col }" { desc })'))

    ret <- .data %>%
      mutate(..rank = rank_sql) %>%
      filter(..rank == 1) %>%
      select(-..rank)
  } else {
    if (type == "first") {
      data_sorted <- .data %>%
        arrange(!!sym(time_col))
    } else if (type == "last") {
      data_sorted <- .data %>%
        arrange(desc(!!sym(time_col)))
    }

    ret <- data_sorted %>%
      distinct(!!sym(user_col), .keep_all = T)
  }
  ret
}
