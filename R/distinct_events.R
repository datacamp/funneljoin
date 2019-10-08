#' Distinct events
#'
#' @param .data a dataset, either local or remote
#' @param time_col the name of the time column
#' @param user_col the name of the user identifying column
#' @param type the type of after_join ("first-first", "first-firstafter", etc.)
#' @importFrom magrittr %>%
#' @export
distinct_events <- function(.data, time_col, user_col, type) {
  if (inherits(.data, "tbl_lazy")) {
    desc <- if (type == "last") " DESC" else ""
    rank_sql <- dplyr::sql(glue::glue('ROW_NUMBER() OVER (PARTITION BY "{ user_col }" ORDER BY "{ time_col }" { desc })'))

    ret <- .data %>%
      dplyr::mutate(..rank = rank_sql) %>%
      dplyr::filter(..rank == 1) %>%
      dplyr::select(-..rank)
  } else {
    if (type == "first") {
      data_sorted <- .data %>%
        dplyr::arrange(!!dplyr::sym(time_col))
    } else if (type == "last") {
      data_sorted <- .data %>%
        dplyr::arrange(desc(!!dplyr::sym(time_col)))
    }

    ret <- data_sorted %>%
      dplyr::distinct(!!dplyr::sym(user_col), .keep_all = T)
  }
  ret
}
