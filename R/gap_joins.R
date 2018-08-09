filter_smallest_gap <- function(pairs, dt, time_xy, user_xy) {
  if (inherits(pairs, "tbl_lazy")) {

    time_difference <- dplyr::sql(glue::glue('DATEDIFF("seconds",
                                             "{ time_xy$x }",
                                             "{ time_xy$y }")::integer'))

    ret <- pairs %>%
      dplyr::mutate(..time_diff = time_difference) %>%
      distinct_events(user_col = user_xy$x,
                      time_col = "..time_diff",
                      type = "first")
  }
  else {

    ret <- pairs %>%
      dplyr::mutate(..time_diff = difftime(!!sym(time_xy$y), !!sym(time_xy$x), "secs")) %>%
      dplyr::group_by(!!sym(user_xy$x)) %>%
      dplyr::filter(..time_diff == min(..time_diff, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  ret
}

filter_within_gap <- function(pairs, dt, time_xy, user_xy) {
  if (inherits(pairs, "tbl_lazy")) {
    time_difference <- dplyr::sql(glue::glue('DATEDIFF("seconds",
                                             "{ time_xy$x }",
                                             "{ time_xy$y }")::integer'))

    ret <- pairs %>%
      dplyr::mutate(..time_diff = time_difference) %>%
      dplyr::filter(..time_diff < !!as_seconds(dt))

  }
  else {
    ret <- pairs %>%
      dplyr::mutate(..time_diff = difftime(!!sym(time_xy$y), !!sym(time_xy$x), "secs")) %>%
      dplyr::filter(..time_diff < !!as_seconds(dt))
  }
  ret
}
