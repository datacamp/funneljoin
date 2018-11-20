filter_within_gap <- function(pairs, max_gap, time_xy, user_xy) {
  if (inherits(pairs, "tbl_lazy")) {
    time_difference <- dplyr::sql(glue::glue('DATEDIFF("seconds",
                                             "{ time_xy$x }",
                                             "{ time_xy$y }")::integer'))

    ret <- pairs %>%
      dplyr::mutate(..time_diff = time_difference) %>%
      dplyr::filter(..time_diff < !!as_seconds(max_gap))

  }
  else {
    ret <- pairs %>%
      dplyr::mutate(..time_diff = difftime(!!dplyr::sym(time_xy$y), !!dplyr::sym(time_xy$x), "secs")) %>%
      dplyr::filter(..time_diff < !!as_seconds(max_gap))
  }
  ret
}

