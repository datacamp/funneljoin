filter_in_gap <- function(pairs, min_gap = NULL, max_gap = NULL, time_xy, user_xy) {
  if (inherits(pairs, "tbl_BigQueryConnection")) {
    time_difference <- dplyr::sql(glue::glue('DATETIME_DIFF(`{ time_xy$y }`,
                                               `{ time_xy$x }`,
                                               SECOND)'))

    ret <- pairs %>%
      dplyr::mutate(..time_diff = time_difference)
  }
  else if (inherits(pairs, "tbl_lazy")) {
    time_difference <- dplyr::sql(glue::glue('DATEDIFF("seconds",
                                             "{ time_xy$x }",
                                             "{ time_xy$y }")::integer'))
    ret <- pairs %>%
      dplyr::mutate(..time_diff = time_difference)
  } else {
    ret <- pairs %>%
      dplyr::mutate(..time_diff = difftime(!!dplyr::sym(time_xy$y),
                                           !!dplyr::sym(time_xy$x),
                                           units = "secs"))
  }

  if (!is.null(max_gap)) {
    ret <- ret %>%
      dplyr::filter(..time_diff < !!as_seconds(max_gap))
  }

  if (!is.null(min_gap)) {
    ret <- ret %>%
      dplyr::filter(..time_diff > !!as_seconds(min_gap))
  }

  ret
}

