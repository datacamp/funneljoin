#' Join tables to get the count of events following another
#'
#' @param x A tbl representing the first event to occur in the funnel.
#' @param y A tbl representing an event to occur in the funnel.
#' @param by_time A character vector to specify the time columns in x and y.
#'   This would typically be a datetime or a date column. These columns are used to
#'   filter for time y being after time x.
#' @param by_user A character vector to specify the user or identity columns in
#'   x and y.
#' @param mode The method used to join: "inner", "full", "anti", "semi",
#'   "right", "left". Each also has its own function, such as
#'   \code{after_inner_join}.
#' @param type The type of event you want from x - can be "first", "last", or "any."
#' @param max_gap Optional: the maximum gap allowed between events. Can be a
#'   integer representing the number of seconds or a difftime object, such as
#'   \code{as.difftime(2, units = "hours")}.
#' @param min_gap Optional: the maximum gap allowed between events. Can be a
#'   integer representing the number of seconds or a difftime object, such as
#'   \code{as.difftime(2, units = "hours")}.
#' @param gap_col Whether to include a numeric column, \code{.gap},
#'   with the time difference in seconds between the events.
#' @param suffix If there are non-joined duplicate variables in x and y,
#' these suffixes will be added to the output to disambiguate them.
#' Should be a character vector of length 2.
#' @importFrom magrittr %>%
#'
#' @return a tbl with the user column, timestamp from the first table, and number of events from the second tbl following that timestamp
#' @export
#'
after_count <- function(x,
                        y,
                        by_time,
                        by_user,
                        mode = "inner",
                        type = "first",
                        max_gap = NULL,
                        min_gap = NULL,
                        gap_col = FALSE,
                        suffix = c(".x", ".y")) {

  user_xy <- dplyr::common_by(by_user, x, y)
  time_xy <- dplyr::common_by(by_time, x, y)
  new_type <- paste0(type, "-any")

  after_join_result <- after_join(x = x,
                                  y = y,
                                  by_time = by_time,
                                  by_user = by_user,
                                  mode = mode,
                                  type = new_type,
                                  min_gap = min_gap,
                                  max_gap = max_gap,
                                  gap_col = gap_col,
                                  suffix = suffix)

  if (time_xy$x == time_xy$y) {
    time_x <- paste0(time_xy$x, suffix[1])
    time_y <- paste0(time_xy$y, suffix[2])
    after_join_result %>%
      dplyr::group_by(!!dplyr::sym((user_xy$x)),
                      !!dplyr::sym((time_x))) %>%
      dplyr::summarize(nb_events = sum(!is.na(!!dplyr::sym((time_y)))))
  }
  else {
    after_join_result %>%
      dplyr::group_by(!!dplyr::sym((user_xy$x)),
                      !!dplyr::sym((time_xy$x))) %>%
      dplyr::summarize(nb_events = sum(!is.na(!!dplyr::sym((time_xy$y)))))
  }
}

#' @rdname after_count
#' @export
after_inner_count <- function(x, y, by_time, by_user, type, max_gap = NULL,  min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_count(x, y, by_time, by_user,
             mode = "inner", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_count
#' @export
after_left_count <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_count(x, y, by_time, by_user,
             mode = "left", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_count
#' @export
after_right_count <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_count(x, y, by_time, by_user,
             mode = "right", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_count
#' @export
after_full_count <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_count(x, y, by_time, by_user,
             mode = "full", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}
